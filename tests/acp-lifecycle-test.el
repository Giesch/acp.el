;;; acp-lifecycle-test.el --- Connection lifecycle regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'acp)
(require 'acp-traffic)

;; The test runner loads acp-test.el before running ERT.  Its captured-filter
;; and notification-line helpers are therefore available when these tests run.

(defmacro acp-lifecycle-test--with-resources (&rest body)
  "Run BODY with captured timers and cleanup of owned processes and buffers."
  (declare (indent 0) (debug t))
  `(let* ((acp-logging-enabled nil)
          (client (acp-make-client :command "cat"))
          (real-make-process (symbol-function 'make-process))
          owned-processes owned-buffers scheduled)
     (cl-letf (((symbol-function 'make-process)
                (lambda (&rest args)
                  (push (plist-get args :stderr) owned-buffers)
                  (let ((process (apply real-make-process args)))
                    (push process owned-processes)
                    process)))
               ((symbol-function 'run-at-time)
                (lambda (time repeat function &rest args)
                  ;; Emacs can schedule undo housekeeping during buffer cleanup.
                  ;; It is not an ACP callback and must not affect these assertions.
                  (unless (eq function 'undo-auto--boundary-timer)
                    (push (list time repeat function args) scheduled))
                  nil)))
       (unwind-protect
           (progn ,@body)
         ;; Do not signal a process group or leave shutdown timers behind.
         (dolist (process owned-processes)
           (when (processp process)
             (set-process-sentinel process #'ignore)
             (ignore-errors (delete-process process))))
         (ignore-errors (acp-shutdown :client client))
         (dolist (buffer owned-buffers)
           (when (buffer-live-p buffer)
             (kill-buffer buffer)))))))

(defun acp-lifecycle-test--run-timer (timer)
  "Invoke the callback in captured TIMER."
  (apply (nth 2 timer) (nth 3 timer)))

(defmacro acp-lifecycle-test--stub-filter (client)
  "Capture CLIENT's filter, tracking the stderr buffer for cleanup."
  `(cl-letf (((symbol-function 'generate-new-buffer)
              (let ((original (symbol-function 'generate-new-buffer)))
                (lambda (&rest args)
                  (let ((buffer (apply original args)))
                    (push buffer owned-buffers)
                    buffer)))))
     (acp-test--captured-filter ,client)))

(ert-deftest acp-test-initialize-elicitation-capability-matrix ()
  "Advertise each requested mode as an empty JSON object, never a boolean."
  (dolist (case '((nil nil ()) (t nil ("form"))
                  (nil t ("url")) (t t ("form" "url"))))
    (let* ((request (acp-make-initialize-request
                     :protocol-version 1
                     :elicitation-form-capability (nth 0 case)
                     :elicitation-url-capability (nth 1 case)))
           (wire (json-serialize (map-elt request :params)))
           (parsed (json-parse-string wire :object-type 'hash-table))
           (capabilities (gethash "clientCapabilities" parsed))
           (missing (make-symbol "missing"))
           (elicitation (gethash "elicitation" capabilities missing))
           (modes (nth 2 case)))
      (if (null modes)
          (should (eq elicitation missing))
        (should (hash-table-p elicitation))
        (should (= (hash-table-count elicitation) (length modes))))
      (dolist (mode '("form" "url"))
        (if (member mode modes)
            (let ((value (gethash mode elicitation missing)))
              (should (hash-table-p value))
              (should (= (hash-table-count value) 0))
              (should (string-match-p (format "\"%s\":{}" mode) wire)))
          (when modes
            (should (eq (gethash mode elicitation missing) missing))))))))

(ert-deftest acp-test-drain-stops-after-shutdown ()
  "A handler closing the client prevents routing or rescheduling its tail."
  (acp-lifecycle-test--with-resources
    (let ((acp-drain-time-budget nil) received)
      (acp-subscribe-to-notifications
       :client client :on-notification
       (lambda (notification)
         (push (map-nested-elt notification '(params n)) received)
         (acp-shutdown :client client)))
      (let ((filter (acp-lifecycle-test--stub-filter client)))
        (funcall filter nil
                 (mapconcat #'acp-test--notification-line '(1 2 3) ""))
        (should (= (length scheduled) 1))
        ;; Inspect routing, not just handlers cleared by shutdown.
        (let ((route (symbol-function 'acp--route-incoming-message))
              (routes 0)
              (timer (pop scheduled)))
          (cl-letf (((symbol-function 'acp--route-incoming-message)
                     (lambda (&rest args)
                       (cl-incf routes)
                       (apply route args))))
            (acp-lifecycle-test--run-timer timer))
          (should (= routes 1))
          (should (equal received '(1)))
          (should-not scheduled))))))

(ert-deftest acp-test-old-drain-does-not-route-after-restart ()
  "An old queued callback cannot deliver messages into a new connection."
  (acp-lifecycle-test--with-resources
    (let ((old-filter (acp-lifecycle-test--stub-filter client)) received)
      (funcall old-filter nil
               (mapconcat #'acp-test--notification-line '(1 2) ""))
      (let ((old-timer (pop scheduled)))
        (acp-shutdown :client client)
        (let ((new-filter (acp-lifecycle-test--stub-filter client)))
          (acp-subscribe-to-notifications
           :client client :on-notification
           (lambda (notification)
             (push (map-nested-elt notification '(params n)) received)))
          (cl-letf (((symbol-function 'acp--route-incoming-message)
                     (lambda (&rest _) (ert-fail "Old drain routed a message"))))
            (acp-lifecycle-test--run-timer old-timer))
          (should-not scheduled)
          (should-not received)
          (funcall new-filter nil (acp-test--notification-line 3))
          (acp-lifecycle-test--run-timer (pop scheduled))
          (should (equal received '(3)))
          (should-not scheduled))))))

(ert-deftest acp-test-old-sentinel-does-not-close-restarted-client ()
  "A retiring process cannot close its replacement or steal its stderr."
  (acp-lifecycle-test--with-resources
    (map-put! client :command "sh")
    (map-put! client :command-params
              '("-c" "while IFS= read -r line; do printf '%s\\n' \"$line\" >&2; done"))
    (acp--start-client :client client)
    (let* ((old-process (map-elt client :process))
           (old-sentinel (process-sentinel old-process))
           (old-stderr (car owned-buffers))
           errors (disconnects 0))
      ;; Retain a real, live retiring process across the restart.
      (cl-letf (((symbol-function 'signal-process) (lambda (&rest _) nil)))
        (acp-shutdown :client client))
      (should (process-live-p old-process))
      (acp--start-client :client client)
      (let* ((replacement (map-elt client :process))
             (new-stderr (car owned-buffers))
             (real-status (symbol-function 'process-status)))
        (should-not (eq old-process replacement))
        (should-not (eq old-stderr new-stderr))
        (should (buffer-live-p old-stderr))
        (should (buffer-live-p new-stderr))
        (acp-subscribe-to-disconnect
         :client client :on-disconnect (lambda () (cl-incf disconnects)))
        (acp-subscribe-to-errors
         :client client :on-error (lambda (error) (push error errors)))
        (cl-letf (((symbol-function 'process-status)
                   (lambda (process)
                     (if (eq process old-process) 'exit
                       (funcall real-status process)))))
          (funcall old-sentinel old-process "finished\n"))
        (should-not (map-elt client :closed))
        (should (= disconnects 0))
        (should (eq replacement (map-elt client :process)))
        (should (process-live-p replacement))
        (should-not (buffer-live-p old-stderr))
        (should (buffer-live-p new-stderr))
        ;; Use the pipe boundary, not manual insertion into the stderr buffer.
        (process-send-string replacement "replacement-stderr\n")
        (let ((deadline (+ (float-time) 2)))
          (while (and (null errors) (< (float-time) deadline))
            (accept-process-output nil 0.02)))
        (should (= (length errors) 1))
        (should (equal (map-elt (car errors) 'message)
                       "replacement-stderr\n"))
        (should (= disconnects 0))))))

(ert-deftest acp-test-shutdown-fallback-does-not-kill-restarted-client ()
  "The delayed force-kill targets the retired process, not its replacement."
  (acp-lifecycle-test--with-resources
    (acp--start-client :client client)
    (let ((old-process (map-elt client :process)))
      (cl-letf (((symbol-function 'signal-process) (lambda (&rest _) nil)))
        (acp-shutdown :client client))
      (should (process-live-p old-process))
      (should (= (length scheduled) 1))
      (let ((fallback (pop scheduled)))
        (should (equal (car fallback) acp--shutdown-grace-period))
        (acp--start-client :client client)
        (let ((replacement (map-elt client :process)))
          (should-not (eq old-process replacement))
          ;; Avoid group signals in a test; delete-process still acts on the
          ;; actual process captured by the production fallback closure.
          (let (signals)
            (cl-letf (((symbol-function 'signal-process)
                       (lambda (target signal)
                         (push (list target signal) signals)
                         nil)))
              (acp-lifecycle-test--run-timer fallback))
            (should (equal signals
                           (list (list (- (process-id old-process)) 'SIGKILL)))))
          (should-not (process-live-p old-process))
          (should (process-live-p replacement))
          (should (eq replacement (map-elt client :process)))
          (should-not (map-elt client :closed))
          (should-not scheduled))))))

(ert-deftest acp-test-terminal-sentinel-disconnect-subscriber-restarts-client ()
  "A terminal sentinel finishes old requests without touching a callback restart."
  (acp-lifecycle-test--with-resources
    (acp--start-client :client client)
    (let* ((old-process (map-elt client :process))
           (old-sentinel (process-sentinel old-process))
           (old-generation (map-elt client :connection-generation))
           (old-failures 0) (new-failures 0) (disconnects 0)
           closed-in-callback generation-in-callback replacement new-pending
           new-handlers)
      (map-put! client :pending-requests
                `((101 . ((:on-failure . ,(lambda (_error)
                                           (cl-incf old-failures)))))))
      (acp-subscribe-to-disconnect
       :client client :on-disconnect
       (lambda ()
         (cl-incf disconnects)
         (setq closed-in-callback (map-elt client :closed)
               generation-in-callback (map-elt client :connection-generation))
         (acp--start-client :client client)
         (setq replacement (map-elt client :process)
               new-pending
               `((102 . ((:on-failure . ,(lambda (_error)
                                          (cl-incf new-failures)))))))
         (map-put! client :pending-requests new-pending)
         (acp-subscribe-to-notifications :client client :on-notification #'ignore)
         (acp-subscribe-to-requests :client client :on-request #'ignore)
         (acp-subscribe-to-errors :client client :on-error #'ignore)
         (acp-subscribe-to-disconnect :client client :on-disconnect #'ignore)
         (setq new-handlers
               (mapcar (lambda (key) (cons key (map-elt client key)))
                       '(:notification-handlers :request-handlers
                         :error-handlers :disconnect-handlers)))))
      ;; Make the process terminal without letting Emacs deliver the sentinel.
      (set-process-sentinel old-process #'ignore)
      (delete-process old-process)
      (funcall old-sentinel old-process "finished\n")
      (should (= disconnects 1))
      (should closed-in-callback)
      (should (eq generation-in-callback old-generation))
      (should-not (eq old-generation (map-elt client :connection-generation)))
      (should (= old-failures 1))
      (should (= new-failures 0))
      (should (eq new-pending (map-elt client :pending-requests)))
      (should-not (eq old-process replacement))
      (should (eq replacement (map-elt client :process)))
      (should (process-live-p replacement))
      (should-not (map-elt client :closed))
      (dolist (entry new-handlers)
        (should (cdr entry))
        (should (eq (cdr entry) (map-elt client (car entry)))))
      ;; Re-delivery of the old sentinel must remain harmless.
      (funcall old-sentinel old-process "finished\n")
      (should (= disconnects 1))
      (should (= old-failures 1))
      (should (= new-failures 0))
      (should (eq new-pending (map-elt client :pending-requests)))
      (should (process-live-p replacement))
      (should-not (map-elt client :closed)))))

(ert-deftest acp-test-shutdown-disconnect-subscriber-restarts-client ()
  "Shutdown releases only old resources when a subscriber restarts CLIENT."
  (acp-lifecycle-test--with-resources
    (acp--start-client :client client)
    (let* ((old-process (map-elt client :process))
           (old-pid (process-id old-process))
           (old-generation (map-elt client :connection-generation))
           (old-log (acp-logs-buffer :client client))
           (old-traffic (acp-traffic-buffer :client client))
           (old-failures 0) (new-failures 0) (disconnects 0)
           closed-in-callback generation-in-callback replacement new-pending
           new-log new-traffic new-handlers signals)
      (push old-log owned-buffers)
      (push old-traffic owned-buffers)
      (map-put! client :pending-requests
                `((201 . ((:on-failure . ,(lambda (_error)
                                           (cl-incf old-failures)))))))
      (acp-subscribe-to-disconnect
       :client client :on-disconnect
       (lambda ()
         (cl-incf disconnects)
         (setq closed-in-callback (map-elt client :closed)
               generation-in-callback (map-elt client :connection-generation))
         (acp--start-client :client client)
         (setq replacement (map-elt client :process)
               new-pending
               `((202 . ((:on-failure . ,(lambda (_error)
                                          (cl-incf new-failures)))))))
         (map-put! client :pending-requests new-pending)
         (acp-subscribe-to-notifications :client client :on-notification #'ignore)
         (acp-subscribe-to-requests :client client :on-request #'ignore)
         (acp-subscribe-to-errors :client client :on-error #'ignore)
         (acp-subscribe-to-disconnect :client client :on-disconnect #'ignore)
         (setq new-handlers
               (mapcar (lambda (key) (cons key (map-elt client key)))
                       '(:notification-handlers :request-handlers
                         :error-handlers :disconnect-handlers))
               new-log (acp-logs-buffer :client client)
               new-traffic (acp-traffic-buffer :client client))
         (push new-log owned-buffers)
         (push new-traffic owned-buffers)))
      (cl-letf (((symbol-function 'signal-process)
                 (lambda (target signal)
                   (push (list target signal) signals))))
        (acp-shutdown :client client)
        (should (= disconnects 1))
        (should closed-in-callback)
        (should (eq generation-in-callback old-generation))
        (should-not (eq old-generation (map-elt client :connection-generation)))
        (should (= old-failures 1))
        (should (= new-failures 0))
        (should (eq new-pending (map-elt client :pending-requests)))
        (should (eq replacement (map-elt client :process)))
        (should-not (eq old-process replacement))
        (should (process-live-p old-process))
        (should (process-live-p replacement))
        (should-not (map-elt client :closed))
        (should-not (buffer-live-p old-log))
        (should-not (buffer-live-p old-traffic))
        (should-not (eq old-log new-log))
        (should-not (eq old-traffic new-traffic))
        (should (buffer-live-p new-log))
        (should (buffer-live-p new-traffic))
        (should (eq new-log (acp-logs-buffer :client client)))
        (should (eq new-traffic (acp-traffic-buffer :client client)))
        (dolist (entry new-handlers)
          (should (cdr entry))
          (should (eq (cdr entry) (map-elt client (car entry)))))
        (should (equal signals (list (list (- old-pid) 'SIGTERM))))
        (should (= (length scheduled) 1))
        (let ((fallback (pop scheduled)))
          (should (equal (car fallback) acp--shutdown-grace-period))
          (acp-lifecycle-test--run-timer fallback))
        (should (equal (reverse signals)
                       (list (list (- old-pid) 'SIGTERM)
                             (list (- old-pid) 'SIGKILL))))
        (should-not (process-live-p old-process))
        (should (process-live-p replacement))
        (should (eq replacement (map-elt client :process)))
        (should-not (map-elt client :closed))
        (should (= old-failures 1))
        (should (= new-failures 0))
        (should (eq new-pending (map-elt client :pending-requests)))
        (should (buffer-live-p new-log))
        (should (buffer-live-p new-traffic))
        (dolist (entry new-handlers)
          (should (eq (cdr entry) (map-elt client (car entry)))))
        (should-not scheduled)))))

(ert-deftest acp-test-remote-shutdown-signals-only-retired-process ()
  "Remote shutdown signals the old process directly, never its process group."
  (acp-lifecycle-test--with-resources
    (acp--start-client :client client)
    (let* ((old-process (map-elt client :process))
           (real-process-get (symbol-function 'process-get))
           (real-delete-process (symbol-function 'delete-process))
           signals deleted)
      (cl-letf (((symbol-function 'process-get)
                 (lambda (process property)
                   (if (and (eq process old-process) (eq property 'remote-pid))
                       12345
                     (funcall real-process-get process property))))
                ((symbol-function 'signal-process)
                 (lambda (target signal) (push (list target signal) signals)))
                ((symbol-function 'delete-process)
                 (lambda (process)
                   (push process deleted)
                   (funcall real-delete-process process))))
        (acp-shutdown :client client)
        (should (map-elt client :closed))
        (should (equal signals (list (list old-process 'SIGTERM))))
        (should (process-live-p old-process))
        (should (= (length scheduled) 1))
        (acp--start-client :client client)
        (let ((replacement (map-elt client :process)))
          (acp-lifecycle-test--run-timer (pop scheduled))
          (should (equal signals (list (list old-process 'SIGTERM))))
          (should (equal deleted (list old-process)))
          (should-not (process-live-p old-process))
          (should (process-live-p replacement))
          (should (eq replacement (map-elt client :process)))
          (should-not (map-elt client :closed))
          (should-not scheduled))))))

(ert-deftest acp-test-disconnect-subscriber-error-isolation-and-idempotence ()
  "Disconnect is idempotent and an erroring handler cannot block others."
  (acp-lifecycle-test--with-resources
    (let ((healthy 0) (failing 0))
      (acp-subscribe-to-disconnect
       :client client :on-disconnect (lambda () (cl-incf healthy)))
      ;; Subscriptions prepend, so this error occurs before the healthy one.
      (acp-subscribe-to-disconnect
       :client client :on-disconnect
       (lambda () (cl-incf failing) (error "Disconnect subscriber failed")))
      (acp--notify-disconnected client)
      (should (= failing 1))
      (should (= healthy 1))
      (should (map-elt client :closed))
      (acp--notify-disconnected client)
      (acp-shutdown :client client)
      (acp-shutdown :client client)
      (should (= failing 1))
      (should (= healthy 1)))))

(ert-deftest acp-test-traffic-save-returns-destination ()
  "Return the selected filename and write readable traffic objects there."
  (let ((destination (make-temp-file "acp-lifecycle-" nil ".traffic"))
        (objects '((:direction . "outgoing") (:kind . "request"))))
    (unwind-protect
        (with-temp-buffer
          (acp-traffic-mode)
          (cl-letf (((symbol-function 'read-file-name)
                     (lambda (&rest _) destination))
                    ((symbol-function 'acp-traffic--objects)
                     (lambda () objects)))
            (should (equal (acp-traffic-save-to) destination)))
          (should (file-exists-p destination))
          (should (equal (acp-traffic-read-file destination) objects)))
      (delete-file destination))))

(provide 'acp-lifecycle-test)
;;; acp-lifecycle-test.el ends here
