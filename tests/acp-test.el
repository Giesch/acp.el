;;; acp-test.el --- Tests for acp.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for acp.el.
;;
;; Log buffer trimming enforces byte limits while preserving whole log
;; messages using boundary markers.
;;
;; Request and response builders are tested through their serialized JSON,
;; since the agent reads the wire format rather than the alist.

;;; Code:

(require 'ert)
(setq load-prefer-newer t)
(require 'acp)
(require 'acp-fakes)

(defun acp-test--format-log-message (message)
  "Return a formatted log message for MESSAGE."
  (acp--format-log-message (car message) "%s" (cdr message)))

(defun acp-test-log-buffer-string (max-bytes &rest messages)
  "Log MESSAGES with MAX-BYTES and return the log buffer contents."
  (let* ((acp-logging-enabled t)
         (acp--log-buffer-max-bytes max-bytes)
         (client (list (cons :command (make-temp-name "acp-test-"))
                       (cons :instance-count 1)))
         (log-buffer (acp-logs-buffer :client client)))
    (unwind-protect
        (with-current-buffer log-buffer
          (erase-buffer)
          (dolist (message messages)
            (acp--log client (car message) "%s" (cdr message)))
          (buffer-string))
      (when (buffer-live-p log-buffer)
        (kill-buffer log-buffer)))))

(ert-deftest acp-test-trim-log-buffer-unibyte ()
  "Trim unibyte logs on whole-message boundaries."
  (let* ((msg1 (cons "A" "one"))
         (msg2 (cons "B" "two"))
         (msg3 (cons "C" "three"))
         (log1 (acp-test--format-log-message msg1))
         (log2 (acp-test--format-log-message msg2))
         (log3 (acp-test--format-log-message msg3))
         (max-bytes (+ (string-bytes log2) (string-bytes log3)))
         (messages (list msg1 msg2 msg3))
         (result (apply #'acp-test-log-buffer-string max-bytes messages)))
    (should (equal result (concat log2 log3)))
    (should (<= (string-bytes result) max-bytes))))

(ert-deftest acp-test-trim-log-buffer-multibyte ()
  "Trim multibyte logs by bytes while keeping whole messages."
  (let* ((msg1 (cons "A" "alpha"))
         (msg2 (cons "B" "café ✓"))
         (msg3 (cons "C" "omega"))
         (log1 (acp-test--format-log-message msg1))
         (log2 (acp-test--format-log-message msg2))
         (log3 (acp-test--format-log-message msg3))
         (chars-m2m3 (+ (length log2) (length log3)))
         (bytes-m2m3 (+ (string-bytes log2) (string-bytes log3)))
         (max-bytes (1+ chars-m2m3))
         (messages (list msg1 msg2 msg3))
         (result (apply #'acp-test-log-buffer-string max-bytes messages)))
    (should (< max-bytes bytes-m2m3))
    (should (equal result log3))
    (should (<= (string-bytes result) max-bytes))))

(ert-deftest acp-test-sync-request-fails-when-agent-exits-after-read ()
  "Synchronous requests error instead of waiting forever after agent exit."
  (let ((client (acp-make-client
                 :command "sh"
                 :command-params '("-c" "IFS= read -r _; exit 42"))))
    (unwind-protect
        (should-error
         (acp-send-request
          :client client
          :request '((:method . "initialize"))
          :sync t))
      (when-let ((process (map-elt client :process))
                 ((process-live-p process)))
        (delete-process process)))))

(defun acp-test--initialize-capabilities (&rest args)
  "Return `clientCapabilities' of an initialize request built with ARGS."
  (map-nested-elt (apply #'acp-make-initialize-request :protocol-version 1 args)
                  '(:params clientCapabilities)))

(ert-deftest acp-test-initialize-request-omits-elicitation-by-default ()
  "Omit the elicitation capability unless it is asked for."
  (let ((capabilities (acp-test--initialize-capabilities)))
    (should (map-contains-key capabilities 'fs))
    (should-not (map-contains-key capabilities 'elicitation))
    (should-not (string-search "elicitation" (json-serialize capabilities)))))

(ert-deftest acp-test-initialize-request-elicitation-form-is-an-object ()
  "Serialize the form capability as an object.
Agents gate on the value being an object.  A boolean fails their
schema check and disables the capability."
  (let ((capabilities (acp-test--initialize-capabilities
                       :elicitation-form-capability t)))
    (should (map-contains-key (map-elt capabilities 'elicitation) 'form))
    (should (string-search "\"elicitation\":{\"form\":{}}"
                           (json-serialize capabilities)))))

(ert-deftest acp-test-elicitation-create-response-accept ()
  "Serialize an accepted form, including a multi-select answer."
  (let ((response (acp-make-elicitation-create-response
                   :request-id 7
                   :action "accept"
                   :content '((question_0 . "JWT")
                              (question_1 . ["Tests" "Docs"])))))
    (should (equal (map-elt response :request-id) 7))
    (should (equal (json-serialize (map-elt response :result))
                   (concat "{\"action\":\"accept\","
                           "\"content\":{\"question_0\":\"JWT\","
                           "\"question_1\":[\"Tests\",\"Docs\"]}}")))))

(ert-deftest acp-test-elicitation-create-response-without-content ()
  "Omit the content key when there is no content."
  (dolist (action '("accept" "decline" "cancel"))
    (let ((response (acp-make-elicitation-create-response
                     :request-id 1 :action action)))
      (should (equal (json-serialize (map-elt response :result))
                     (format "{\"action\":\"%s\"}" action))))))

(ert-deftest acp-test-elicitation-create-response-error ()
  "Build an error response instead of a result."
  (let ((response (acp-make-elicitation-create-response
                   :request-id 1
                   :error (acp-make-error :code -32603 :message "Boom"))))
    (should (map-contains-key response :error))
    (should-not (map-contains-key response :result))))

(ert-deftest acp-test-elicitation-create-response-rejects-invalid-arguments ()
  "Reject argument combinations the agent would silently discard."
  (should-error (acp-make-elicitation-create-response :action "accept"))
  (should-error (acp-make-elicitation-create-response :request-id 1))
  (should-error (acp-make-elicitation-create-response
                 :request-id 1 :action "accept"
                 :error '((code . -32603) (message . "Boom"))))
  (should-error (acp-make-elicitation-create-response
                 :request-id 1 :action "dismiss"))
  (should-error (acp-make-elicitation-create-response
                 :request-id 1 :action "decline"
                 :content '((question_0 . "JWT"))))
  ;; A list serializes as a JSON object, which fails the agent's schema
  ;; check and drops every answer without an error.
  (should-error (acp-make-elicitation-create-response
                 :request-id 1 :action "accept"
                 :content '((question_0 "Tests" "Docs")))))

(ert-deftest acp-test-fake-client-records-sent-responses ()
  "Record responses sent through a fake client."
  (let* ((client (acp-fakes-make-client nil))
         (declined (acp-make-elicitation-create-response
                    :request-id 1 :action "decline"))
         (cancelled (acp-make-elicitation-create-response
                     :request-id 2 :action "cancel")))
    (should-not (acp-fakes-sent-responses client))
    (acp-send-response :client client :response declined)
    (acp-send-response :client client :response cancelled)
    (should (equal (acp-fakes-sent-responses client)
                   (list declined cancelled)))))

(ert-deftest acp-test-fake-client-records-sent-requests ()
  "Record requests and notifications sent through a fake client."
  (let* ((client (acp-fakes-make-client nil))
         (prompt (acp-make-session-prompt-request
                  :session-id "s1"
                  :prompt [("type" . "text") ("text" . "hello")]))
         (cancel (acp-make-session-cancel-notification
                  :session-id "s1" :reason "done")))
    (should-not (acp-fakes-sent-requests client))
    (unwind-protect
        (progn
          (acp-send-request :client client :request prompt)
          (acp-send-notification :client client :notification cancel)
          (should (equal (acp-fakes-sent-requests client) (list prompt)))
          (should (equal (acp-fakes-sent-notifications client) (list cancel))))
      (acp-shutdown :client client))))

(defun acp-test--exited-client ()
  "Return a started client whose process has since exited."
  (let* ((client (acp-make-client :command "cat"))
         (process (progn (acp--start-client :client client)
                         (map-elt client :process))))
    (delete-process process)
    (while (process-live-p process)
      (accept-process-output nil 0.05))
    client))

(ert-deftest acp-test-shutdown-releases-client-whose-process-exited ()
  "Release handlers and buffers even when the process already exited."
  (let ((client (acp-test--exited-client)))
    (acp-subscribe-to-notifications
     :client client :on-notification (lambda (_notification) nil))
    (should (map-elt client :notification-handlers))
    (let ((logs (acp-logs-buffer :client client))
          (traffic (acp-traffic-buffer :client client)))
      (acp-shutdown :client client)
      (should-not (map-elt client :notification-handlers))
      (should-not (buffer-live-p logs))
      (should-not (buffer-live-p traffic)))))

(ert-deftest acp-test-shutdown-releases-running-client ()
  "Release handlers and buffers for a client with a live process."
  (let ((client (acp-make-client :command "cat")))
    (acp--start-client :client client)
    (acp-subscribe-to-notifications
     :client client :on-notification (lambda (_notification) nil))
    (let ((logs (acp-logs-buffer :client client))
          (traffic (acp-traffic-buffer :client client)))
      (acp-shutdown :client client)
      (should-not (map-elt client :notification-handlers))
      (should-not (buffer-live-p logs))
      (should-not (buffer-live-p traffic)))))

(ert-deftest acp-test-shutdown-is-idempotent ()
  "Leave no buffers behind when shutdown is called more than once."
  (let ((client (acp-make-client :command "cat")))
    (acp--start-client :client client)
    (acp-logs-buffer :client client)
    (acp-traffic-buffer :client client)
    (acp-shutdown :client client)
    (acp-shutdown :client client)
    (should-not (get-buffer (acp--logs-buffer-name client)))
    (should-not (get-buffer (acp--traffic-buffer-name client)))))

(ert-deftest acp-test-shutdown-does-not-create-buffers ()
  "Do not resurrect buffers that were never opened."
  (let ((client (acp-make-client :command "cat")))
    (acp--start-client :client client)
    (should-not (get-buffer (acp--logs-buffer-name client)))
    (acp-shutdown :client client)
    (should-not (get-buffer (acp--logs-buffer-name client)))
    (should-not (get-buffer (acp--traffic-buffer-name client)))))

(ert-deftest acp-test-shutdown-tolerates-externally-killed-buffer ()
  "Release the traffic buffer when the log buffer is already gone."
  (let ((client (acp-make-client :command "cat")))
    (acp--start-client :client client)
    (kill-buffer (acp-logs-buffer :client client))
    (let ((traffic (acp-traffic-buffer :client client)))
      (acp-shutdown :client client)
      (should-not (buffer-live-p traffic)))))

(ert-deftest acp-test-shutdown-releases-never-started-client ()
  "Release handlers on a client that was never started."
  (let ((client (acp-make-client :command "cat")))
    (acp-subscribe-to-notifications
     :client client :on-notification (lambda (_notification) nil))
    (acp-shutdown :client client)
    (should-not (map-elt client :notification-handlers))))

(ert-deftest acp-test-shutdown-after-restart-releases-again ()
  "Allow a restarted client to be shut down a second time."
  (let ((client (acp-make-client :command "cat")))
    (acp--start-client :client client)
    (acp-shutdown :client client)
    (acp--start-client :client client)
    (acp-subscribe-to-notifications
     :client client :on-notification (lambda (_notification) nil))
    (let ((logs (acp-logs-buffer :client client))
          (traffic (acp-traffic-buffer :client client)))
      (acp-shutdown :client client)
      (should-not (map-elt client :notification-handlers))
      (should-not (buffer-live-p logs))
      (should-not (buffer-live-p traffic)))))

(provide 'acp-test)

;;; acp-test.el ends here
