;;; acp-elicitation-test.el --- Elicitation wire tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests JSON encoding and independent logging projections.

;;; Code:
(require 'ert)
(require 'acp)

(ert-deftest acp-test-elicitation-values-and-empty-accept ()
  "Preserve JSON scalar and array shapes, and the SDK's empty accept contract."
  (dolist (entry '((false . :false) (zero . 0) (text . "") (array . [])))
    (let* ((response (acp-make-elicitation-create-response :request-id "0" :action "accept" :content (list entry)))
           (parsed (json-parse-string (acp--serialize-json (map-elt response :result))
                                      :object-type 'alist :false-object :false)))
      (should (equal (cdr entry) (map-nested-elt parsed (list 'content (car entry)))))))
  (should (equal "{\"action\":\"accept\"}\n"
                 (acp--serialize-json (map-elt (acp-make-elicitation-create-response :request-id 0 :action "accept") :result)))))

(ert-deftest acp-test-elicitation-log-projection ()
  "Traffic object and JSON views use redacted data; the wire stays original."
  (let ((client (acp-make-client :command "fake"))
        (acp-logging-enabled t) sent)
    (unwind-protect
        (let ((response (acp-make-elicitation-create-response :request-id 9 :action "accept"
                                                              :content '((secret . "SYNTHETIC_ACP_SECRET")))))
          (setf (map-elt response :log-result) '((action . "accept") (content . ((secret . "[redacted]")))))
          (cl-letf (((symbol-function 'process-send-string) (lambda (_process json) (setq sent json))))
            (acp-send-response :client client :response response))
          (should (string-search "SYNTHETIC_ACP_SECRET" sent))
          (should (equal "SYNTHETIC_ACP_SECRET" (map-nested-elt response '(:result content secret))))
          (with-current-buffer (acp-traffic-buffer :client client)
            (should-not (string-search "SYNTHETIC_ACP_SECRET" (format "%S" (acp-traffic--objects))))
            (should (string-search "[redacted]" (format "%S" (acp-traffic--objects))))))
      (kill-buffer (acp-traffic-buffer :client client)))))

(ert-deftest acp-test-elicitation-invalid-value-safe-error ()
  "Invalid content errors never format answer values."
  (condition-case err
      (progn (acp-make-elicitation-create-response :request-id 1 :action "accept"
                                                   :content '((secret . ("SYNTHETIC_ACP_SECRET"))))
             (ert-fail "Expected invalid value"))
    (error (should-not (string-search "SYNTHETIC_ACP_SECRET" (error-message-string err))))))

(ert-deftest acp-test-elicitation-parsing-preserves-booleans ()
  "Form schemas distinguish false and null without changing legacy messages."
  (let ((form (acp--parse-json
               "{\"id\":0,\"method\":\"elicitation/create\",\"params\":{\"false\":false,\"null\":null}}")))
    (should (eq :false (map-nested-elt form '(params false))))
    (should (eq :null (map-nested-elt form '(params null)))))
  (should-not (map-elt (acp--parse-json "{\"enabled\":false}") 'enabled)))

(ert-deftest acp-test-disconnect-notifies-once ()
  "Disconnect subscribers are isolated and shutdown remains idempotent."
  (let ((client (acp-make-client :command "fake")) (count 0))
    (acp-subscribe-to-disconnect :client client :on-disconnect (lambda () (setq count (1+ count))))
    (acp-subscribe-to-disconnect :client client :on-disconnect (lambda () (error "Synthetic failure")))
    (acp--notify-disconnected client)
    (acp-shutdown :client client)
    (should (= 1 count))
    (should (map-elt client :closed))
    (should-not (map-elt client :disconnect-handlers))))

(provide 'acp-elicitation-test)
;;; acp-elicitation-test.el ends here
