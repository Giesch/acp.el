;;; run-batch.el --- Batch test runner for acp.el -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Runs the suite with `ert-run-tests-batch' and returns the counts as a
;; string, so callers over `emacsclient -e' get total, completed,
;; unexpected, and skipped numbers without batch-and-exit semantics:
;;
;;   emacsclient -s SOCKET -e '(load "tests/run-batch.el")'
;;   emacsclient -s SOCKET -e '(acp-tests-run-batch "DIR")'
;;
;; DIR is the repository root; it defaults to the directory one level up
;; from this file.

;;; Code:

(require 'ert)

(defun acp-tests-run-batch (&optional root selector)
  "Run the acp.el suite from ROOT with SELECTOR.

Loads acp, acp-fakes, and the tests under ROOT/tests, then runs
`ert-run-tests-batch'.  SELECTOR defaults to \"acp-test\".  Returns a
summary string with total, completed, unexpected, and skipped counts."
  (interactive)
  (let* ((root (or root
                   (expand-file-name ".." (file-name-directory
                                           (or load-file-name
                                               buffer-file-name
                                               default-directory)))))
         (selector (or selector "acp-test")))
    (add-to-list 'load-path (expand-file-name root))
    ;; An Emacs with another acp already loaded would satisfy `require'
    ;; from that copy; load the checkout explicitly so it wins.
    (dolist (file '("acp.el" "acp-traffic.el" "acp-fakes.el"))
      (load (expand-file-name file root) nil t))
    (dolist (file (directory-files (expand-file-name "tests" root) t "\\.el\\'"))
      (load file nil t))
    (let* ((stats (ert-run-tests-batch selector))
           (tests (ert--stats-tests stats))
           (results (mapcar #'ert-test-most-recent-result tests))
           (pairs (seq-mapn #'cons tests results))
           (skipped (cl-count-if #'ert-test-skipped-p results)))
      (format "SUITE=acp TOTAL=%d COMPLETED=%d UNEXPECTED=%d SKIPPED=%d PROVENANCE=%s %s"
              (length tests)
              (cl-count-if #'identity results)
              (cl-count-if (lambda (pair)
                             (not (ert-test-result-expected-p (car pair)
                                                              (cdr pair))))
                           pairs)
              skipped
              (symbol-file 'acp-send-request)
              (mapconcat (lambda (pair)
                           (format "%s=%s"
                                   (ert-test-name (car pair))
                                   (acp-tests--result-message (cdr pair))))
                         (seq-filter (lambda (pair)
                                       (or (ert-test-skipped-p (cdr pair))
                                           (not (ert-test-result-expected-p
                                                 (car pair) (cdr pair)))))
                                     pairs)
                         " ")))))

(defun acp-tests--result-message (result)
  "Return a one-line description of a test RESULT."
  (cond ((ert-test-skipped-p result)
         (format "skipped: %S"
                 (ert-test-result-with-condition-condition result)))
        ((ert-test-result-with-condition-p result)
         (replace-regexp-in-string
          "\n" "; "
          (format "%S" (ert-test-result-with-condition-condition result))))
        (t "unexpected result")))

(provide 'run-batch)
;;; run-batch.el ends here
