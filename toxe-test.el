(require 'cl-lib)
(require 'ert)
(require 'toxe)

(defun toxe-test-call (test-flag input)
  "Call toxe running the given test and return its output.
TEST-FLAG is the flag representing the wanted test, and INPUT
what will be fed to toxe' standard input."
  (with-temp-buffer
    (let* ((process-file-side-effects nil))
      (when (zerop (call-process-region input nil "toxe" nil t nil (format "-T%s" test-flag)))
        (buffer-string)))))

(defun toxe-test-send-plist (p)
  "Sends P to toxe running in parse test, yielding its output."
  (let ((out (toxe-test-call "p"
                             (with-output-to-string
                               (prin1 p)
                               (princ "\n")))))
    (unless out
      (signal 'error (format "toxe crashed with input %s" p)))
    (read out)))

(ert-deftest toxe-can-parse-plists-test ()
  (cl-loop for test in '((:@type 55)
                         (:@type "foobar")
                         (:@type 55 :message "gnegne"))
           do (let ((got (toxe-test-send-plist test)))
                (should (equal test got)))))

(provide 'toxe-test)
;;; toxe-test.el ends here
