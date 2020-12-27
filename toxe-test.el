;;; toxe-tests --- Toxe test suite.                  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Omar Polo

;; Author: Omar Polo <op@omarpolo.com>

;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

;;; Commentary

;; Tests for toxe.el

;;; Code:

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
                         (:@type 55 :message "gnegne")
                         (:@type friend-message :friend-number 7)
                         (foo "bar" 77 "baz")
                         (foo))
           do (let ((got (toxe-test-send-plist test)))
                (should (equal test got)))))

(provide 'toxe-test)
;;; toxe-test.el ends here
