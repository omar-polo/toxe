;;; toxe-tests --- Toxe test suite.                  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Omar Polo

;; Author: Omar Polo <op@omarpolo.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

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
