;;; toxe-bootstrap -- Bootstrap Tox DHT              -*- lexical-binding: t; -*-

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

;;; Commentary

;; Utility code to fetch a list of known nodes from tox.chat to help
;; bootstrap the network when toxe starts.  Uses the same login as
;; toxic' boostrap.c:
;; https://github.com/JFreegman/toxic/blob/master/src/bootstrap.c

;;; Code:

(require 'cl-lib)
(require 'url)

(defvar toxe-bootstrap-url "https://nodes.tox.chat/json"
  "URL from which the list of bootstrap nodes is fetched.")

(defvar toxe-bootstrap-offline-timeout (* 60 60 24 2)
  "Number of seconds since last successful ping before we consider a node offline.")

(defun toxe-bootstrap-fetch-list (n)
  "Fetch a list of N known hosts that are part of tox network."
  (let ((res (with-current-buffer (url-retrieve-synchronously toxe-bootstrap-url)
               ;; there must be better way to parse an HTTP response
               (goto-char (point-min))
               (mark-paragraph)
               (exchange-point-and-mark)
               (kill-region (point-min) (point))
               (kill-line)
               (json-parse-buffer))))
    (cl-loop with now = (/ (car (time-convert nil t)) 1000000000)
             for node being the elements of (gethash "nodes" res)
             for i from 0 to n
             when (< (- now (gethash "last_ping" node))
                     toxe-bootstrap-offline-timeout)
             collect (list (gethash "ipv4" node)
                           (gethash "port" node)
                           (gethash "public_key" node)))))

(provide 'toxe-bootstrap)
;;; toxe-bootstrap.el ends here
