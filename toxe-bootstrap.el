;;; toxe-bootstrap -- Bootstrap Tox DHT              -*- lexical-binding: t; -*-

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
