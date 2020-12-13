;; very very very inspired by toxic' bootstrap.c
;; https://github.com/JFreegman/toxic/blob/master/src/bootstrap.c

(require 'cl-lib)
(require 'url)

(defvar toxe-bootstrap-url "https://nodes.tox.chat/json"
  "URL from which the list of bootstrap nodes is fetched.")

(defvar toxe-bootstrap-offline-timeout (* 60 60 24 2)
  "Number of seconds since last successful ping before we consider a node offline.")

(defun toxe-bootstrap-fetch-list ()
  "Fetch a list of known hosts that are part of tox network."
  (let ((res (with-current-buffer (url-retrieve-synchronously toxe-bootstrap-url)
               (goto-char (point-min))
               (mark-paragraph)
               (exchange-point-and-mark)
               (kill-region (point-min) (point))
               (kill-line)
               (json-parse-buffer))))
    (cl-loop with now = (/ (car (time-convert nil t)) 1000000000)
             for node being the elements of (gethash "nodes" res)
             for i from 0 to 5          ;limit
             when (< (- now (gethash "last_ping" node))
                     toxe-bootstrap-offline-timeout)
             collect (list (gethash "ipv4" node)
                           (gethash "port" node)
                           (gethash "public_key" node)))))

(provide 'toxe-bootstrap)
;;; toxe-bootstrap.el ends here
