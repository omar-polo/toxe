;;; toxec --- Tox client for Emacs.  -*- lexical-binding: t; -*-

;;; Commentary

;; TODO

;;; Code:

(require 'cl-lib)

;; don't require dynamic module at byte compile time.
(declare-function toxe--start                  "toxe-core" (savepath))
(declare-function toxe--bootstap               "toxe-core" (host port public-key))
(declare-function toxe--save                   "toxe-core" (path tmp))
(declare-function toxe--stop                   "toxe-core" ())
(declare-function toxe-self-set-name           "toxe-core" (name))
(declare-function toxe-self-set-status-message "toxe-core" (msg))
(declare-function toxe-iteration-interval      "toxe-core" ())
(declare-function toxe-iterate                 "toxe-core" ())
(declare-function toxe-self-get-address        "toxe-core" ())
(declare-function toxe-friend-add              "toxe-core" (pk msg))
(declare-function toxe-friend-add-norequest    "toxe-core" (pk))
(declare-function toxe-friend-send-message     "toxe-core" (friend-number type msg))

(cl-eval-when (load eval)
  (require 'toxe-core))


;;; vars

(defvar toxe-user-name (user-login-name)
  "The username.")

(defvar toxe-status-message "Toxin' on Emacs"
  "Status message.")

;; fetched on 2020/12/11 using toxe-bootstrap.el
(defvar toxe-bootstrap-nodes
  '(("85.172.30.117" 33445 "8E7D0B859922EF569298B4D261A8CCB5FEA14FB91ED412A7603A585A25698832")
    ("85.143.221.42" 33445 "DA4E4ED4B697F2E9B000EEFE3A34B554ACD3F45F5C96EAEA2516DD7FF9AF7B43")
    ("tox.verdict.gg" 33445 "1C5293AEF2114717547B39DA8EA6F1E331E5E358B35F9B6B5F19317911C5F976")
    ("78.46.73.141" 33445 "02807CF4F8BB8FB390CC3794BDF1E8449E9A8392C5D3F2200019DA9F1E812E46")
    ("tox.initramfs.io" 33445 "3F0A45A268367C1BEA652F258C85F4A66DA76BCAA667A49E770BCC4917AB6A25")
    ("46.229.52.198" 33445 "813C8F4187833EF0655B10F7752141A352248462A567529A38B6BBF73E979307"))
  "Known DHT node for the boostrap.
List of triple (HOST PORT PUBLIC-KEY).")

(defvar toxe-savedata-dir (expand-file-name "toxe" user-emacs-directory)
  "Directory where store toxe data.  If nil, nothing will be stored.")

(defvar toxe-connection-status 'toxe-connection-none
  "The status of the connection.  Must be considered read-only.")

(defvar toxe--timer nil
  "The internal timer used to fetch updates.")


;;; hooks

(defvar toxe-friend-request-hook (list #'toxe-friend-request-always-accept)
  "Hook called upon receiving a friend request.
The function gets called with the public key of the peer and the
message.  Add `toxe-friend-request-always-accept' to
automatically accept friend requests.")

(defvar toxe-friend-message-hook (list #'toxe-log-friend-message)
  "Hook called upon receiving a message from a friend.
The functions will get called with the friend-number, a symbol
representing the tox message type and the message itself as
string.")

(defvar toxe-self-connection-status-hook (list #'toxe--self-connection-changed)
  "Hook called upon network status changes.
The functions will get called with a symbol representing the status of the connection:

 - toxe-connection-none    :: offline
 - toxe-connection-tcp     :: online (tcp)
 - toxe-connection-udp     :: online (udp)
 - toxe-connection-unknown :: unknown")


;;; errors

(define-error 'toxe-new-error "failure during initialisation"
  'error)

(define-error 'toxe-bootstrap-error "failure during bootstrapping"
  'error)

(define-error 'toxe-malformed-string "message couldn't be decoded as UTF8 string"
  'wrong-type-argument)


;;; implementation

(defun toxe-save ()
  "Save the client info in `toxe-savedata-dir'."
  (when toxe-savedata-dir
    (let* ((path (expand-file-name "toxe.data" toxe-savedata-dir))
           (tmp  (concat path ".tmp")))
      (toxe--save path tmp))))

(defun toxe-start ()
  "Start the client."
  (when (if toxe-savedata-dir
            (toxe--start (expand-file-name "toxe.data" toxe-savedata-dir))
          (toxe--start nil))
    (toxe-self-set-name toxe-user-name)
    (toxe-self-set-status-message toxe-status-message)
    (dolist (spec toxe-bootstrap-nodes)
      (condition-case err
          (apply #'toxe--bootstrap spec)
        (toxe-bootstrap-error (message "failure (but we continued) with %s: %s"
                                       spec err))))
    (setq toxe--timer (run-with-timer nil
                                      (/ (toxe-iteration-interval) 1000.0)
                                      #'toxe-iterate))
    (toxe-save)))

(defun toxe-stop ()
  "Stops the client."
  (when toxe--timer
    (cancel-timer toxe--timer))
  (toxe-save)
  (toxe--stop))

(defun toxe--self-connection-changed (status)
  (setq toxe-connection-status status)
  (message "toxe: connection status: %s" status))

(defun toxe-friend-request-always-accept (pk msg)
  "Accept the friend request from PK and log the message MSG.
Meant to be added to the toxe-friend-request-hook."
  (message "toxe: accepting friend request from %s: %s" pk msg)
  (toxe-friend-add-norequest pk))

(defun toxe-log-friend-message (friend-number msg-type msg)
  (message "toxe: got message (%s) from %s: %s" msg-type friend-number msg))

(provide 'toxe)
;;; toxe.el ends here
