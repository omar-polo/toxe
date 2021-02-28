;;; toxe --- Tox client for Emacs.                   -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Omar Polo

;; Author: Omar Polo <op@omarpolo.com>
;; Keyword: comm

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

;; Toxe is a Tox client for Emacs.

;; Usage:

;; To start the client, do
;;
;; M-x toxe RET

;;; Code:

(eval-when-compile (require 'subr-x))

(require 'cl-lib)
(require 'toxe-chat)


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
    ("46.229.52.198" 33445 "813C8F4187833EF0655B10F7752141A352248462A567529A38B6BBF73E979307")
    ("144.217.167.73" 33445 "7E5668E0EE09E19F320AD47902419331FFEE147BB3606769CFBE921A2A2FD34C")
    ("tox.abilinski.com" 33445 "10C00EB250C3233E343E2AEBA07115A5C28920E9C8D29492F6D00B29049EDC7E")
    ("tox.novg.net" 33445 "D527E5847F8330D628DAB1814F0A422F6DC9D0A300E6C357634EE2DA88C35463")
    ("95.31.18.227" 33445 "257744DBF57BE3E117FE05D145B5F806089428D4DCE4E3D0D50616AA16D9417E")
    ("198.199.98.108" 33445 "BEF0CFB37AF874BD17B9A8F9FE64C75521DB95A37D33C5BDB00E9CF58659C04F"))
  "Known DHT node for the boostrap.
List of triple (HOST PORT PUBLIC-KEY).")

(defvar toxe-savedata-dir (expand-file-name "toxe" user-emacs-directory)
  "Directory where store toxe data.  If nil, nothing will be stored.")


;;; hooks

(defvar toxe-friend-request-hook nil
  "Hook called upon receiving a friend request.
The function gets called with the public key of the peer and the
message.")

(defvar toxe-friend-message-hook nil
  "Hook called upon receiving a message from a friend.
The functions will get called with the friend-number, a symbol
representing the tox message type and the message itself as
string.")

(defvar toxe-self-connection-status-hook nil
  "Hook called upon network status changes.
The functions will get called with a symbol representing the
status of the connection:

 - none    (offline)
 - tcp     (online on a tcp connection)
 - udp     (online on a udp connection)
 - unknown (unknown connection status)")


;;; implementation

(defvar toxe--process nil
  "The toxe process.")

(defvar toxe--connection-status 'none
  "The status of the connection.")

(defvar toxe--self-address nil
  "Our address.")

(defvar toxe--contacts nil
  "Holds the contact list.")

(cl-defstruct toxe--friend
  number public-key last-seen status
  name status-message conn-status
  buffer)

(cl-defstruct toxe--message
  from type text)

(defun toxe-connection-status ()
  "Return the status of the connection."
  toxe--connection-status)

(defun toxe-self-address ()
  "Return the address of the toxe instance."
  toxe--self-address)

(defun toxe--friend-by-number (number)
  "Return the friend with the given NUMBER."
  (cl-find-if (lambda (x)
                (= number (toxe--friend-number x)))
              toxe--contacts))

(defun toxe--make-process ()
  "Make the toxe process."
  (unless (process-live-p toxe--process)
    (let (cmd)
      (when toxe-savedata-dir
        (ignore-errors (mkdir toxe-savedata-dir t))
        (push (concat "-s" toxe-savedata-dir "/toxe.data") cmd))
      (cl-loop for (host port key) in toxe-bootstrap-nodes
               do (push (with-output-to-string
                          (princ "-B")
                          (prin1 (list :host host
                                       :port port
                                       :public-key key)))
                        cmd))
      (push "toxe" cmd)
      (setq toxe--process
            (make-process :name "toxe"
                          :buffer (get-buffer-create "*toxe-cmd*")
                          :command cmd
                          :connection 'pipe
                          :filter #'toxe--proc-filter
                          :sentinel (lambda (_proc s)
                                      (message "toxe: status %s" s)))))))

(define-derived-mode toxe-mode special-mode "toxe"
  "Mode for the toxe main buffer.")

(defvar toxe--pending-friend-requests nil
  "List of pending friend requests.")

;;;###autoload
(defun toxe ()
  "Start toxe."
  (interactive)
  (unless (process-live-p toxe--process)
    (toxe--make-process)
    (toxe--cmd-get-chatlist))
  (pop-to-buffer "*toxe*")
  (toxe-mode)
  (use-local-map widget-keymap)
  (widget-setup)
  (toxe--update-root-buffer))

(defvar toxe-ascii-art "
ooooooooooooo
8'   888   `8
     888    .ooooo.  oooo    ooo  .ooooo.
     888   d88' `88b  `88b..8P'  d88' `88b
     888   888   888    Y888'    888ooo888
     888   888   888  .o8\"'88b   888    .o
    o888o  `Y8bod8P' o88'   888o `Y8bod8P'"
  "The ASCII art for the banner.")

(defun toxe--update-root-buffer ()
  "Draw/update the *toxe* buffer."
  (with-current-buffer "*toxe*"
    (let ((inhibit-read-only t))
      (erase-buffer)
      (remove-overlays)
      (widget-insert toxe-ascii-art "\n\n")
      (widget-insert "Your address: " (propertize toxe--self-address 'face 'font-lock-constant-face) "\n\n")
      (widget-insert "Chats:\n\n")
      (cl-loop for contact in toxe--contacts
               do (widget-insert "* ")
               do (widget-create 'push-button
                                 :notify (lambda (&rest _ignore)
                                           (pop-to-buffer
                                            (toxe--friend-getcreate-buffer contact)))
                                 (toxe--friend-name contact))
               do (widget-insert "\n"))
      (when toxe--pending-friend-requests
        (widget-insert "\n\nPending friend requests:\n\n")
        (cl-loop for (key message) in toxe--pending-friend-requests
                 do (widget-insert "* ")
                 do (widget-create 'push-button
                                   :notify (lambda (&rest _ignore)
                                             (message "deleting %s from the list" key)
                                             (setq toxe--pending-friend-requests
                                                   (cl-delete-if (lambda (r)
                                                                   (string= (car r) key))
                                                                 toxe--pending-friend-requests))
                                             (toxe--cmd-friend-add key)
                                             (toxe--cmd-get-chatlist))
                                   "Accept")
                 do (widget-insert " "
                                   (propertize message 'face 'font-lock-comment-face) "\n"
                                   "from: "
                                   (propertize key 'face 'font-lock-constant-face)
                                   "\n\n"))))))

(defun toxe--friend-getcreate-buffer (friend)
  "Return the buffer with the chat with the given FRIEND."
  (let ((buf (toxe--friend-buffer friend)))
    (if (and buf (buffer-live-p buf))
        buf
      (setf (toxe--friend-buffer friend)
            (let ((buf (get-buffer-create (format "*toxe-%s*"
                                                  (toxe--friend-name friend)))))
              (with-current-buffer buf
                (let ((toxe-chat--pass-friend friend))
                  (toxe-chat-mode)))
              buf)))))

;;;###autoload
(defun toxe-kill ()
  "Closes toxe."
  (interactive)
  (when (process-live-p toxe--process)
    (toxe--cmd-quit)
    (kill-buffer (process-buffer toxe--process))
    (dolist (c toxe--contacts)
      (when-let (buf (toxe--friend-buffer c))
        (kill-buffer buf)))
    (kill-buffer "*toxe*")
    (setq toxe--process nil
          toxe--contacts nil
          toxe--connection-status 'none)
    t))

(defun toxe--proc-filter (proc string)
  "Filter for toxe process.
PROC is toxe and STRING part of its output."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (save-excursion
        (goto-char (process-mark proc))
        (insert string)
        (set-marker (process-mark proc) (point)))
      (move-beginning-of-line nil)
      (cl-loop while (not (= (point) (point-max)))
               ;; TODO: what happens if a line is very long and emacs
               ;; decide to split it?
               do (toxe--dispatch-event
                   (read (buffer-substring (point)
                                           (save-excursion
                                             (move-end-of-line nil)
                                             (point)))))
               do (forward-line)))))

(defun toxe--dispatch-event (plist)
  "Dispatch the received PLIST."
  (apply #'toxe--handle-event (plist-get plist :@type) plist))

(cl-defgeneric toxe--handle-event (type &rest params)
  (:documentation "Handler for the toxe event TYPE with PARAMS.")
  (message "toxe: unhandled event %s with params %s" type params))

(defmacro toxe--defhandler (type decls &rest body)
  "Syntactical sugar over `cl-defmethod'.
TYPE is the symbol of the handled method.  DECLS is the
declaration, it's implicitly put as &key, so order doesn't
matter.  BODY is the implementation."
  (declare (indent defun))
  (let ((typesym (gensym "_type")))
    `(cl-defmethod toxe--handle-event ((,typesym (eql ,type))
                                       &key ,@decls
                                       &allow-other-keys)
       ,@body)))

(toxe--defhandler self-get-address (address)
  (setq toxe--self-address address))

(toxe--defhandler chatlist-start ()
  (setq toxe--contacts nil))

(toxe--defhandler chatlist-entry (friend-number public-key last-seen status
                                                name status-message conn-status)
  (push (make-toxe--friend :number friend-number
                           :public-key public-key
                           :last-seen last-seen
                           :status status
                           :name name
                           :status-message status-message
                           :conn-status conn-status)
        toxe--contacts))

(toxe--defhandler chatlist-end (@status)
  (unless @status
    (error "[toxe] chatlist-end failed"))
  (toxe--update-root-buffer))

(toxe--defhandler friend-request (public-key message)
  (add-to-list 'toxe--pending-friend-requests (list public-key message))
  (toxe--update-root-buffer)
  (message "toxe: friend-request from pk %s with message: %s"
           public-key
           message)
  (run-hook-with-args toxe-friend-request-hook public-key message))

(toxe--defhandler friend-message (friend-number message-type message)
  (when-let (f (toxe--friend-by-number friend-number))
    (with-current-buffer (toxe--friend-getcreate-buffer f)
      (toxe-chat--insert (toxe--friend-name f) message-type message)))
  (run-hook-with-args toxe-friend-message-hook friend-number message-type message))

(toxe--defhandler connection-status (connection-status)
  (setq toxe--connection-status connection-status)
  (message "toxe: connection status %s" connection-status)
  (run-hook-with-args toxe-self-connection-status-hook connection-status))

(toxe--defhandler friend-name (friend-number name)
  (when-let (f (toxe--friend-by-number friend-number))
    (setf (toxe--friend-name f) name)))

(toxe--defhandler friend-status-message (friend-number message)
  (when-let (f (toxe--friend-by-number friend-number))
    (setf (toxe--friend-status-message f) message)))

(toxe--defhandler friend-status (friend-number status)
  (when-let (f (toxe--friend-by-number friend-number))
    (setf (toxe--friend-status f) status)))

(toxe--defhandler friend-connection-status (friend-number connection-status)
  (when-let (f (toxe--friend-by-number friend-number))
    (setf (toxe--friend-conn-status f) connection-status)))

(toxe--defhandler friend-read-receipt (friend-number message-id)
  )

(defun toxe--send-request (req)
  "Send REQ to the `toxe--process'."
  (when (process-live-p toxe--process)
    (process-send-string (process-buffer toxe--process)
                         (concat
                          (replace-regexp-in-string
                           "\n" "\\\\n"
                           (with-output-to-string
                             (prin1 req)))
                          "\n"))))

(defmacro toxe--defcmd (cmd args &optional documentation)
  "Define a function that invokes the toxe CMD with ARGS.
Every method should have a DOCUMENTATION."
  (declare (indent defun))
  (let ((s (intern (concat "toxe--cmd-" (symbol-name cmd)))))
    `(defun ,s ,args
       ,documentation
       (toxe--send-request
        ,(cl-loop with c = (list 'list :@type (list 'quote cmd))
                  for arg in args
                  do (nconc c (list (intern (concat ":" (symbol-name
                                                         (if (consp arg)
                                                             (car arg)
                                                           arg))))
                                    arg))
                  finally (return c))))))

(toxe--defcmd self-set-name (name)
  "Set the name to NAME.")

(toxe--defcmd self-set-status-message (message)
  "Set the status message to MESSAGE.")

(toxe--defcmd self-get-address ()
  "Get our address.")

;; (toxe--defcmd friend-add (public-key &optional message)
;;   "Send a friend request with MESSAGE to the user identified by PUBLIC-KEY.")
(defun toxe--cmd-friend-add (public-key &optional message)
  "Send a friend request with MESSAGE to the user identified by PUBLIC-KEY."
  (toxe--send-request `(:@type friend-add
                               :public-key ,public-key
                               ,@(when message
                                   `(:message ,message)))))

(toxe--defcmd friend-send-message (friend-number message-type message)
  "Send MESSAGE with the given MESSAGE-TYPE to FRIEND-NUMBER.")

(toxe--defcmd get-chatlist ()
  "Retrieve the list of contacts.")

(toxe--defcmd quit ()
  "Quit toxe.")

(provide 'toxe)
;;; toxe.el ends here
