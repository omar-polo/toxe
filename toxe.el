;;; toxec --- Tox client for Emacs.  -*- lexical-binding: t; -*-

;;; Commentary

;; TODO

;;; Code:

(require 'cl-lib)


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

 - toxe-connection-none    :: offline
 - toxe-connection-tcp     :: online (tcp)
 - toxe-connection-udp     :: online (udp)
 - toxe-connection-unknown :: unknown")


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
  friend-number public-key last-seen status
  name status-message conn-status)

(defun toxe-connection-status ()
  "Return the status of the connection."
  toxe--connection-status)

(defun toxe-self-address ()
  "Return the address of the toxe instance."
  toxe--self-address)

(defun toxe--friend-by-number (number)
  "Return the friend with the given NUMBER."
  (cl-find-if (lambda (x)
                (= number (toxe--friend-friend-number x)))
              toxe--contacts))

(defun toxe--make-process ()
  "Start toxe."
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
  "Syntactical sugar over cl-defmethod.
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
  (push (make-toxe--friend :friend-number friend-number
                           :public-key public-key
                           :last-seen last-seen
                           :status status
                           :name name
                           :status-message status-message
                           :conn-status conn-status)
        toxe--contacts))

(toxe--defhandler chatlist-end (@status)
  (unless @status
    (error "toxe: chatlist-end failed")))

(toxe--defhandler friend-request (public-key message)
  (message "toxe: friend-request from pk %s with message: %s"
           public-key
           message))

(toxe--defhandler friend-message (friend-number message-type message)
  (message "toxe: %s message from %s: %s" message-type friend-number message))

(toxe--defhandler connection-status (connection-status)
  (setq toxe--connection-status connection-status)
  (message "toxe: connection status %s" connection-status))

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

(defun toxe--send-request (req)
  "Send REQ to the `toxe--process'."
  (when (process-live-p toxe--process)
    (process-send-string (process-buffer toxe--process)
                         (with-output-to-string
                           ;; XXX: toxe cannot parse symbols (yet).
                           ;; Silently transform them into strings.
                           (prin1 (mapcar (lambda (x)
                                            (if (and (symbolp x)
                                                     (not (keywordp x)))
                                                (symbol-name x)
                                              x))
                                          req))
                           (princ "\n")))))

(defmacro toxe--defcmd (cmd args &optional doc)
  (let ((s (intern (concat "toxe--cmd-" (symbol-name cmd)))))
    `(defun ,s ,args
       ,doc
       (toxe--send-request
        ,(cl-loop with c = (list 'list :@type (list 'quote cmd))
                  for arg in args
                  do (nconc c (list (intern (concat ":" (symbol-name arg)))
                                    arg))
                  finally (return c))))))

(toxe--defcmd self-set-name (name)
  "Set the name to NAME.")

(toxe--defcmd self-set-status-message (message)
  "Set the status message to MESSAGE.")

(toxe--defcmd self-get-address ()
  "Get our address.")

(provide 'toxe)
;;; toxe.el ends here
