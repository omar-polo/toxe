;;; toxe-chat.el --- toxe chatbuf stuff              -*- lexical-binding: t; -*-

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

;;; Commentary:

;; Toxe chatbuf.

;;; Code:

(require 'cl-lib)
(require 'ewoc)

;; iimage for inline images?

;; toxe.el
(defvar toxe-user-name)

(declare-function toxe--cmd-friend-send-message "toxe" (friend-number message-type message))

(declare-function toxe--friend-number "toxe" (friend))
(declare-function toxe--friend-name   "toxe" (friend))

(declare-function make-toxe--message "toxe")

(defvar-local toxe-chat-friend nil
  "Friend for the current tox-chat buffer.")

(defvar-local toxe-chat-messages nil
  "List of messages in the current toxe chat buffer.")

(defvar-local toxe-chat-ewoc nil
  "EWOC data for the current toxe chatbuf.")

(defmacro toxe-with-accessors (spec type struct &rest body)
  "Like CL' with-accessors.
Destructure a STRUCT of the given TYPE using SPEC.  SPEC is in
the form ((VAR-NAME ACCESSOR-NAME)).  BODY is then executed with
the bindings set."
  (declare (indent 3))
  (let ((o (gensym)))
    `(let ((,o ,struct))
       (cl-symbol-macrolet
           ,(cl-loop for (var acc) in spec
                     collect `(,var (cl-struct-slot-value ,type ',acc ,o)))
         ,@body))))

(defun toxe-chat--ewoc-pp (msg)
  "Pretty print MSG (for EWOC)."
  (toxe-with-accessors ((from from)
                        (text tex))
      'toxe--message msg
    (insert from ":\t" text)))

(defvar toxe-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'toxe-chat-send)
    (define-key map (kbd "i") #'toxe-chat-send)
    map)
  "Keymap for toxe chat buffers.")

(defvar toxe-chat--pass-friend nil)

(define-derived-mode toxe-chat-mode special-mode "toxe-chat"
  "mode for the toxe chatbuf."
  (erase-buffer)
  (buffer-disable-undo)
  (setq toxe-chat-friend toxe-chat--pass-friend
        toxe-chat-ewoc
        (ewoc-create #'toxe-chat--ewoc-pp
                     (format "Chat with %s\n\n"
                             (toxe--friend-name toxe-chat-friend)))))

(defun toxe-chat--insert (from type msg)
  "Insert the message MSG with TYPE from the user FROM."
  (let ((datum (make-toxe--message :from from
                                   :type type
                                   :text msg)))
    (setq toxe-chat-messages
          (vconcat toxe-chat-messages datum))
    (ewoc-enter-last toxe-chat-ewoc datum)
    (ewoc-invalidate toxe-chat-ewoc (ewoc-nth toxe-chat-ewoc -1))
    (goto-char (point-max))))

;;;###autoload
(defun toxe-chat-send ()
  "Prompt for a message and send it."
  (interactive)
  (when-let (msg (read-string "Message: "))
    (toxe-chat--insert (or toxe-user-name "me") 'normal msg)
    (toxe--cmd-friend-send-message (toxe--friend-number toxe-chat-friend)
                                   'normal
                                   msg)))

(provide 'toxe-chat)
;;; toxe-chat.el ends here
