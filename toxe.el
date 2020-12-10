;;; toxec --- Tox client for Emacs.  -*- lexical-binding: t; -*-

;;; Commentary

;; TODO

;;; Code:

(require 'cl-lib)

;; don't require dynamic module at byte compile time.
(declare-function toxe-start                "toxe-core" ())
(declare-function toxe-stop                 "toxe-core" ())
(declare-function toxe-iteration-interval   "toxe-core" ())
(declare-function toxe-iterate              "toxe-core" ())
(declare-function toxe-self-get-address     "toxe-core" ())
(declare-function toxe-friend-add           "toxe-core" (pk msg))
(declare-function toxe-friend-add-norequest "toxe-core" (pk))
(declare-function toxe-friend-send-message  "toxe-core" (friend-number type msg))

(cl-eval-when (load eval)
  (require 'toxe-core))


;;; hooks

(defvar toxe-friend-request-hook nil
  "Hook called upon receiving a friend request.
The function gets called with the public key of the peer and the message.")

(defvar toxe-friend-message-hook nil
  "Hook called upon receiving a message from a friend.
The functions will get called with the friend-number, a symbol
representing the tox message type and the message itself as
string.")


;;; errors

(define-error 'toxe-malformed-string "message couldn't be decoded as UTF8 string"
  'wrong-type-argument)


;;; implementation

(provide 'toxe)
;;; toxe.el ends here
