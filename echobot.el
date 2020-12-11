;; echobot written with toxe.el

(require 'toxe)

;; start the client
(toxe-start)

;; get my address
(toxe-self-get-address)

;; check the connection status
toxe-connection-status

;; quit
(toxe-stop)
