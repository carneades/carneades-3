(require 'srfi-0)
(require 'srfi-8)
(require 'srfi-13)

(require 'socket)
(load "../prelude.scm")
(load "../util.scm")
(load "../input-parse.scm")
(load "mime.scm")

(load "http.scm")

(http-transaction 'GET "http://www.goggle.com/" '() (lambda x x))

(define s (make-client-socket "www.google.com" 80))
(define op (socket-output-port s))
(define ip (socket-input-port s))

(display "GET HTTP/1.0 /" op)
(write-char #x10 op)
(write-char #x13 op)
(flush-output-port op)
