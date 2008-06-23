#!r6rs
(import
  (rnrs base) ; no R6RS records
  (only (rnrs io simple) display write newline)
  (carneades srfi records))

(define-record-type thing
  (make-thing x)
  thing?
  (x thing-x)
  (y thing-y set-thing-y!))

(define t (make-thing 123))
(display "t => ") (write t) (newline)
(set-thing-y! t 'blah)
(display "t => ") (write t) (newline)
