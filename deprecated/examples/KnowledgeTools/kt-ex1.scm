#!r6rs

(import (rnrs base)
        (rnrs hashtables)
        (prefix (rnrs lists) list:)
        (prefix (carneades lkif2) lkif:)
        (carneades statement)
        (carneades shell)
        (carneades stream)
        (carneades argument-search)
        (prefix (carneades argument) argument:)
        (carneades argument-diagram)
        (carneades argument-from-arguments)
        (carneades lib srfi lightweight-testing))

(define null '())

(define lkif-data (lkif:lkif-import "kt-ex1.xml"))
 
(define ag1 (car (lkif:lkif-data-argument-graphs lkif-data)))
(define ag2 (argument:accept argument:empty-argument-graph (list "max. 10% of nominal value")))

; engine integer integer argument-graph -> statement -> (stream-of argument-state)
(define e1 
  (make-engine* 20 1 ag2
                (list (generate-arguments-from-argument-graph ag1))))

; (view ag1)
; (ask1 "cash payment" e1) 
(show1 "cash payment" e1) 
