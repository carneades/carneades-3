#!r6rs

; Constructs an argument graph with all kinds of statement nodes, premises and arguments
; and exports the graph to LKIF. For testing different visualization and mapping
; methods.

(import (rnrs)
        (carneades argument)
        (carneades rule)
        (carneades lkif2)
        (carneades argument-diagram))


(define-argument a1
  (pro "P"
       (pr "Q")
       (am "R")
       (ex "S")))

(define-argument a2
  (con "P"
        (pr '(not "T"))
        (am '(not "U"))
        (ex '(not "V"))))

(define-argument a3 
  (pro "R" 
       (pr "W")))

(define-argument a4 
  (pro "V"
      (pr "X")))

(define-argument a5
  (pro "Q"
       (am "Y")))

(define ag1 (make-argument-graph 'ag1 "argument graph test" "P"))
(set! ag1 (assert-arguments ag1 (list a1 a2 a3 a4 a5)))
(set! ag1 (accept ag1 (list "Q" '(not "S") "W" "X")))
(set! ag1 (reject ag1 (list "T"))) ; to check that reject is same as accepting the complement
(set! ag1 (question ag1 (list "R" "Y")))
(set! ag1 (assign-standard ag1 'se (list "P")))

; (define ld (make-lkif-data (lkif-data-sources kb1) (lkif-data-rulebase kb1) (list ag1)))
(define ld (make-lkif-data '() (rulebase) (list ag1)))
(lkif-export '() ld "argument-map-tests2.xml")

(view ag1)