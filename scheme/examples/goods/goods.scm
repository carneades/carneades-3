#!r6rs

(import (rnrs)
        (prefix (carneades lkif) lkif:)
        (carneades shell)
        (carneades argument-builtins)
        (carneades rule)
        (carneades lib srfi lightweight-testing))

(define null '())

(define goods 
  (add-rules empty-rulebase 
             (lkif:import "goods.xml")))

; type question = excluded | priority | valid

(define (engine max-nodes max-turns critical-questions)
  (make-engine max-nodes max-turns 
               (list (generate-arguments-from-rules goods critical-questions)
                     builtins)))

(check (all-acceptable? '(Goods g1) (engine 20 1 null)) => #t)
(check (not (all-acceptable? '(Goods g1) (engine 20 2 null))) => #t)
(check-report)

; (ask1 '(Goods ?x) (engine 20 1 null))

