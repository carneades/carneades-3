#!r6rs

(import (rnrs)
        (carneades lkif2)
        (carneades shell)
        (carneades argument-builtins)
        (carneades rule)
        (carneades lib srfi lightweight-testing))




(define import-data (lkif-import "goods.xml"))

(define goods (lkif-data-rulebase import-data))

(define (engine max-nodes max-turns critical-questions)
  (make-engine max-nodes max-turns 
               (list (generate-arguments-from-rules goods critical-questions)
                     builtins)))

(check (all-in? '(Goods g1) (engine 20 1 '())) => #t)
(check (not (all-in? '(Goods g1) (engine 20 2 '()))) => #t)
(check-report)