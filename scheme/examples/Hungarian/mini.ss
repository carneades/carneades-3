#!r6rs

(import (rnrs)
       (carneades lkif2)
       (prefix (carneades evidence) e:)
       (carneades argument-builtins)
       (carneades rule)
       (carneades shell)
       (carneades lib srfi lightweight-testing))


(define null '())

(define import-data (lkif-import "mini.xml"))

(define vat (lkif-data-rulebase import-data))

; type question = excluded | priority | valid

(define (engine max-nodes max-turns critical-questions)
 (make-engine max-nodes max-turns 
              (list (generate-arguments-from-rules vat critical-questions)
                    builtins)))

(check (all-acceptable? '(goal-A X) (engine 100 1 null)) => #t)
(check (all-acceptable? '(goal-B X) (engine 100 1 null)) => #t)
