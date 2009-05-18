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

(define e1 (engine 100 1 null))

(check (succeed? '(p a) e1) => #t)
(check (succeed? '(not (q a)) e1) => #t)
(check (succeed? '(r b) e1) => #t)
