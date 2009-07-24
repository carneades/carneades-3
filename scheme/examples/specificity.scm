#!r6rs

(import (rnrs base)
        (carneades base)
        (carneades shell)
        (prefix (carneades argument) arg:)
        (carneades argument-builtins)
        (carneades rule)
        (carneades lib srfi lightweight-testing)
        )

(define rb1 
  (rulebase
   
   (rule r1 
         (if (bird? ?x)
             (flies ?x)))
   
   (rule r2 
         (if (penguin ?x)
             (bird ?x)))
  
   (rule r3
         (if (penguin ?x)
            (and (excluded r1 (flies ?x))
                  (not (flies ?x)))))
   
   (rule r4 
         (if (wierd-penguin ?x)
             (penguin ?x)))
   
   (rule r5 
         (if (wierd-penguin ?x)
             (and (excluded r3 (not (flies ?x)))
                  (flies ?x))))
   
   )) ; end of rule base

; accept some facts 
(define ag1
  (arg:accept arg:empty-argument-graph 
              '((wierd-penguin Tweety))))

; type question = excluded | priority | valid


; engine integer integer (list-of symbol) -> statement -> (stream-of argument-state)
(define (engine max-nodes max-turns critical-questions)
  (make-engine* max-nodes max-turns ag1
                (list (generate-arguments-from-rules rb1 critical-questions) builtins)))

(define e1 (engine 20 1 '(excluded)))
(define e2 (engine 20 2 '(excluded)))
(define e3 (engine 20 3 '(excluded)))


; Example commands
; (ask '(goods ?x) (engine 20 2 null))
; (show '(goods ?x) (engine 20 2 '(priority)))
