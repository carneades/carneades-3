#!r6rs

(import (rnrs) 
        (carneades rule)
        (carneades shell)
        (carneades argument)
        (carneades argument-builtins)
        (carneades lib srfi lightweight-testing))

(define rb1 
  (rulebase
   
   (rule* r1 
          (if (parent ?x ?y)
              (ancestor ?x ?y)))   
   
   (rule* r2 
          (if (and (ancestor ?x ?z) 
                   (parent ?z ?y))
              (ancestor ?x ?y)))
   
   (rule* r3
          (if (and (male ?x) 
                   (parent ?y ?x))
              (father ?x)))
   
   (rule* r4
          (if (and (female ?x)
                   (parent ?y ?x))
              (mother ?x)))
   
  ; (rule* r5 (male Tom))
  ; (rule* r6 (parent Caroline Tom))
   
   )) ; rulebase

(define assumptions
  '((parent Caroline Tom) 
    (parent Caroline Ines)
    (parent Dustin Tom)
    (parent Dustin Ines)
    (parent Tom Gloria)
    (parent Ines Hildegard)
    (male Tom)
    (female Ines)
    ))

; engine integer integer  -> statement -> (stream-of argument-state)
(define (engine max-nodes max-turns)
  (make-engine* max-nodes max-turns 
               (accept empty-argument-graph assumptions)
               (list builtins 
                     (generate-arguments-from-rules rb1 '()))))

(define e1 (engine 50 1))

;(check (all-in? '(parent ?x ?y) e1) => #t)
;(check (all-in? '(ancestor ?x ?y) e1) => #t)
;(check (all-in? '(ancestor Caroline ?y) e1) => #t)
;(check (all-in? '(ancestor Caroline Tom) e1) => #t)
;(check (no-argument-found? '(parent Hildegard Tom) e1) => #t)
;(check (all-in? '(ancestor Caroline Gloria) e1) => #t)
;(check (all-in? '(applies ?r (ancestor ?x ?y)) e1) => #t)
;(check (all-in? '(applies r1 (ancestor Caroline Tom)) e1) => #t)
;(check (all-in? '(father Tom) e1) => #t)
;(check (all-in? '(mother Ines) e1) => #t)
;
;
;;; (test/text-ui tests)
;(check-report)
