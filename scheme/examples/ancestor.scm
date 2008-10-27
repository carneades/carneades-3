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
          (if (and (Parent ?x)
                   (Male ?x))
              (Father ?x)))
   (rule* r4
          (if (Father ?x)
              (and (Male ?x)
                   (Parent ?x))))
   
   (rule* r5
          (if (and (Female ?x)
                   (Parent ?x))
              (Mother ?x)))
   
   (rule* r6
          (if (Mother ?x)
              (and (Female ?x)
                   (Parent ?x))))
   
   
   )) ; rulebase

(define assumptions
  '((parent Caroline Tom) 
    (parent Caroline Ines)
    (parent Dustin Tom)
    (parent Dustin Ines)
    (parent Tom Gloria)
    (parent Ines Hildegard)
    (Male Tom)
    (Parent Tom)
    (Female Ines)
    (Parent Ines)
    ))

; engine integer integer  -> statement -> (stream-of argument-state)
(define (engine max-nodes max-turns)
  (make-engine* max-nodes max-turns 
               (accept default-context assumptions)
               (list (generate-arguments-from-rules rb1 '())
                     builtins)))

(define e1 (engine 50 1))


(check (all-in? '(parent ?x ?y) e1) => #t)
(check (all-in? '(ancestor ?x ?y) e1) => #t)
(check (all-in? '(ancestor Caroline ?y) e1) => #t)
(check (all-in? '(ancestor Caroline Tom) e1) => #t)
(check (no-argument-found? '(parent Hildegard Tom) e1) => #t)
(check (all-in? '(ancestor Caroline Gloria) e1) => #t)
(check (all-in? '(applies ?r (ancestor ?x ?y)) e1) => #t)
(check (all-in? '(applies r1 (ancestor Caroline Tom)) e1) => #t)
(check (all-in? '(Father Tom) e1) => #t)
(check (all-in? '(Mother Ines) e1) => #t)


; (test/text-ui tests)
(check-report)
