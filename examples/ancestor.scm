#!r6rs

(library
 (ancestor)
 
 (export)
 
 (import (rnrs) 
         (carneades rule)
         (carneades shell)
         (srfi/78 check))
 
 (define rb1 
   (rulebase
    
    (rule* facts 
           (parent Caroline Tom) 
           (parent Caroline Ines)
           (parent Dustin Tom)
           (parent Dustin Ines)
           (parent Tom Gloria)
           (parent Ines Hildegard)
           (Male Tom)
           (Parent Tom)
           (Female Ines)
           (Parent Ines)
           )
    
    (rule* r1 
           (if (parent ?x ?y)
               (ancestor ?x ?y)))   
    
    (rule* r2 
           (if (and (ancestor ?x ?z) 
                    (ancestor ?z ?y))
               (ancestor ?x ?y)))
    
    (rule* r3
           (if (and (Parent ?x)
                    (Male ?x))
               (Father ?x)))
    (rule* r4
           (if (Father ?x)
               (and
                (Male ?x)
                (Parent ?x))))
    
    (rule* r5
           (if (and (Female ?x)
                    (Parent ?x))
               (Mother ?x)))
    
    (rule* r6
           (if (Mother ?x)
               (and (Female)
                    (Parent))))
                
    
    )) ; rulebase
 
 
 ; engine integer integer  -> statement -> (stream-of argument-state)
 (define (engine max-nodes max-turns)
   (make-engine max-nodes max-turns 
                (list (generate-arguments-from-rules rb3 '()))))
 
 (define e1 (engine 50 1))
 
 
 (check (all-acceptable? '(parent ?x ?y) e1) => #t)
 (check (all-acceptable? '(ancestor ?x ?y) e1) => #t)
 (check (all-acceptable? '(ancestor Caroline ?y) e1) => #t)
 (check (all-acceptable? '(ancestor Caroline Tom) e1) => #t)
 (check (failure? '(parent Hildegard Tom) e1) => #t)
 (check (all-acceptable? '(ancestor Caroline Gloria) e1) => #t)
 (check (all-acceptable? '(applies ?r (parent ?x ?y)) e1) => #t)
 (check (all-acceptable? '(applies facts (parent Caroline Tom)) e1) => #t)
 (check (all-acceptable? '(Father Tom) e1) => #t)
 (check (all-acceptable? '(Mother Ines) e1) => #t)
 
 
 ; (test/text-ui tests)
 (check-report)
 
 ) ; end library