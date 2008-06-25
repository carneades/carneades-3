#!r6rs

(library 
 (ontologytest)
 
 (export)
 
 (import (rnrs) 
         (carneades dlp)
         (carneades shell)
         (carneades argument-builtins)
         (carneades lib srfi lightweight-testing))
 
 (define kb1
   (ontology
    
    (axiom fact1 (related Caroline Tom parent))
    (axiom fact2 (related Caroline Ines parent))
    (axiom fact3 (related Dustin Tom parent))
    (axiom fact4 (related Dustin Ines parent))
    (axiom fact5 (related Tom Gloria parent))
    (axiom fact6 (related Ines Hildegard parent))
    (axiom fact7 (instance Tom Male))
    (axiom fact8 (instance Tom Parent))
    (axiom fact9 (instance Ines Female))
    (axiom fact10 (instance Ines Parent))
    
    (axiom o1 (define-primitive-role parent ancestor))
    
    (axiom o2 (define-concept Father (and Male Parent)))
    
    (axiom o3 (define-concept Mother (and Female Parent)))
    
    (axiom o4 (define-primitive-role (transitive-closure ancestor) ancestor))))
 

 ;(define kb2 (add-ontologies kb1 (list (axiom fact9 (instance Ines Female))
 ;                                        (axiom fact10 (instance Ines Parent)))))
 
 ;(define kb3 (add-ontologies kb2 (list (axiom o3 (define-concept Mother (and Female Parent))))))
 
 
 (define (engine max-nodes max-turns)
   (make-engine max-nodes max-turns 
                (list (generate-arguments-from-ontologies kb1 '()))))
 
 (define e1 (engine 100 10))
 
 (check (all-acceptable? '(parent ?x ?y) e1) => #t)
 (check (all-acceptable? '(ancestor ?x ?y) e1) => #t)
 (check (all-acceptable? '(ancestor Caroline ?y) e1) => #t)
 (check (all-acceptable? '(ancestor Caroline Tom) e1) => #t)
 (check (failure? '(parent Hildegard Tom) e1) => #t)
 (check (all-acceptable? '(ancestor Caroline Gloria) e1) => #t)
 (check (all-acceptable? '(applies ?r (parent ?x ?y)) e1) => #t)
 (check (all-acceptable? '(Father Tom) e1) => #t)
 (check (all-acceptable? '(Mother Ines) e1) => #t)
 
 (check-report)
 
 ; Example commands
 ; (ask '(goods item2) e1)
 ; (show '(goods item2) e1)
 
 
 )
