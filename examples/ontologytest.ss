;#;(module ontologytest mzscheme
;  
;  (require (lib "dlp.ss" "carneades"))
;  (require (lib "shell.ss" "carneades"))
;  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 4)))
;  (require (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2)))
;  
;  (define kb1
;    (knowledgebase
;     
;     (ontology 'fact1 '(related Caroline Tom parent))
;     (ontology 'fact2 '(related Caroline Ines parent))
;     (ontology 'fact3 '(related Dustin Tom parent))
;     (ontology 'fact4 '(related Dustin Ines parent))
;     (ontology 'fact5 '(related Tom Gloria parent))
;     (ontology 'fact6 '(related Ines Hildegard parent))
;     
;     (ontology 'o1 '(define-primtive-role parent ancestor))
;     
;     (ontology 'o2 '(define-primitive-role (transitive-closure ancestor) ancestor))))
;  
;  (define (engine max-nodes max-turns)
;    (make-engine max-nodes max-turns 
;                 (list (generate-arguments-from-ontologies kb1 '()))))
;  
;  (define e1 (engine 50 1))       
;  
;  (test/text-ui
;    (test-suite
;     "Tests for ancestor.scm"
;     (test-true "test 1" (all-acceptable? '(parent ?x ?y) e1))
;     (test-true "test 2" (all-acceptable? '(ancestor ?x ?y) e1))
;     (test-true "test 3" (all-acceptable? '(ancestor Caroline ?y) e1))
;     (test-true "test 4" (all-acceptable? '(ancestor Caroline Tom) e1))
;     (test-true "test 5" (failure? '(parent Hildegard Tom) e1))
;     (test-true "test 6" (all-acceptable? '(ancestor Caroline Gloria) e1))
;     (test-true "test 7" (all-acceptable? '(applies ?r (parent ?x ?y)) e1))
;     (test-true "test 8" (all-acceptable? '(applies facts (parent Caroline Tom)) e1))))
;  
;  
;  ;(test/text-ui tests)
;  
;  
;  )

#!r6rs

(library (ontologytest)
         
         (export)
         
         (import (rnrs) 
                 (carneades dlp)
                 (carneades shell)
                 (carneades argument-builtins)
                 (carneades rule)
                 ;(planet test)
                 (srfi/78 check)
                 )
         
         
         (define kb1
           (knowledgebase
            
            (ontology 'fact1 '(related Caroline Tom parent))
            (ontology 'fact2 '(related Caroline Ines parent))
            (ontology 'fact3 '(related Dustin Tom parent))
            (ontology 'fact4 '(related Dustin Ines parent))
            (ontology 'fact5 '(related Tom Gloria parent))
            (ontology 'fact6 '(related Ines Hildegard parent))
            
            (ontology 'o1 '(define-primitive-role parent ancestor))
            
            (ontology 'o2 '(define-primitive-role (transitive-closure ancestor) ancestor))))
         
         (define (engine max-nodes max-turns)
           (make-engine max-nodes max-turns 
                        (list (generate-arguments-from-ontologies kb1 '()))))
         
         (define e1 (engine 50 2))
         
         #;(test/text-ui
            (test-suite
             "Tests for ancestor.scm"
             (test-true "test 1" (all-acceptable? '(parent ?x ?y) e1))
             (test-true "test 2" (all-acceptable? '(ancestor ?x ?y) e1))
             (test-true "test 3" (all-acceptable? '(ancestor Caroline ?y) e1))
             (test-true "test 4" (all-acceptable? '(ancestor Caroline Tom) e1))
             (test-true "test 5" (failure? '(parent Hildegard Tom) e1))
             (test-true "test 6" (all-acceptable? '(ancestor Caroline Gloria) e1))
             (test-true "test 7" (all-acceptable? '(applies ?r (parent ?x ?y)) e1))
             (test-true "test 8" (all-acceptable? '(applies facts (parent Caroline Tom)) e1))))
         
         (check (all-acceptable? '(parent ?x ?y) e1) => #t)
         (check (all-acceptable? '(ancestor ?x ?y) e1) => #t)
         (check (all-acceptable? '(ancestor Caroline ?y) e1) => #t)
         (check (all-acceptable? '(ancestor Caroline Tom) e1) => #t)
         (check (failure? '(parent Hildegard Tom) e1) => #t)
         (check (all-acceptable? '(ancestor Caroline Gloria) e1) => #t)
         (check (all-acceptable? '(applies ?r (parent ?x ?y)) e1) => #t)
         (check (all-acceptable? '(applies facts (parent Caroline Tom)) e1) => #t)
         
         (check-report)
         
         ; Example commands
         ; (ask '(goods item2) e1)
         ; (show '(goods item2) e1)
  
         
         )
