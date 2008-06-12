(module ancestor mzscheme
  
  (require (lib "rule.ss" "carneades"))
  (require (lib "shell.ss" "carneades"))
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 4)))
  (require (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2)))
  
  (define rb1 
    (rulebase
      
     (rule* facts 
            (parent Caroline Tom) 
            (parent Caroline Ines)
            (parent Dustin Tom)
            (parent Dustin Ines)
            (parent Tom Gloria)
            (parent Ines Hildegard))
  
     (rule* r1 
            (if (parent ?x ?y)
                (ancestor ?x ?y)))   
     
     (rule* r2 
            (if (and (parent ?x ?z) 
                     (ancestor ?z ?y))
                (ancestor ?x ?y)))
     
     )) ; rulebase
     
    ; engine integer integer  -> statement -> (stream-of argument-state)
  (define (engine max-nodes max-turns)
    (make-engine max-nodes max-turns 
          (list (generate-arguments-from-rules rb1 null))))
  
  (define e1 (engine 50 1))
  
  
  (define tests
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
  
  
  (test/text-ui tests)
  
  ) ; end module