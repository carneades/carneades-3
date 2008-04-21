(module goods mzscheme
    
  (require (prefix lkif: (lib "lkif.scm" "carneades")))
  (require (lib "shell.scm" "carneades"))
  (require (lib "argument-builtins.scm" "carneades"))
  (require (lib "rule.scm" "carneades"))
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 4)))
  (require (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2)))
  
  
  (define goods 
    (add-rules empty-rulebase 
               (lkif:import "goods.xml")))
  
  ; type question = excluded | priority | valid
  
  (define (engine max-nodes max-turns critical-questions)
    (make-engine max-nodes max-turns 
                 (list (generate-arguments-from-rules goods critical-questions)
                       builtins)))
   
  (test/text-ui
   (test-suite
    "goods"
    (test-true "q1" (all-acceptable? '(Goods g1) (engine 20 1 null)))
    (test-true "q2" (not (all-acceptable? '(Goods g1) (engine 20 2 null))))
    ))
  
  (ask '(Goods g1) (engine 20 2 null))
  
  ) ; module end