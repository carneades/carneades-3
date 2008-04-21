(module mccarty mzscheme
  
  ; Examples from "The Case for Explicit Exceptions", by L. Thorne McCarty and William W. Cohen
  
  (require (lib "argument-builtins.scm" "carneades"))
  (require (lib "rule.scm" "carneades"))
  (require (lib "shell.scm" "carneades"))
  (require (planet "test.ss" ("schematics" "schemeunit.plt")))
  (require (planet "text-ui.ss" ("schematics" "schemeunit.plt")))
  
  (define rb1 
    (rulebase
     
     (rule r1 
           (if (and (elephant ?x)
                    (unless (royal ?x)))
               (gray ?x)))
     
     (rule r2 
           (if (and (elephant ?x)
                    (royal ?x))
               (not (gray ?x))))
     
     (rule r3
           (if (native-speaker ?x Pa-Dutch)
               (born ?x America)))

     (rule r4
           (if (and (native-speaker ?x German)
                    (unless (native-speaker ?x Pa-Dutch)))
               (not (born ?x America))))
     
     (rule r5 
           (if (native-speaker ?x Pa-Dutch)
               (native-speaker ?x German)))
     
     (rule r6 (if (and (citizen ?x)
                       (crook ?y))
                  (not (like ?x ?y))))
     
;     (rule r7 (if (and (citizen ?x)
;                       (gullible ?x)
;                       (crook ?y)
;                       (elected ?y))
;                  (like ?x ?y)))
     
     (rule r7 (if (and (citizen ?x)
                       (gullible ?x)
                       (crook ?y)
                       (elected ?y))
                  (excluded r6 (not (like ?x ?y)))))
                  
     (rule* facts 
            (elephant clyde)
            (african clyde)
            (royal clyde)
            (elephant dumbo)
            (african dumbo)
            (native-speaker Herman Pa-Dutch)
            (native-speaker Fritz German)
            (citizen Fred)
            (citizen John)
            (gullible Fred)
            (crook Dick)
            (elected Dick)
            )
     
     )) ; end of rule base
  
  ; type question = excluded | priority | valid
  
  ; engine integer integer (list-of symbol) -> statement -> (stream-of argument-state)
  (define (engine max-nodes max-turns critical-questions)
    (make-engine max-nodes max-turns 
                 (list (generate-arguments-from-rules rb1 critical-questions) builtins)))
  
;  (define tests
;    (test-suite
;     "rule tests"
;     (test-true "fact" (all-acceptable? '(bird Tweety) (engine 20 1 null)))
; 
;     ))
;  
;   (test/text-ui tests)
  
  ; Example commands
  ; (ask '(goods item2) (engine 20 2 null))
  ; (show '(goods item2) (engine 20 2 '(priority)))
  
  ) ; end of rule-tests



