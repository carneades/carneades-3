(module prohibitions mzscheme
  
  (require "../../src/shell.scm")
  (require "../../src/argument-builtins.scm") 
  (require "../../src/rule.scm")
  
  (define rb1 
    (rulebase
     
     (rule r1 
           (if (and (killing ?x)
                    (unless (in-self-defence ?x)))
               (prohibited ?x)))
     
     (rule r2 
           (if (stealing ?x)                    
               (prohibited ?x)))
     
     (rule r3 (if (attack ?x) (not (in-self-defence ?x))))
     
     (rule* r4 (if (plagiarism ?x) (stealing ?x)))
     
     
     (rule* facts 
            (killing action1)
            (stealing action2)
            (killing action3)
            (attack action3)
            (killing action5)
            (in-self-defence action5)
            (not (in-self-defence action1))
            (plagiarism action4))
     
     
     )) ; end of rule base
  
  ; type question = excluded | priority | valid
  
  (define (engine max-nodes max-turns critical-questions)
    (make-engine max-nodes max-turns 
                 (list (generate-arguments-from-rules rb1 critical-questions)
                       builtins)))
  
  ; queries

  (ask '(prohibited ?x) (engine 50 2 null))
  ; (show '(prohibited ?x) (engine 50 2 null))
  
  )  