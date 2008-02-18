(module error_test mzscheme
  
 (require "../../src/shell.scm")
  (require "../../src/argument-builtins.scm") 
  (require "../../src/rule.scm")
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 4)))
  (require (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2)))
  
  (define rb1
    
    (rulebase
     
     ;--------------------------------------------   
     ; article 2 : beneficial owner
     ;--------------------------------------------
     
     (rule §-2-1-pa1-EUSD
           (if (and (individual ?x)
                    (interest-payment ?c)
                    (receives ?x ?c))
               (beneficial-owner ?x)))
     
     
     ;--------------------------------------------   
     ; article 4 : paying agent
     ;--------------------------------------------
     
     (rule §-4-1-001-EUSD
           (if (and (economic-operator ?x)
                    (beneficial-owner ?y)
                    (pays-interest-for-the-benefit-of ?x ?y)
                    (debt-claim-that-produces-interest ?c)
                    (debtor-of ?x ?c)
                    (has-debt-with ?x ?y))
               (paying-agent ?x)))
     
     
     ;--------------------------------------------   
     ; article 6: interest payment
     ;--------------------------------------------
        
     
    
     (rule §-6-1-a003-EUSD
           (if (income-from-government-securities ?x)
               (interest-payment-following6-1-a ?x)))
          
     (rule §-6-1-a004-EUSD
           (if (income-from-bonds ?x)
               (interest-payment-following6-1-a ?x)))
 
     (rule §-6-1-a007-EUSD
           (if (interest-payment-following6-1-a ?x)
               (interest-payment ?x))) 
     
     ;--------------------------------------------   
     ; article 7: covering by directive
     ;--------------------------------------------
     
     (rule §-7-EUSD
           (if (and (interest-payment ?x)
                    (paying-agent ?y)
                    (pays ?y ?x)
                    (territory-of-the-treaty ?t)
                    (established-in ?y ?t))
               (covered-by-directive ?x)))
     
     
     ;--------------------------------------------   
     ; rule facts
     ;--------------------------------------------
     
     (rule* facts
            ; article 2 (beneficial owner)
            (individual Brown)
            (individual Green)
            (receives Brown money-item1)
            ; article 4 (paying agent)
            (economic-operator Alfabank)
            (pays-interest-for-the-benefit-of Alfabank Brown)
            (debt-claim-that-produces-interest debt-item1)
            (debtor-of Alfabank debt-item1)
            (has-debt-with Alfabank Brown)
            (income-from-bonds money-item1)
            ; article 7 (covering by directive)
            (pays Alfabank money-item1)
            (territory-of-the-treaty England)
            (established-in Alfabank England)
            ; (income-from-government-securities ?x)
            )
     
     
     ))
  
  
  ; type question = excluded | priority | valid
  
  ; engine integer integer (list-of symbol) -> statement -> (stream-of argument-state)
  (define (engine max-nodes max-turns critical-questions)
    (make-engine max-nodes max-turns 
                 (list (generate-arguments-from-rules rb1 critical-questions) 
                       ; builtins
                       )))
  
  
  
;  ; queries
;  (define q1 (find 80 1 '(covered-by-directive ?x) null)) ; find
;  (define q2 (find 80 2 '(interest-payment-following6-1-a ?x) null))
;  (define q3 (find 80 2 '(interest-payment ?x) null))
  
  
  (define tests
    (test-suite
     "Italian error test"
     (test-true "q1" (all-acceptable? '(covered-by-directive ?x) (engine 80 1 null)))
     (test-true "q2" (all-acceptable? '(interest-payment-following6-1-a ?x) (engine 80 2 null)))
     (test-true "q3" (all-acceptable? '(interest-payment ?x) (engine 80 2 null)))
     ))
  
  (test/text-ui tests)
  
  ; (ask '(interest-payment ?x) (engine 80 2 null))
  ; (show '(covered-by-directive ?x) (engine 80 1 null))
  
  
  ; (view-first-state (find 80 2 '(covered-by-directive ?x) null))
  
  ) ; end of rule-tests

