
(module vat mzscheme
  
  (require (prefix lkif: (lib "lkif.ss" "carneades")))
  (require (prefix e: (lib "evidence.ss" "carneades")))
  (require (lib "shell.ss" "carneades"))
  (require (lib "argument-builtins.ss" "carneades")) 
  (require (lib "rule.ss" "carneades"))
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 4)))
  (require (planet "text-ui.ss" ("schematics" "schemeunit.plt" 2)))
    
  (define vat
    (add-rules empty-rulebase 
               (lkif:import "vat.xml")))    
  
  (define witness (e:make-witness "Andras"))
  
  (define form1 
    (e:make-form 
     ; questions
     (list (e:make-question '_placeOfPayingVATFor 'symbol 'one "In which country did \"~v\" take place?")
           (e:make-question 'priceOf 'number 'one "What price was paid in the \"~v\" transaction?"))
     ; help text, in SXML format
     null))
  
  (define testimony (e:make-testimony witness (list form1)))
  
  ; type question = excluded | priority | valid
 
  (define (engine max-nodes max-turns critical-questions)
    (make-engine max-nodes max-turns 
                 (list (e:generate-arguments-from-testimony testimony) ; ask the user first
                       (generate-arguments-from-rules vat critical-questions)
                       builtins)))

  
  (test/text-ui
   (test-suite
    "vat"
    (test-true "q1" (all-acceptable? '(isLiableForPayingVATFor Selling Vendor) (engine 100 1 null)))
    (test-true "q2" (all-acceptable? '(placeOfPayingVATFor Selling Austria) (engine 100 1 null)))
    (test-true "q3" (all-acceptable? '(amountOfVATFor Selling ?x) (engine 100 1 null)))
    ))
  
  ; For q3, provide these answers to the questions asked:
  ; 1. In which country did "Selling" take place? 
  ; Answer: (all Austria)
  ; 2. What price was paid in the "Selling" transaction? 
  ; Answer: (all 100)
  
  ;  (ask '(amountOfVATFor Selling ?x) (engine 100 1 null))
  ;  (show1 '(isLiableForPayingVATFor Selling Vendor) (engine 100 1 null))
  
  ) ; vat module end