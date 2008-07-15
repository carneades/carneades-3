(module family-support-old mzscheme
    
  (require (prefix lkif: "../../src/lkif.scm"))
  (require "../../src/argument-builtins.scm") 
  (require "../../src/rule.scm")
  (require (prefix e:"../../src/evidence.scm"))
  (require "../../src/shell.scm")

  (define family-support 
    (add-rules empty-rulebase 
               (lkif:import "family-support.xml")))    
  
  (define witness (e:make-witness "Radboud Winkels"))
  
  (define form1 
    (e:make-form 
     ; questions
     (list (e:make-question 'mother 'symbol 'one "Who is ~v's mother?")
           (e:make-question 'father 'symbol 'one "Who is ~v's father?"))
     ; help text, in SXML format
     null))
  
  (define form2
    (e:make-form
     ; questions
     (list (e:make-question 'needy 'boolean 'one "Is ~v needy?"))
     ; help text
     null))
    
  (define form3
    (e:make-form
     ; questions
     (list (e:make-question 
            'capacity-to-provide-support 
            'boolean 
            'one 
            "Does ~v have the capacity to provide support?"))
     ; help text
     null))
 
  (define testimony (e:make-testimony witness (list form1 form2 form3)))
    
  ; type question = excluded | priority | valid
  
  ; engine integer integer (list-of symbol) -> statement -> (stream-of argument-state)
  (define (engine max-nodes max-turns critical-questions)
    (make-engine max-nodes max-turns 
                 (list (e:generate-arguments-from-testimony testimony) ; ask the user first
                       (generate-arguments-from-rules family-support critical-questions)
                       builtins)))
    
  
  (define e1 (engine 20 2 null))
  (ask '(obligated-to-support Radboud ?x) e1)
;  (show '(obligated-to-support Radboud ?x) e1)

  
 
  ; Answers to provide to questions asked:
  ; 1. Who is Radboud's mother? 
  ; Answer: (all Maxine)
  ; 2. Who is Radboud's father? 
  ; Answer: (all Heribert)
  ; 3. Is Maxine needy? 
  ; Answer: (all #t)
  ; Does Radboud have the capacity to provide support?
  ; Answer: (all #t)

  ) ; module end 