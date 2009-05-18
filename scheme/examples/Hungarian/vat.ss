#!r6rs

(import (rnrs)
        (carneades base)
        (carneades lkif2)
        (prefix (carneades evidence) e:)
        (carneades argument-builtins)
        (carneades rule)
        (carneades shell)
        (carneades lib srfi lightweight-testing))


(define import-data (lkif-import "vat.xml"))

(define vat (lkif-data-rulebase import-data))

(define witness (e:make-witness "Andras"))

(define form1 
  (e:make-form 
   ; id
   'form1
   ; questions
   (list (e:make-question 'placeOfPayingVATFor 'symbol 'one "In which country did \"~A\" take place?")
         (e:make-question 'priceOf 'number 'one "What price was paid in the \"~A\" transaction?"))
   ; help text, in SXML format
   null))

(define testimony (e:make-testimony witness (list form1)))

; type question = excluded | priority | valid

(define (engine max-nodes max-turns critical-questions)
  (make-engine max-nodes max-turns 
               (list (e:generate-arguments-from-testimony testimony) ; ask the user first
                     (generate-arguments-from-rules vat critical-questions)
                     builtins)))


(check (succeed? '(isLiableForPayingVATFor Selling Vendor) (engine 100 1 null)) => #t)
(check (succeed? '(placeOfPayingVATFor Selling Austria) (engine 100 1 null)) => #t)
(check (succeed? '(amountOfVATFor Selling ?x) (engine 100 1 null)) => #t)
(check-report)

; For q3, provide these answers to the questions asked:
; 1. In which country did "Selling" take place? 
; Answer: (all Austria)
; 2. What price was paid in the "Selling" transaction? 
; Answer: (all 100)

;(ask '(amountOfVATFor Selling ?x) (engine 100 1 null))
;(ask '(isLiableForPayingVATFor Selling Vendor) (engine 100 1 null))