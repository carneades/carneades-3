#!r6rs

(import (rnrs)
        (carneades lkif2)
        (prefix (carneades argument)  argument:)
        (carneades argument-builtins)
        (carneades rule)
        (carneades evidence)
        (carneades case)
        (carneades shell)
        (carneades dlp))

(ontology family-relations
          
          (define-primitive-role family:ancestor family:relative)
          (define-primitive-role family:parent family:ancestor)
          (define-primitive-role family:ancestor family:descendent)
          (define-primitive-role family:mother family:parent)
          (define-primitive-role family:father family:parent)
          (define-primitive-role family:sibling family:relative)
          (define-primitive-role family:brother family:sibling)
          (define-primitive-role family:sister family:sibling))

(define family-data
  (lkif-import "family-support.xml"))

(define family-support 
  (lkif-data-rulebase family-data))


;; undue hardship cases

; factors

(define f1
  (make-factor 'family:hasAlreadyProvidedMuchSupport 'plaintiff #f))

(define f2
  (make-factor 'family:expectedDurationOfSupportIsShort 'defendant #f))

(define f3
  (make-factor 'family:neverHadParentChildRelationship 'plaintiff #f))

(define f4 
  (make-factor 'family:wouldCauseIrrepairableHarmToFamily 'plaintiff #f))

(define f5
  (make-factor 'family:hasNotProvidedCare 'defendant #f))

; cases
(define müller (make-case "Müller" 'plaintiff (list f3)))
(define schmidt (make-case "Schmidt" 'defendant (list f2 f3)))
(define bauer (make-case "Bauer" 'plaintiff (list f2 f3 f4)))

(define undue-hardship-casebase
  (make-casebase 
   'family:undueHardship
   (list f1 f2 f3 f4 f5) ; factors
   (list bauer schmidt müller))) ; cases

; Testimony of the respondent

(define witness1 (make-witness "Respondent"))

(define form1 
  (make-form 
   ; id
   'parents
   ; questions
   (list (make-question 'family:mother 'symbol 'one "Who is ~a's mother?")
         (make-question 'family:father 'symbol 'one "Who is ~a's father?"))
   ; help text, in SXML format
   '()))

(define form2
  (make-form
   ; id
    'neediness
   ; questions
   (list (make-question 'family:needy 'boolean 'one "Is ~a needy?"))
   ; help text
   '()))

(define form3
  (make-form
   ; id
   'capacity
   ; questions
   (list (make-question 
          'family:hasCapacityToSupport 
          'boolean 
          'one 
          "Does ~a have the capacity to provide support?"))
   ; help text
   '()))

(define form4
  (make-form
   'undue-hardship
   (list 
    (make-question 
     (factor-statement f1)
     'boolean
     'one
     "Has much support already been provided?")
    (make-question 
     (factor-statement f2)
     'boolean
     'one
     "Would support be provided for only a short period of time?")
    (make-question
     (factor-statement f3)
     'boolean
     'one
     "Is it true that the respondent never had a close relationship with the beneficiary?")
    (make-question
     (factor-statement f4)
     'boolean
     'one
     "Would requiring respondent to provide support cause irreparable harm to his family?")
    (make-question
     (factor-statement f5)
     'boolean
     'one
     "Is it true that the respondent has never provided care to the beneficiary?")
    )
   ; help text
   '()))

(define testimony (make-testimony witness1 (list form1 form2 form3 form4)))
; (define testimony (make-testimony witness1 (list form3 form4)))


; bridging rules, from the rulebase to the casebase

(define bridge-rules 
  (rulebase
   
   (rule hardship-bridge
         (if family:undueHardship
             (family:undueHardship (family:obligatedToSupport ?x ?y) ?x)))))


; type critical-question = excluded | priority | valid


(define argument-generators
  (list builtins
        (generate-arguments-from-ontology family-relations '())
        (generate-arguments-from-rules family-support '(excluded))
        (generate-arguments-from-testimony testimony) ; ask the user 
        (generate-arguments-from-rules bridge-rules '())
        (generate-arguments-from-cases undue-hardship-casebase)))


; 1. Find the best arguments about whether Tom is obligated to support Gloria, without considering the
; issue of undue hardship

(define e1 (make-engine* 50 2 argument:empty-argument-graph argument-generators))

(show1 '(family:obligatedToSupport Max ?y) e1)

; Answers to provide to questions asked:
; 1. Who is Max's mother? 
; Answer: (all Gertrude)
; 2. Who is Max's father? 
; Answer: (all Heribert)
; 3. Is Gertrude needy? 
; Answer: (all #t)
; Does Masx have the capacity to provide support?
; Answer: (all #t)

; 2. Collect the information about Max needed for using case-based reasoning, by asking the user

(get-answer testimony
            (get-question testimony (factor-statement f1))
            #t)

; Answer (all #t) to questions 2-4.  Answer all other questions with (all #f).

; 3. Construct a context, c2, in which the information collected is assumed to be true.

(define ag1 (argument:accept argument:empty-argument-graph (testimony-statements testimony)))

; 4. Using this context, search again for the best arguments about Max's support obligations, to see
; if the undue hardship exception now applies.

(define e2 (make-engine* 50 4 ag1 argument-generators))
(show1 '(family:obligatedToSupport Max ?y) e2)




; end of file