#!r6rs

(import (rnrs)
        (carneades lkif2)
        (prefix (carneades argument)  argument:)
        (carneades argument-builtins)
        (carneades rule)
        (carneades evidence)
        (carneades case)
        (carneades shell))

(define family-support 
  (lkif-data-rulebase (lkif-import "family-support2.xml")))    


;; undue hardship cases

; factors

(define f1
  (make-factor 'has-already-provided-much-support 'plaintiff #f))

(define f2
  (make-factor 'expected-duration-of-support-is-short 'defendant #f))

(define f3
  (make-factor 'never-had-parent-child-relationship 'plaintiff #f))

(define f4 
  (make-factor 'would-cause-irrepairable-harm-to-family 'plaintiff #f))

(define f5
  (make-factor 'has-not-provided-care 'defendant #f))

; cases
(define müller (make-case "Müller" 'plaintiff (list f3)))
(define schmidt (make-case "Schmidt" 'defendant (list f2 f3)))
(define bauer (make-case "Bauer" 'plaintiff (list f2 f3 f4)))

(define undue-hardship-casebase
  (make-casebase 
   'undue-hardship
   (list f1 f2 f3 f4 f5) ; factors
   (list müller schmidt bauer))) ; cases

; Testimony of the respondent

(define witness1 (make-witness "Respondent"))

(define form1 
  (make-form 
   ; questions
   (list (make-question 'mother 'symbol 'one "Who is ~a's mother?")
         (make-question 'father 'symbol 'one "Who is ~a's father?"))
   ; help text, in SXML format
   '()))

(define form2
  (make-form
   ; questions
   (list (make-question 'needy 'boolean 'one "Is ~a needy?"))
   ; help text
   '()))

(define form3
  (make-form
   ; questions
   (list (make-question 
          'capacity-to-provide-support 
          'boolean 
          'one 
          "Does ~a have the capacity to provide support?"))
   ; help text
   '()))

(define form4
  (make-form
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

; bridging rules, from the rulebase to the casebase

(define bridge-rules 
  (rulebase
   
   (rule hardship-bridge
         (if undue-hardship
             (undue-hardship ?x (obligated-to-support ?x ?y))))))


; type critical-question = excluded | priority | valid

(define argument-generators
  (list (generate-arguments-from-testimony testimony) ; ask the user first
        (generate-arguments-from-rules family-support '(excluded))
        (generate-arguments-from-rules bridge-rules '())
        (generate-arguments-from-cases undue-hardship-casebase)
        builtins))


; 1. Find the best arguments about whether Max is obligated to support Gertrude, without considering the
; issue of undue hardship

(define e1 (make-engine 50 2 argument-generators))

(show1 '(obligated-to-support Max ?y) e1)

; Answers to provide to questions asked:
; 1. Who is Max's mother? 
; Answer: (all Gertrude)
; 2. Who is Max father? 
; Answer: (all Heribert)
; 3. Is Gertrude needy? 
; Answer: (all #t)
; Does Radboud have the capacity to provide support?
; Answer: (all #t)

; 2. Collect the information about Max needed for using case-based reasoning, by asking the user

(get-answer testimony
            (get-question testimony (factor-statement f1))
            #t)

; Answer (all #t) to questions 2-4.  Answer all other questions with (all #f).

; 3. Construct a context, c2, in which the information collected is assumed to be true.

(define c2 (argument:accept argument:default-context (testimony-statements testimony)))

; 4. Using this context, search again for the best arguments about Max's support obligations, to see
; if the undue hardship exception now applies.

(define e2 (make-engine* 50 4 c2 argument-generators))
(ask1 '(obligated-to-support Max ?y) e2)

; end of file