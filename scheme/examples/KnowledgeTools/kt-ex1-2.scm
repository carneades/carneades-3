#!r6rs

;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;                                                                 !
;   DOESN'T WORK YET:                                             !
;                                                                 !
;   In the lkif-file exist statements with different ids which    !
;   have the same atom. This is not allowed and leads in this     !
;   example to cyclic arguments.                                  !
;                                                                 !
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(import (rnrs)
        (carneades lkif2)
;        (rnrs hashtables)
;        (prefix (rnrs lists) list:)
;        (prefix (carneades lkif) lkif:)
;        (carneades statement)
        (carneades shell)
;        (carneades stream)
        (carneades argument-search)
        (prefix (carneades argument) argument:)
        (prefix (carneades evidence) e:)
;        (carneades argument-diagram)
        (carneades argument-from-arguments)
        (carneades lib srfi lightweight-testing))

(define null '())

(define import-data (lkif-import "kt-ex1-2.xml"))
(define graphs (lkif-data-argument-graphs import-data))

(define ag1 (car graphs))

(define witness (e:make-witness "Gerd"))

(define form1 
  (e:make-form
   ; id
   'form1
   ; questions
   (list (e:make-question 'k473 'boolean 'one "Max 10% of nominal value?"))
   ; help text, in SXML format
   null))

(define testimony (e:make-testimony witness (list form1)))

(define (engine max-nodes max-turns graph)
  (make-engine* max-nodes max-turns graph
                (list  (e:generate-arguments-from-testimony testimony) ; ask the user first
                       (generate-arguments-from-argument-graph ag1))))


(define e1 (engine 20 1 argument:empty-argument-graph))
  
;(check (all-in? 'k473 e1) => #t)  ; max 10% of nominal value   
;(check (all-in? 'k472 e1) => #t)  ; cash payment
;(check (all-in? 'k470 e1) => #t)  ; counter-performance
;(check-report)


; Answers to provide to questions asked:
; 1. Max 10% of nominal value?
; Answer: (all #t)

(ask1 'k470 e1)