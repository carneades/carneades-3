#!r6rs

(import (rnrs base)
        (rnrs hashtables)
        (prefix (rnrs lists) list:)
        (prefix (carneades lkif2) lkif:)
        (carneades statement)
        (carneades shell)
        (carneades stream)
        (carneades argument-search)
        (prefix (carneades argument) argument:)
        (prefix (carneades evidence) e:)
        (carneades argument-diagram)
        (carneades argument-from-arguments)
        (carneades lib srfi lightweight-testing))

(define null '())

(define lkif-data (lkif:lkif-import "kt-ex1.xml"))
 
(define ag1 (car (lkif:lkif-data-argument-graphs lkif-data)))

(define witness (e:make-witness "Gerd"))

(define form1 
  (e:make-form 
   'form1
   ; questions
   (list (e:make-question 'k473 'boolean 'one "Max 10% of nominal value?"))
   ; help text, in SXML format
   null))

(define testimony (e:make-testimony witness (list form1)))

; engine integer integer argument-graph -> statement -> (stream-of argument-state)
(define (engine max-nodes max-turns ag)
  (make-engine* max-nodes max-turns ag
                (list  (e:generate-arguments-from-testimony testimony) ; ask the user first
                       (generate-arguments-from-argument-graph ag1))))


(define c1 (argument:accept argument:empty-argument-graph (list 'k473)))
(define e1 (engine 20 1 argument:empty-argument-graph))

;(check (succeed? 'k473 e1) => #t)  ; max 10% of nominal value   
;(check (succeed? 'k472 e1) => #t)  ; cash payment
;(check (succeed? 'k470 e1) => #t)  ; counter-performance
;(check-report)


; Answers to provide to questions asked:
; 1. Max 10% of nominal value?
; Answer: (all #t)

(ask1 'k470 e1) 
; (show1 'k470 e1) 
