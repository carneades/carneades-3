#!r6rs

(import (rnrs base)
        (rnrs hashtables)
        (prefix (rnrs lists) list:)
        (prefix (carneades lkif) lkif:)
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

(define imports (lkif:import "kt-ex1.xml"))
(define texts (index-by-id (list:filter text? imports)))  
(define ag1 (car (list:filter argument:argument-graph? imports)))

(define witness (e:make-witness "Gerd"))

(define form1 
  (e:make-form 
   ; questions
   (list (e:make-question 'k473 'boolean 'one "Max 10% of nominal value?"))
   ; help text, in SXML format
   null))

(define testimony (e:make-testimony witness (list form1)))



; engine integer integer context -> statement -> (stream-of argument-state)
(define (engine max-nodes max-turns context)
  (make-engine* max-nodes max-turns context
                (list  (e:generate-arguments-from-testimony testimony) ; ask the user first
                       (generate-arguments-from-argument-graph ag1))))


(define c1 (argument:accept argument:default-context (list 'k473)))
(define e1 (engine 20 1 argument:default-context))


; view1: statement (statement -> (stream-of argument-state)) -> void
; view a diagram of argument graph of the first state in a stream of states.
(define (view1 query engine)
  (let ((str (engine query)))
    (if (not (stream-null? str)) 
        (let ((s (stream-car str)))
          (view* (state-arguments s)
                 (state-context s)
                 (lambda (x) x)
                 (lambda (stmt)
                   (let ((txt (hashtable-ref texts stmt #f)))
                     (if (and txt (not (equal? (text-summary txt) "")))
                         (text-summary txt)
                         stmt))))))))

;(check (all-acceptable? 'k473 e1) => #t)  ; max 10% of nominal value   
;(check (all-acceptable? 'k472 e1) => #t)  ; cash payment
;(check (all-acceptable? 'k470 e1) => #t)  ; counter-performance
;(check-report)


; Answers to provide to questions asked:
; 1. Max 10% of nominal value?
; Answer: (all #t)

(ask1 'k470 e1) 

