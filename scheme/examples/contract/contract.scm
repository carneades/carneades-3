#!r6rs

(import (except (rnrs base) assert)
        (prefix (rnrs lists) list:)
        (rnrs hashtables)
        (prefix (carneades lkif) lkif:)
        (carneades statement)
        (carneades argument)
        (carneades argument-diagram))


(define imports (lkif:import "contract.xml"))
; (define imports (lkif:import "contract-simplified.xml"))
(define texts (index-by-statement (list:filter text? imports)))

(define ag1 (car (list:filter argument-graph? imports)))

(define c1 (accept default-context (list 'deed)))
(define c2 (reject c1 (list 'email)))

(define (show ag context)
  (view* ag 
         context 
         (lambda (x) x)
         (lambda (s) 
           (let ((txt (hashtable-ref texts s #f)))
             (if (and txt (not (equal? (text-summary txt) "")))
                 (text-summary txt)
                 s)))))

; (view ag1 c1)
(diagram ag1 c2)