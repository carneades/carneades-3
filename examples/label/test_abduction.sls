#!r6rs

(import (rnrs)
        (carneades argument)
        (carneades argument-diagram)
        (carneades abduction)
        (carneades statement))

(define u "u")
(define v "v")
(define t "t")
(define w "w")
(define r "r")
(define s "s")
(define q "q")
(define p "p")

(define-argument a4 (pro r
                         (pr u)
                         (ex v)))
(define a3 (make-argument 'a3 #f 0.6 'con r (list (pr t)) ""))
(define-argument a5 (con r
                         (pr w)))
(define a1 (make-argument 'a1 #f 0.6 'con p (list (pr r)) ""))
(define a2 (make-argument 'a2 #f 0.4 'pro p (list (pr s) (pr q)) ""))

(define ag (assign-standard (assert-arguments (reject empty-argument-graph (list s w))
                                             (list a1 a2 a3 a4 a5))
                           'ba
                           (list u v t w r s q p)))

(define asm (list (statement-complement q)
                  (statement-complement v)
                  (statement-complement t)
                  w
                  s))


(display (statement-in-label ag asm p))
