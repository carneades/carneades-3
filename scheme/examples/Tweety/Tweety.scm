
#!r6rs
(import (except (rnrs) assert)
        (carneades argument)
        (carneades argument-diagram))


(define birds-fly 
  "Birds normally fly.") 

(define tweety-isa-bird 
  "Tweety is a bird.")

(define tweety-is-abnormal 
  "Tweety is an abnormal bird.")

(define tweety-isa-penguin
  "Tweety is a penguin.")

(define tweety-flies
  "Tweety can fly.")

(define-argument a1 (pro tweety-flies
                         (am birds-fly)
                         (pr tweety-isa-bird)
                         (ex tweety-is-abnormal)))

(define-argument a2
  (pro tweety-is-abnormal 
       (pr tweety-isa-penguin)))

(define-argument a3 (pro '(p a) (pr '(not (q a)))))
(define-argument a4 (con '(q a)))

(define ag1 (assert empty-argument-graph (list a1)))

(define ag2 (assert ag1 (list a2)))

(define c1 (accept default-context (list tweety-isa-penguin tweety-isa-bird)))

(define ag3 (assert empty-argument-graph (list a3 a4)))

; (view ag2 c1)

; (view ag3 default-context)