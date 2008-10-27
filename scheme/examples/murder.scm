
#!r6rs

(import (rnrs base)
        (carneades statement)
        (carneades argument)
        (carneades argument-diagram)
        (prefix (carneades table) table:)
        (carneades unify))

(define null '())

(define murder "murder")

(define r187
  "Murder is killing 
with malice aforethought.")

(define r187-valid "§187 is valid.")

(define r187-excluded "§187 excluded.")

(define killing "killing")

(define malice "malice")

(define r2
  "Killing justified if in self-defense.")

(define r197-valid "§197 is valid.")

(define r197-excluded "§197 is excluded.")

(define self-defense "self-defense")

(define f1 "W1 testified 'attack'.")

(define r3
  "If W1 testified 'attack.'
then attack.")

(define w1-not-credible "W1 not credible.")

(define f2
  "Witness W2 testified
'time to run away'")

(define r6
  "If witness W2 testified 'time to run away'
then time to run away.")

(define w2-not-credible "W2 is not credible.")

(define-argument a1 
  (pro murder (pr killing) (pr malice) (am r187-valid) (ex r187-excluded)))

(define-argument a2 
  (pro r187-excluded (pr self-defense) (am r197-valid) (ex r197-excluded)))

(define-argument a3
  (pro self-defense (pr f1) (ex w1-not-credible)))

(define-argument a4
  (con self-defense (pr f2) (ex w2-not-credible)))

(define c0 
  (make-context (table:make-table) ; status table
                (lambda (statement) 'se) ; default standard: scintilla
                (lambda (arg1 arg2) 0) ; no priorities
                identity))

(define ag1 (assert-argument empty-argument-graph (list a1)))
(define c1 (accept c0 (list killing malice)))

(define ag2 (assert-argument ag1 (list a2 a3)))
(define c2 (accept c1 (list f1)))

; problem: the defendant's argument also met the BA standard,
; since there are no counterarguments.  So this doesn't illustrate
; the need to *raise* the proof standard after the burden of
; production has been met, just, if to change it the complement
; when the "tactical burden" is to be switch back to the other
; party, as in the next step below.

(define ag3 (assert-argument ag2 (list a4)))
(define c3 (accept c2 (list f2)))
(set! c3 (assign-standard c3 self-defense (list 'n~ba)))


;(diagram ag1 c1)
; (view ag2 c2)
(diagram ag3 c3)
