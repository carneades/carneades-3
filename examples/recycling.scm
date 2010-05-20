#!r6rs

(import (rnrs base)
        (carneades base)
        (carneades statement)
        (carneades argument)
        (carneades abduction)
        (carneades argument-diagram)
        (prefix (carneades table) table:)
        (carneades unify)
        (carneades argument-from-arguments)
        (carneades shell)
        (carneades argument-search)
        (carneades stream))

; The recycling example used by Adam Wyner 
; Also illustrates the argument-from-arguments module.

(define p1 "Every household should pay some tax for the household's garbage.")
; p2 is the complement of p1
(define p3 "Every household which pays some tax for the household's garbage
increases an amount of the household's garbage which the household recycles.")
(define p4 "If a household increases an amount of the household's garbage
which the household recycles then the household benefits the household's 
society.")
(define p5 "If a household pays a tax for the household's garbage then the tax
is unfair to the household.")
(define p6 "Every household should pay an equal portion of the sum of the tax 
for the household's garbage.")
(define p7 "No household which receives a benefit which is paid by a council
recycles the household's garbage.")
(define p8 "Every household which does not receive a benefit which is paid
by a council supports a household which receives a benefit which is paid by
the council.")
(define p9 "Tom says that every household which recycles the household's
garbage reduces a need of a new dump which is for the garbage.")
(define p10 "Every household which reduces a need of a new dump benefits
society.")
; p11 is the complement of p18
(define p12 "Tom owns a company that recycles some garbage.")
(define p13 "Every person who owns a company that recycles some garbage earns
some money from the garbage which is recycled.")
(define p14 "Every supermarket creates some garbage.")
(define p15 "Every supermarket should pay a tax for the garbage that the
supermarket creates.")
(define p16 "Every tax which is for some garbage which the supermarket
creates is passed by the supermarket onto a household.")
; p17 is the complement of p15
(define p18 "Tom is an objective expert about recycling.")
(define p19 "If an objective expert says every household which recycles
the household's garbage reduces a need of a new dump which is for the 
garbage, then every household which recycles the household's garbage
reduces a need of a new dump which is for the garbage.")


(define-argument a1 (pro p1 (pr p4) (pr p3)))
(define-argument a2 (pro p4 (pr p9) (pr p18) (pr p19)))
(define-argument a3 (pro p3 (pr p10)))
(define-argument a4 (con p18 (pr p12) (pr p13)))
(define-argument a5 (con p1 (pr p5)))
(define-argument a6 (pro p5 (pr p6) (pr p7) (pr p8)))
(define-argument a7 (con p1 (pr p15)))
(define-argument a8 (pro p15 (pr p14)))
(define-argument a9 (con p15 (pr p16)))

(define args1
  (assert-arguments empty-argument-graph
          (list a1 a2 a3 a4 a5 a6 a7 a8 a9)))

; (define facts1 (list ...))
; (define arg2 (accept args1 facts1))

(define in-label (statement-in-label args1 '() p1))
(define comp-in-label (statement-in-label args1 '() `(not ,p1)))

(define args2 (accept empty-argument-graph (list p14))) ; every supermarket creates some garbage

(define e1
  (make-engine* 100 2 args2
                (list (generate-arguments-from-argument-graph args1))))

; (view args1)
(show1 p15 e1)
