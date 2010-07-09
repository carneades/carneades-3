(ns carneades.examples.recycling
(:use carneades.engine.argument
      carneades.engine.statement
      carneades.engine.argument-from-arguments
      carneades.engine.shell
      carneades.engine.rule
      carneades.mapcomponent.viewer))

; The recycling example used by Adam Wyner 
; Also illustrates the argument-from-arguments module.

(def p1 "Every household should pay some tax for the household's garbage.")
; p2 is the complement of p1
(def p3 "Every household which pays some tax for the household's garbage
increases an amount of the household's garbage which the household recycles.")
(def p4 "If a household increases an amount of the household's garbage
which the household recycles then the household benefits the household's 
society.")
(def p5 "If a household pays a tax for the household's garbage then the tax
is unfair to the household.")
(def p6 "Every household should pay an equal portion of the sum of the tax 
for the household's garbage.")
(def p7 "No household which receives a benefit which is paid by a council
recycles the household's garbage.")
(def p8 "Every household which does not receive a benefit which is paid
by a council supports a household which receives a benefit which is paid by
the council.")
(def p9 "Tom says that every household which recycles the household's
garbage reduces a need of a new dump which is for the garbage.")
(def p10 "Every household which reduces a need of a new dump benefits
society.")
; p11 is the complement of p18
(def p12 "Tom owns a company that recycles some garbage.")
(def p13 "Every person who owns a company that recycles some garbage earns
some money from the garbage which is recycled.")
(def p14 "Every supermarket creates some garbage.")
(def p15 "Every supermarket should pay a tax for the garbage that the
supermarket creates.")
(def p16 "Every tax which is for some garbage which the supermarket
creates is passed by the supermarket onto a household.")
; p17 is the complement of p15
(def p18 "Tom is an objective expert about recycling.")
(def p19 "If an objective expert says every household which recycles
the household's garbage reduces a need of a new dump which is for the 
garbage, then every household which recycles the household's garbage
reduces a need of a new dump which is for the garbage.")


(defargument a1 (pro p1 (pm p4) (pm p3)))
(defargument a2 (pro p4 (pm p9) (pm p18) (pm p19)))
(defargument a3 (pro p3 (pm p10)))
(defargument a4 (con p18 (pm p12) (pm p13)))
(defargument a5 (con p1 (pm p5)))
(defargument a6 (pro p5 (pm p6) (pm p7) (pm p8)))
(defargument a7 (con p1 (pm p15)))
(defargument a8 (pro p15 (pm p14)))
(defargument a9 (con p15 (pm p16)))

(def args1
     (assert-arguments *empty-argument-graph*
                    [a1 a2 a3 a4 a5 a6 a7 a8 a9]))

;; every supermarket creates some garbage
(def args2 (accept *empty-argument-graph* [p14]))

(def e1 (make-engine* 100 2 args2
                      [(generate-arguments-from-argument-graph args1)]))

(view (:arguments (first (e1 p15))))
