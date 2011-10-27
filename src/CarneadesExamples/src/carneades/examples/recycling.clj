;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.examples.recycling
(:use carneades.engine.argument
      ; carneades.engine.statement
      carneades.engine.shell
      ; carneades.engine.rule
      carneades.mapcomponent.viewer))

; The recycling example used by Adam Wyner 

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


(def a1 (make-argument :id 'a1 :conclusion p1 :premises [p4, p3]))
(def a2 (make-argument :id 'a2 :conclusion p4 :premises [p9, p18, p19]))
(def a3 (make-argument :id 'a3 :conclusion p3 :premises [p10]))
(def a4 (make-argument :id 'a4 :conclusion (¬ p18) :premises [p12, p13]))
(def a5 (make-argument :id 'a5 :conclusion (¬ p1) :premises [p5]))
(def a6 (make-argument :id 'a6 :conclusion p5 :premises [p6, p7, p8]))
(def a7 (make-argument :id 'a7 :conclusion (¬ p1) :premises [p15]))
(def a8 (make-argument :id 'a8 :conclusion p15 :premises [p14]))
(def a9 (make-argument :id 'a9 :conclusion (¬ p15) :premises [p16]))

(def ag
    (-> (argument-graph)
        (assert-arguments [a1 a2 a3 a4 a5 a6 a7 a8 a9])
        (accept [p14])))

(view ag)
