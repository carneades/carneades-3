;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.examples.recycling
  (:use carneades.engine.statement
        carneades.engine.argument
        carneades.engine.argument-graph
        ; carneades.mapcomponent.viewer
        ))

; The recycling example used by Adam Wyner 

(def p1 
  (make-statement :text {:en "Every household should pay some tax for the household's garbage."}))

(def p2 (¬ p1))

(def p3 
  (make-statement :text {:en "Every household which pays some tax for the household's garbage increases an amount of the household's garbage which the household recycles."}))

(def p4 
  (make-statement :text {:en "If a household increases an amount of the household's garbage which the household recycles then the household benefits the household's society."}))

(def p5 
  (make-statement :text {:en "If a household pays a tax for the household's garbage then the tax is unfair to the household."}))

(def p6 
  (make-statement :text {:en "Every household should pay an equal portion of the sum of the tax for the household's garbage."}))

(def p7 
  (make-statement :text {:en "No household which receives a benefit which is paid by a council recycles the household's garbage."}))

(def p8 
  (make-statement :text {:en "Every household which does not receive a benefit which is paid by a council supports a household which receives a benefit which is paid by
the council."}))

(def p9 
  (make-statement :text {:en "Tom says that every household which recycles the household's garbage reduces a need of a new dump which is for the garbage."}))

(def p10 
  (make-statement :text {:en "Every household which reduces a need of a new dump benefits society."}))

; p11 is the complement of p18 and is defined below after p18

(def p12 
  (make-statement :text {:en "Tom owns a company that recycles some garbage."}))

(def p13 
  (make-statement :text {:en "Every person who owns a company that recycles some garbage earns some money from the garbage which is recycled."}))

(def p14 
  (make-statement :text {:en "Every supermarket creates some garbage."}))

(def p15 
  (make-statement :text {:en "Every supermarket should pay a tax for the garbage that the supermarket creates."}))

(def p16 
  (make-statement :text {:en "Every tax which is for some garbage which the supermarket creates is passed by the supermarket onto a household."}))

(def p17 (¬ p15))

(def p18 
  (make-statement :text {:en "Tom is an objective expert about recycling."}))

(def p11 (¬ p18))

(def p19 
  (make-statement :text {:en "If an objective expert says every household which recycles the household's garbage reduces a need of a new dump which is for the 
garbage, then every household which recycles the household's garbage
reduces a need of a new dump which is for the garbage."}))

(def a1 (make-argument :conclusion p1 :premises [p4, p3]))
(def a2 (make-argument :conclusion p4 :premises [p9, p18, p19]))
(def a3 (make-argument :conclusion p3 :premises [p10]))
(def a4 (make-argument :conclusion (¬ p18) :premises [p12, p13]))
(def a5 (make-argument :conclusion (¬ p1) :premises [p5]))
(def a6 (make-argument :conclusion p5 :premises [p6, p7, p8]))
(def a7 (make-argument :conclusion (¬ p1) :premises [p15]))
(def a8 (make-argument :conclusion p15 :premises [p14]))
(def a9 (make-argument :conclusion (¬ p15) :premises [p16]))

(def ag
    (-> (make-argument-graph)
        (assert-arguments [a1 a2 a3 a4 a5 a6 a7 a8 a9])
        (accept [p14])))

; (view ag)
