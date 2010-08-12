;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1


(ns carneades.engine.test-argument-from-arguments
  (:use clojure.test
        clojure.contrib.pprint
        carneades.engine.utils
        carneades.engine.shell
        carneades.engine.argument-from-arguments
        carneades.engine.argument))

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

(deftest test-argument-from-arguments
  (let [a1 (make-arg a1 (pro p1 (pm p4) (pm p3)))
        a2 (make-arg a2 (pro p4 (pm p9) (pm p18) (pm p19)))
        a3 (make-arg a3 (pro p3 (pm p10)))
        a4 (make-arg a4 (con p18 (pm p12) (pm p13)))
        a5 (make-arg a5 (con p1 (pm p5)))
        a6 (make-arg a6 (pro p5 (pm p6) (pm p7) (pm p8)))
        a7 (make-arg a7 (con p1 (pm p15)))
        a8 (make-arg a8 (pro p15 (pm p14)))
        a9 (make-arg a9 (con p15 (pm p16)))
        args1 (assert-arguments *empty-argument-graph*
                                [a1 a2 a3 a4 a5 a6 a7 a8 a9])
        args2 (accept *empty-argument-graph* [p14])
        e1 (make-engine* 100 2 args2
                         [(generate-arguments-from-argument-graph args1)])
        state (first (e1 p15))]
    (is (not (nil? state)))
    (let [ag (sget state :arguments)]
      (is (some (fn [arg]
                  (and (= (sget arg :conclusion) p15)
                       (some (fn [premise]
                               (= (sget premise :atom) p14))
                             (sget arg :premises))))
                (vals (sget ag :arguments)))))))
