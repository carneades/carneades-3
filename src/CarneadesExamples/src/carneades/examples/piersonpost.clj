;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.examples.piersonpost
  (:use carneades.engine.statement
        carneades.engine.argument
        carneades.mapcomponent.viewer
        carneades.mapcomponent.export
        carneades.ui.diagram.graphvizviewer))

;; The Pierson vs. Post case.  Used to illustrate the use of
;;   a scheme for "practical reasoning" in legal argument.
;; See Atkinson, Bench-Capon, McBurney, "Arguing About Cases
;;  as Practical Reasoning",  ICAIL05.  The following is a recontruction
;; of the arguments in the published opinion, not a reconstruction
;; of the reconstruction in Katie's, Trevor's and Peter's paper. 

;; The full text of the decision can be found at:
;; http://www.saucyintruder.org/pages/pierson.html 

;;  Judge Tompkins Opinion, for the majority 


(def not-property 
  "Post, by pursuing the fox, 
did not acquire property 
in the fox.") 

(def possession-required 
  "Property rights in wild 
animals may be acquired 
only by possession.")

(def foxes-are-wild 
  "Foxes are wild animals.")

(def no-possession
  "Post did not have 
possession of the fox.")

(def pursuit-not-sufficient 
  "Pursuit is not sufficient 
to acquire possession.")

(def justinian 
  "Justinian's 
Institutes")

(def fleta "Fleta")

(def bracton "Bracton")

(def actual-possession-required 
  "Actual corporal 
possession is required.")

(def puffendorf "Puffendorf")

(def bynkershoek "Bynkershoek")

(def mortally-wounded-deemed-possessed  
  "Pursuit is sufficient to obtain 
possession when the animal
is mortally wounded.")

(def barbeyrac "Barbeyrac")

(def grotius "Grotius")

(def mortally-wounded 
  "The fox was 
mortally wounded.")

(def land-owner-has-possession ; warrant
  "The owner of land pursuing a 
livelihood with animals on his land is
deemed to have possession of the animals.")

(def livelihood-on-own-land 
  "Post was pursing his livelihood 
on his own land")

(def keeble "Keeble") ; backing

(def certainty     ; action
  "A bright-line rule creates
legal certainty, preserving 
peace and order.")

(def order         ; value
  "Peace and order is
an important social value.")

(def a1 (make-argument 
          :id 'a1 
          :conclusion not-property 
          :premises [possession-required, no-possession, foxes-are-wild]))

(def a2 (make-argument 
          :id 'a2 
          :conclusion no-possession 
          :premises [pursuit-not-sufficient]))

(def a3 (make-argument
          :id 'a3 
          :conclusion pursuit-not-sufficient 
          :premises [justinian]))

(def a4 (make-argument 
          :id 'a4
          :conclusion pursuit-not-sufficient
          :premises [fleta]))

(def a5 (make-argument
          :id pursuit-not-sufficient
          :premises [bracton]))

(def a6 (make-argument 
          :id 'a6
          :conclusion no-possession 
          :premise [actual-possession-required]))

(def a7 (make-argument 
          :id 'a7
          :conclusion actual-possession-required
          :premises [puffendorf]))

(def a8 (make-argument 
          :id 'a8 
          :conclusion puffendorf 
          :premises [bynkershoek]))

(def a9 (make-argument 
          :id 'a9
          :conclusion (¬ actual-possession-required)
          :premises [mortally-wounded-deemed-possessed, mortally-wounded]))

(def a10 (make-argument 
           :id 'a10
           :conclusion mortally-wounded-deemed-possessed 
           :premises [grotius]))

(def a11 (make-argument 
           :id 'a11 
           :conclusion mortally-wounded-deemed-possessed 
           :premises [barbeyrac]))

(def a12 (make-argument
           :id 'a12
           :conclusion (¬ actual-possession-required)
           :premises [land-owner-has-possession, livelihood-on-own-land]))

(def a13 (make-argument 
           :id 'a13
           :conclusion land-owner-has-possession 
           :premises [keeble]))

; teleological argument 
(def a14 (make-argument 
           :id 'a14
           :conclusion actual-possession-required 
           :premises [certainty,    ; policy/action
                      order]))      ; value promoted

(def args1 
  (-> (make-argument-graph)
      (assert-arguments [a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14])))

(def facts1 [foxes-are-wild possession-required certainty order])

(def tompkins (accept args1 facts1))

;(view tompkins)

;; Judge Livingston's dissent.

(def chased-by-big-dogs 
  "The fox was being 
chased by large hounds.")

(def deemed-mortally-wounded 
  "A noxious animal being chased 
by large hounds shall be 
deemed mortally wounded.")

(def protecting-farmers   ; value
  "Protecting farmers is an 
important social value.")

(def encourage-hunting    ; policy/action
  "Encouraging hunting 
helps protect farmers 
from noxious animals.")

(def foxes-are-noxious
  "Foxes are noxious animals.")

(def admitted-in-the-pleadings
  "It is admitted in 
the pleadings that
a fox is a wild
and noxious beast.")

(def a15 (make-argument 
           :id 'a15
           :conclusion mortally-wounded
           :premises [deemed-mortally-wounded, chased-by-big-dogs, foxes-are-noxious]))

(def a16 (make-argument 
           :id 'a16
           :conclusion deemed-mortally-wounded 
           :premises [protecting-farmers, encourage-hunting]))

(def a17 (make-argument
           :id 'a17
           :conclusion foxes-are-noxious
           :premises [admitted-in-the-pleadings]))

(def args2 (-> (make-argument-graph) (assert-arguments [a15 a16 a17]))) 
(def facts2 [chased-by-big-dogs])
(def livingston (accept args2 facts2))

(def both (-> tompkins 
              (assert-arguments [a15 a16 a17])
              (accept facts2)))

; (view both)

(def fig4-args (-> (make-argument-graph) (assert-arguments [a9 a10 a11])))
(def fig5-args (-> (make-argument-graph) (assert-arguments [a12 a13])))

(defn main1 []
  (view both))

(defn main2 []
  (spit "/tmp/piersonpost.dot" 
        (gen-graphvizcontent both statement-formatted))
  (export-ag both statement-formatted "/tmp/piersonpost.svg"
             :layout :radial
             :width 1280
             :height 1024))


