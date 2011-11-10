(ns carneades.engine.test-cycles
  (:use clojure.test
        carneades.engine.shell
        carneades.engine.statement
        carneades.engine.argument
        carneades.engine.argument-graph
        carneades.engine.caes))

;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.examples.piersonpost
  (:use carneades.engine.statement
        carneades.engine.argument
        carneades.engine.argument-graph
       ;  carneades.mapcomponent.viewer
       ;  carneades.mapcomponent.export
       ; carneades.ui.diagram.graphvizviewer
        ))

;; The Pierson vs. Post case.  Used to illustrate the use of
;; a scheme for "practical reasoning" in legal argument.
;; See Atkinson, Bench-Capon, McBurney, "Arguing About Cases
;; as Practical Reasoning",  ICAIL05.  The following is a recontruction
;; of the arguments in the published opinion, not a reconstruction
;; of the reconstruction in Katie's, Trevor's and Peter's paper. 

;; The full text of the decision can be found at:
;; http://www.saucyintruder.org/pages/pierson.html 

;; TO DO: extend to illustrate argumentation schemes, premises roles,
;; support for representing statements in multiple natural languages
;; and references to source documents.

;;  Judge Tompkins Opinion, for the majority 


(def not-property 
  (make-statement 
    :text {:en "Post, by pursuing the fox, did not acquire property in the fox."}))

(def possession-required 
  (make-statement 
    :text {:en "Property rights in wild animals may be acquired only by possession."}))

(def foxes-are-wild 
  (make-statement :text {:en "Foxes are wild animals."}))

(def no-possession
  (make-statement :text {:en "Post did not have possession of the fox."}))

(def pursuit-not-sufficient 
  (make-statement :text {:en "Pursuit is not sufficient to acquire possession."}))

(def justinian 
  (make-statement :text {:en "Justinian's Institutes"}))

(def fleta (make-statement :text {:en "Fleta"}))

(def bracton (make-statement :text {:en "Bracton"}))

(def actual-possession-required 
  (make-statement :text {:en "Actual corporal possession is required."}))

(def puffendorf (make-statement :text {:en "Puffendorf"}))

(def bynkershoek (make-statement :text {:en "Bynkershoek"}))

(def mortally-wounded-deemed-possessed  
  (make-statement :text {:en "Pursuit is sufficient to obtain possession when the animal
is mortally wounded."}))

(def barbeyrac (make-statement :text {:en "Barbeyrac"}))

(def grotius (make-statement :text {:en "Grotius"}))

(def mortally-wounded (make-statement :text {:en "The fox was mortally wounded."}))

(def land-owner-has-possession ; warrant
  (make-statement :text {:en "The owner of land pursuing a livelihood with animals 
  on his land is deemed to have possession of the animals."}))

(def livelihood-on-own-land 
  (make-statement :text {:en "Post was pursing his livelihood on his own land"}))

(def keeble (make-statement :text {:en "Keeble"})) ; backing

(def certainty     ; action
  (make-statement :text {:en "A bright-line rule creates legal certainty, preserving 
peace and order."}))

(def order         ; value
  (make-statement :text {:en "Peace and order is an important social value."}))

(def a1 (make-argument 
          :conclusion not-property 
          :premises [possession-required, no-possession, foxes-are-wild]))

(def a2 (make-argument 
          :conclusion no-possession 
          :premises [pursuit-not-sufficient]))

(def a3 (make-argument
          :conclusion pursuit-not-sufficient 
          :premises [justinian]))

(def a4 (make-argument 
          :conclusion pursuit-not-sufficient
          :premises [fleta]))

(def a5 (make-argument
          :conclusion pursuit-not-sufficient
          :premises [bracton]))

(def a6 (make-argument 
          :conclusion no-possession 
          :premise [actual-possession-required]))

(def a7 (make-argument 
          :conclusion actual-possession-required
          :premises [puffendorf]))

(def a8 (make-argument 
          :conclusion puffendorf 
          :premises [bynkershoek]))

(def a9 (make-argument 
          :conclusion (neg actual-possession-required)
          :premises [mortally-wounded-deemed-possessed, mortally-wounded]))

(def a10 (make-argument 
           :conclusion mortally-wounded-deemed-possessed 
           :premises [grotius]))

(def a11 (make-argument 
           :conclusion mortally-wounded-deemed-possessed 
           :premises [barbeyrac]))

(def a12 (make-argument
           :conclusion (neg actual-possession-required)
           :premises [land-owner-has-possession, livelihood-on-own-land]))

(def a13 (make-argument 
           :conclusion land-owner-has-possession 
           :premises [keeble]))

; teleological argument 
(def a14 (make-argument 
           :conclusion actual-possession-required 
           :premises [certainty,    ; policy/action
                      order]))      ; value promoted

(def tompkins 
  (-> (make-argument-graph)
      (assert-arguments [a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14])
      (accept [foxes-are-wild possession-required certainty order])))

;(view tompkins)

;; Judge Livingston's dissent.

(def chased-by-big-dogs 
  (make-statement :text {:en "The fox was being chased by large hounds."}))

(def deemed-mortally-wounded 
  (make-statement :text {:en "A noxious animal being chased by large hounds shall be 
deemed mortally wounded."}))

(def protecting-farmers   ; value
  (make-statement :text {:en "Protecting farmers is an important social value."}))

(def encourage-hunting    ; policy/action
  (make-statement :text {:en "Encouraging hunting helps protect farmers 
  from noxious animals."}))

(def foxes-are-noxious
  (make-statement :text {:en "Foxes are noxious animals."}))

(def admitted-in-the-pleadings
  (make-statement :text {:en "It is admitted in the pleadings that a fox is a wild
and noxious beast."}))

(def a15 (make-argument 
           :conclusion mortally-wounded
           :premises [deemed-mortally-wounded, chased-by-big-dogs, foxes-are-noxious]))

(def a16 (make-argument 
           :conclusion deemed-mortally-wounded 
           :premises [protecting-farmers, encourage-hunting]))

(def a17 (make-argument
           :conclusion foxes-are-noxious
           :premises [admitted-in-the-pleadings]))

(def livingston 
  (-> (make-argument-graph) 
      (assert-arguments [a15 a16 a17]) 
      (accept [chased-by-big-dogs])))

(def both (-> tompkins 
              (assert-arguments [a15 a16 a17])
              (accept [chased-by-big-dogs])))

; (view both)

;(defn main []
;  (spit "/tmp/piersonpost.dot" 
;        (gen-graphvizcontent both statement-formatted))
;  (export-ag both statement-formatted "/tmp/piersonpost.svg"
;             :layout :radial
;             :width 1280
;             :height 1024))
;

