;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.examples.piersonpost
  (:use carneades.engine.statement
        carneades.engine.argument
        carneades.mapcomponent.viewer))

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

(defargument a1 (pro not-property 
                         (pm possession-required)
                         (pm no-possession)
                         (pm foxes-are-wild)))

(defargument a2 (pro no-possession (pm pursuit-not-sufficient)))
(defargument a3 (pro pursuit-not-sufficient (pm justinian)))
(defargument a4 (pro pursuit-not-sufficient (pm fleta)))
(defargument a5 (pro pursuit-not-sufficient (pm bracton)))
(defargument a6 (pro no-possession (pm actual-possession-required)))

(defargument a7 (pro actual-possession-required (pm puffendorf)))
(defargument a8 (pro puffendorf (pm bynkershoek)))

(defargument a9
  (con actual-possession-required 
       (pm mortally-wounded-deemed-possessed)
       (pm mortally-wounded)))

(defargument a10 (pro mortally-wounded-deemed-possessed (pm grotius)))

(defargument a11 (pro mortally-wounded-deemed-possessed (pm barbeyrac)))

(defargument a12 
  (con actual-possession-required
       (pm land-owner-has-possession)
       (pm livelihood-on-own-land)))

(defargument a13 (pro land-owner-has-possession (pm keeble)))

; teleological argument 
(defargument a14  
  (pro actual-possession-required 
       (pm certainty)   ; policy/action
       (pm order)))     ; value promoted

(def args1
     (argument-graph [a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14]))

(def facts1 [foxes-are-wild possession-required certainty order])

(def tompkins (accept args1 facts1))

;(view tompkins)


;; ; Judge Livingston's dissent.

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

(defargument a15
  (pro mortally-wounded
       (pm deemed-mortally-wounded)
       (pm chased-by-big-dogs)
       (pm foxes-are-noxious)))

(defargument a16
  (pro deemed-mortally-wounded 
       (pm protecting-farmers)
       (pm encourage-hunting)))

(defargument a17
  (pro foxes-are-noxious (pm admitted-in-the-pleadings)))

(def args2 (argument-graph [a15 a16 a17]))
(def facts2 [chased-by-big-dogs])
(def livingston (accept args2 facts2))


(def both (accept (assert-arguments tompkins [a15 a16 a17])
                     facts2))
; (view both)

(def fig4-args (argument-graph [a9 a10 a11]))
(def fig5-args (argument-graph [a12 a13]))

(defn -main []
  (view both))
