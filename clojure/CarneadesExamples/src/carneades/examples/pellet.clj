
(ns carneades.examples.pellet
  ;(:require )
  (:use
    carneades.ui.diagram.viewer
    carneades.engine.argument
    carneades.engine.pellet
    carneades.engine.shell)

  ;(:import )
  )


(def owl "http://owl.man.ac.uk/2006/07/sssw/people.owl")
(def cname "http://owl.man.ac.uk/2006/07/sssw/people#person")
(def pname "http://owl.man.ac.uk/2006/07/sssw/people#has_pet")
(def iname "http://owl.man.ac.uk/2006/07/sssw/people#Kevin")
(def iname2 "http://owl.man.ac.uk/2006/07/sssw/people#Flossie2")
(def class-symbol (symbol cname))
(def ind-symbol (symbol iname))
(def ind-symbol2 (symbol iname2))
(def prop-symbol (symbol pname))
(def goal1 (list class-symbol '?x))
(def goal2 (list class-symbol ind-symbol))
(def goal3 (list prop-symbol ind-symbol '?x))
(def goal4 (list prop-symbol ind-symbol ind-symbol2))


(def generators
  (list (generate-arguments-from-owl owl)))

(def e1 (make-engine* 5000 1 *empty-argument-graph* generators))