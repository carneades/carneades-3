;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.examples.lkif
  ;(:require )
  (:use
    carneades.engine.argument
    carneades.engine.argument-builtins
    carneades.engine.rule
    carneades.engine.lkif.import
    carneades.engine.shell
    carneades.mapcomponent.viewer
    clojure.contrib.pprint)
  ;(:import )
  )

(def path "src/carneades/examples/lkif.xml")

(def i (lkif-import path))

(def rb (:rb i))

(defn engine [max-nodes max-turns critical-questions]
  (make-engine* max-nodes max-turns (argument-graph)
                (list (generate-arguments-from-rules rb critical-questions)
                      builtins)))

(def e1 (engine 20 2 '()))
(def e2 (engine 20 2 '(excluded)))

(def ag (first (:ags i)))

;(dorun (ask '(flies ?bird) e1))
;(pprint "--------")
;(dorun (ask '(flies ?bird) e2))

  