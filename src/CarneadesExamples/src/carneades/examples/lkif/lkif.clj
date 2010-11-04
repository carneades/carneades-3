;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.examples.lkif
  ;(:require )
  (:use
    carneades.engine.argument
    carneades.engine.argument-builtins
    carneades.engine.rule
    carneades.engine.lkif
    carneades.engine.shell
    carneades.ui.diagram.viewer
    clojure.contrib.pprint)
  ;(:import )
  )

(def i-path "src/carneades/examples/lkif/lkif.xml")
(def e-path "src/carneades/examples/lkif/export.xml")

(def i (lkif-import i-path))

;(def rb (:rb i))

(def goal '(p ?x))
(def ag (unite-solutions (construct-arguments goal 50 *empty-argument-graph* (list (generate-arguments-from-lkif i)))))

(def e (assoc i :ags (cons ag (:ags i))))

(lkif-export e e-path)

;(def e2 (engine 20 2 '(excluded)))
;
;(def ag (first (:ags i)))

;(dorun (ask '(flies ?bird) e1))i
;(pprint "--------")
;(dorun (ask '(flies ?bird) e2))

  