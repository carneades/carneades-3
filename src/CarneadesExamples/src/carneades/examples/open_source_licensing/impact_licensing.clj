;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.examples.open-source-licensing.impact-licensing
  (:use clojure.contrib.pprint
        clojure.contrib.profile
        clojure.set
        carneades.engine.argument-builtins
	carneades.engine.argument-search
        carneades.engine.shell
        carneades.engine.rule
	carneades.engine.owl
        carneades.engine.statement
        [carneades.engine.abduction :as abd]
	carneades.engine.lkif.import
        carneades.engine.lkif.export
        carneades.ui.diagram.viewer)
    (:require [carneades.engine.argument :as arg]))

; defining pathes
(def oss-path "src/carneades/examples/open_source_licensing/oss-rules.xml")
(def prepath "src/carneades/examples/open_source_licensing/")
(def impact-path "impact-licensing.owl")

; loading ontology
(def impact-ont (load-ontology impact-path prepath))

; loading rules from lkif
(def oss-kb (:rb (lkif-import oss-path '(transitive symmetric domain range equivalent))))

; defining helper methods for handling uris
(defn c [n s] (symbol (str n s)))
(def il "http://carneades.berlios.de/impact-licensing#")
(def oss "http://carneades.berlios.de/oss-licenses#")

; do copyright licenses exist that we may use for the carneades engine?
;(def goal1 (list 'exists 'LT (list (c oss "CopyrightLicenseTemplate") 'LT) (list (c oss "mayUseLicenseTemplate") (c il "CarneadesEngine") 'LT)))
(def goal1 (list (c oss "mayUseLicenseTemplate") (c il "CarneadesEngine") (c oss "GPL_Template")))

(def generators
  (list
    (generate-arguments-from-rules oss-kb '() impact-ont)
    (generate-arguments-from-owl impact-ont :reasoner)
    (builtins (list (generate-arguments-from-owl impact-ont :reasoner)))
    ))


; -------------------------------------------
; main
; -------------------------------------------

(defn main
  []

  ; accepting some facts in the initial argument graph
  (def ag0 (arg/accept arg/*empty-argument-graph*
             '((valid FSFTheoryOfLinking)
                (valid http://carneades.berlios.de/impact-licensing))))


  ; constructing arguments
  (def ag1 (construct-arguments-abductively goal1 100 3 ag0 generators))

  ; exporting results to lkif
  (lkif-export {:ags [ag1]} "impact-licensing.xml")
)

;(profile (main))
(main)

;(view ag1)
