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
	carneades.engine.lkif
        carneades.engine.utils
        carneades.mapcomponent.viewer)
    (:require [carneades.engine.argument :as arg]))

;; run with 'lein run -m carneades.examples.open-source-licensing.impact-licensing'
(defn -main []
  (let [basedir "src/carneades/examples/open_source_licensing/" 
        oss-path (str basedir "oss-rules.xml")
        impact-name "impact-licensing.owl"
        impact-ont (load-ontology impact-name basedir)
        oss-kb (:rb (import-lkif oss-path))
        ;; defining helper methods for handling uris
        c (fn [n s] (symbol (str n s)))
        il "http://carneades.berlios.de/impact-licensing#"
        oss "http://carneades.berlios.de/oss-licenses#"
        ;; do copyright licenses exist that we may use for the carneades engine?
        goal1 (list 'exists 'LT (list (c oss "CopyrightLicenseTemplate") 'LT)
                    (list (c oss "mayUseLicenseTemplate") (c il "CarneadesEngine") 'LT))
        generators (list
                    (generate-arguments-from-rules oss-kb '() impact-ont)
                    (generate-arguments-from-owl impact-ont :reasoner)
                    (builtins (list (generate-arguments-from-owl impact-ont :reasoner))))
        ;; accepting some facts in the initial argument graph
        ag (arg/accept arg/*empty-argument-graph*
             '((valid FSFTheoryOfLinking)
                (valid http://carneades.berlios.de/impact-licensing)))
        ;; constructing arguments
        ag1 (construct-arguments-abductively goal1 100 3 ag generators)]
    ;; exporting results to lkif
    (export-lkif {:ags [ag1]} "impact-licensing.xml")
    (view ag1)))


