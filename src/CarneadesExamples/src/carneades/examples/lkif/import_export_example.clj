;; Copyright (c) 2010 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.examples.lkif.import-export-example
  (:use clojure.pprint
        carneades.engine.argument
        carneades.engine.argument-builtins
        carneades.engine.rule
        carneades.engine.lkif
        carneades.engine.utils
        carneades.engine.shell
        carneades.mapcomponent.viewer))

;; run with "lein run carneades.examples.lkif.import-export-example"
(defn -main []
  (let [lkif (import-lkif "src/carneades/examples/lkif/lkif.xml")
        goal '(p ?x)
        ag (unite-solutions (construct-arguments goal
                                                 50
                                                 (argument-graph)
                                                 (list (generate-arguments-from-lkif lkif))))
        lkif2 (assoc lkif :ags (cons ag (:ags lkif)))]
    ;; (prn "lkif =")
    ;; (pprint lkif)
    ;; (view ag)
    (export-lkif lkif "src/carneades/examples/lkif/export.xml")))
