;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.examples.open-source-licensing.impact-empty
  (:use carneades.engine.lkif
        carneades.engine.argument-builtins
        carneades.engine.shell
        carneades.ui.diagram.viewer))

;; run with "lein run carneades.examples.open-source-licensing.impact-empty"
(defn -main []
  (let [path "src/carneades/examples/open_source_licensing/impact-kb.xml"
        epath "src/carneades/examples/open_source_licensing/impact-full.xml"
        lkif (import-lkif path)
        c (fn [n s] (symbol (str n s)))
        il "http://carneades.berlios.de/impact-licensing#"
        oss "http://carneades.berlios.de/oss-licenses#"
        goal1 (list (c oss "mayUseLicenseTemplate") (c il "CarneadesEngine")
                    (c oss "GPL_Template"))
        generators (list (generate-arguments-from-lkif lkif))
        ag1 (construct-arguments-abductively goal1 100 3 (first (:ags lkif)) generators)
        exported (assoc lkif :ags (cons ag1 (:ags lkif)))]
    (export-lkif exported epath)))


