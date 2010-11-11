
(ns carneades.examples.open-source-licensing.impact-empty
  ;(:require )
  (:use
    carneades.engine.lkif
    carneades.engine.argument-builtins
    carneades.engine.shell
    carneades.ui.diagram.viewer
    )
  ;(:import )
  )

(def path "src/carneades/examples/open_source_licensing/impact-kb.xml")
(def epath "src/carneades/examples/open_source_licensing/impact-full.xml")

(def i (lkif-import path))

(defn c [n s] (symbol (str n s)))
(def il "http://carneades.berlios.de/impact-licensing#")
(def oss "http://carneades.berlios.de/oss-licenses#")
(def goal1 (list (c oss "mayUseLicenseTemplate") (c il "CarneadesEngine") (c oss "GPL_Template")))

(def generators
  (list (generate-arguments-from-lkif i)))

(def ag1 (construct-arguments-abductively goal1 100 3 (first (:ags i)) generators))

(def e (assoc i :ags (cons ag1 (:ags i))))

(lkif-export e epath)
