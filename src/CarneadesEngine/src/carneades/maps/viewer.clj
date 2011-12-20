;;; Copyright ? 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1


(ns ^{:doc "Definition for a view function that invokes a particular argument graph
            viewer base on the properties file of the carneades-engine project"}
    carneades.maps.viewer
  (:use clojure.contrib.def
        carneades.config.reader
        carneades.maps.viewerdef
        carneades.maps.graphvizviewer
        carneades.engine.statement))

;;; this is also mostly obsolete since viewers are packaged outside the carneades engine project
;;; now and the only implementation in the engine is the DOT viewer


(defvar- *viewerlibrary* (configvalue "viewer.library"))

(defn view [ag]
  (view-graph *viewerlibrary* ag #(literal->str %)))

