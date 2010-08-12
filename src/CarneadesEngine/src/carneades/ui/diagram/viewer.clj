;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1


(ns carneades.ui.diagram.viewer
  (:use clojure.contrib.def
        carneades.config.reader
        carneades.ui.diagram.viewerdef
        carneades.ui.diagram.graphvizviewer
        carneades.engine.statement))

(defvar- *viewerlibrary* (configvalue "viewer.library"))

(defn view [ag]
  (view-graph *viewerlibrary* ag #(statement-formatted %)))

