;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1


(ns carneades.ui.diagram.viewerdef)

(defmulti view-graph (fn [viewer ag stmt-str] viewer))
