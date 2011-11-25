;;; Copyright ? 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1


(ns ^{:doc "Definition of a multimethod for various argument graph viewers."}
    carneades.maps.viewerdef)

;; this is mostly obsolete since the only implementation is the graphvizviewer
;; and the other code viewer are package outside the carneades-engine project

;; To do: consider porting this to use protocols and records

(defmulti view-graph (fn [viewer ag stmt-str] viewer))