;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1


(ns ^{:doc "Functions to show/hide properties in the panel properties."}
  carneades.editor.view.properties.properties
  (:use clojure.contrib.def
        carneades.editor.view.components.uicomponents))

(defvar- *propertiesPanel* (.propertiesPanel *frame*))

(defvar- *properties* (atom nil))

(defn show-properties
  "show a given property panel"
  [propertypanel]
  (.removeAll *propertiesPanel*)
  (reset! *properties* propertypanel)
  (.add *propertiesPanel* propertypanel)
  (.revalidate *propertiesPanel*)
  (.repaint *propertiesPanel*))

(defn hide-properties []
  (when (deref *properties*)
   (.remove *propertiesPanel* (deref *properties*))
   (reset! *properties* nil)
   (.revalidate *propertiesPanel*)
   (.repaint *propertiesPanel*)))
