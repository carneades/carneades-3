(ns carneades.editor.view.properties.properties
  (:use clojure.contrib.def)
  (:import carneades.editor.uicomponents.EditorApplicationView))

(defvar- *viewinstance* (EditorApplicationView/instance))

(defvar- *propertiesPanel* (.propertiesPanel *viewinstance*))

(defvar- *properties* (atom nil))

(defn show-properties [propertypanel]
  "show a given property panel"
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
