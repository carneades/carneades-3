(ns carneades.editor.view.properties.lkif
  (:use clojure.contrib.def)
  (:import carneades.editor.uicomponents.LkifFilePropertiesView))

(defvar- *lkifProperties* (LkifFilePropertiesView/instance))
(defvar- *lkifPathText* (.pathTextField *lkifProperties*))

(defn lkif-properties-init []
  (LkifFilePropertiesView/reset))

(defn get-lkif-properties-panel [path]
  (.setText *lkifPathText* path)
  *lkifProperties*)

