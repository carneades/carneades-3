(ns carneades.editor.view.properties.lkif
  (:use clojure.contrib.def)
  (:import carneades.editor.uicomponents.LkifFilePropertiesView))

(defvar- *lkifProperties* LkifFilePropertiesView/instance)
(defvar- *lkifPathText* LkifFilePropertiesView/pathTextField)

(defn get-lkif-properties-panel [path]
  (.setText *lkifPathText* path)
  *lkifProperties*)

