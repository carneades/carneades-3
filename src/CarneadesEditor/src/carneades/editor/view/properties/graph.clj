;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.properties.graph
  (:use clojure.contrib.def)
  (:import carneades.editor.uicomponents.ArgumentGraphPropertiesView))

(defvar- *graphProperties* (ArgumentGraphPropertiesView/instance))
(defvar- *titleText* (.titleText *graphProperties*))
(defvar- *pathText* (.pathText *graphProperties*))
(defvar- *mainIssueTextArea* (.mainIssueTextArea *graphProperties*))

(defn graph-properties-init []
  (ArgumentGraphPropertiesView/reset))

(defn get-graph-properties-panel [path title mainissue]
  (.setText *pathText* path)
  (.setText *titleText* title)
  (.setText *mainIssueTextArea* mainissue)
  *graphProperties*)
