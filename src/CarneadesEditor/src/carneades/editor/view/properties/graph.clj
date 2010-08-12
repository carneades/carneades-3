(ns carneades.editor.view.properties.graph
  (:use clojure.contrib.def)
  (:import carneades.editor.uicomponents.ArgumentGraphPropertiesView))

(defvar- *graphProperties* (ArgumentGraphPropertiesView/instance))
(defvar- *idText* (.idText *graphProperties*))
(defvar- *titleText* (.titleText *graphProperties*))
(defvar- *mainIssueTextArea* (.mainIssueTextArea *graphProperties*))

(defn graph-properties-init []
  (ArgumentGraphPropertiesView/reset))

(defn get-graph-properties-panel [id title mainissue]
  (.setText *idText* id)
  (.setText *titleText* title)
  (.setText *mainIssueTextArea* mainissue)
  *graphProperties*)
