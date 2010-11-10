;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.properties.graph
  (:use clojure.contrib.def
        clojure.contrib.swing-utils
        carneades.editor.utils.listeners)
  (:import carneades.editor.uicomponents.ArgumentGraphPropertiesView))

(defvar- *graphProperties* (ArgumentGraphPropertiesView/instance))
(defvar- *titleText* (.titleText *graphProperties*))
(defvar- *pathText* (.pathText *graphProperties*))
(defvar- *mainIssueTextArea* (.mainIssueTextArea *graphProperties*))

(gen-listeners-fns "graph-edit")

(defn- title-action-listener [event]
  (call-graph-edit-listeners event))

(defn init-graph-properties []
  (ArgumentGraphPropertiesView/reset)
  (add-action-listener *titleText* title-action-listener))

(defvar- *previous-graph-content* (atom {}))

(defn get-graph-properties-panel [path id title mainissue]
  (reset! *previous-graph-content* {:id id
                                    :previous-title title})
  (.setText *pathText* path)
  (.setText *titleText* title)
  (.setText *mainIssueTextArea* mainissue)
  *graphProperties*)

(defn graph-being-edited-info []
  (merge
   {:path (.getText *pathText*)
    :title (.getText *titleText*)}
   (deref *previous-graph-content*)))

