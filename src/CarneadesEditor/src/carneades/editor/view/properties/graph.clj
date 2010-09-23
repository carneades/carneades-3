;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.properties.graph
  (:use clojure.contrib.def
        clojure.contrib.swing-utils)
  (:import carneades.editor.uicomponents.ArgumentGraphPropertiesView))

(defvar- *graphProperties* (ArgumentGraphPropertiesView/instance))
(defvar- *titleText* (.titleText *graphProperties*))
(defvar- *pathText* (.pathText *graphProperties*))
(defvar- *mainIssueTextArea* (.mainIssueTextArea *graphProperties*))

(defvar- *graph-edit-listeners* (atom ()))

(defn- title-action-listener [event]
  (doseq [{:keys [listener args]} (deref *graph-edit-listeners*)]
    (apply listener event args)))

(defn graph-properties-init []
  (ArgumentGraphPropertiesView/reset)
  (add-action-listener *titleText* title-action-listener))

(defvar- *id* (atom nil))
(defvar- *previous-title* (atom nil))

(defn get-graph-properties-panel [path id title mainissue]
  (reset! *id* id)
  (reset! *previous-title* title)
  (.setText *pathText* path)
  (.setText *titleText* title)
  (.setText *mainIssueTextArea* mainissue)
  *graphProperties*)

(defn graph-being-edited-info []
  {:path (.getText *pathText*)
   :id (deref *id*)
   :title (.getText *titleText*)
   :previous-title (deref *previous-title*)})

(defn register-graph-edit-listener [f args]
  (swap! *graph-edit-listeners* conj {:listener f :args args}))
