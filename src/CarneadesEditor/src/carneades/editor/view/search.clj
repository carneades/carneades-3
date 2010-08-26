;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.search
  (:use clojure.contrib.def
        clojure.contrib.swing-utils)
  (:import (javax.swing JScrollPane)
           (carneades.editor.uicomponents EditorApplicationView)))

(defvar- *frame* (EditorApplicationView/instance))
(defvar- *optionsPanel* (.optionsPanel *frame*))
(defvar- *showOptionsButton* (.showOptionsButton *frame*))
(defvar- *searchPanel* (.searchPanel *frame*))
(defvar- *searchScrollPane* (.searchScrollPane *frame*))

(defvar- *showOptionsMessage* "Show options")
(defvar- *hideOptionsMessage* "Hide options")

(defvar- *state* (atom false))

(defn- update-scrollbar-size []
  ;; see http://forums.sun.com/thread.jspa?threadID=5426991
  (.setPreferredSize *searchPanel* nil)
  (.setPreferredSize *searchScrollPane* nil)
  (let [scrollbar (.getVerticalScrollBar *searchScrollPane*)
        dimscrollbar (.getPreferredSize scrollbar)
        dim (.getPreferredSize *searchPanel*)]
    (.setSize dim (.width dimscrollbar) (.height dim))
    (.setVisible scrollbar true)
    (.setPreferredSize scrollbar dim)
    (.setSize scrollbar dim)
    ;; (.revalidate scrollbar)
    ))

(defn- set-options-visible [state]
  (reset! *state* state)
  (if state
    (do
      (.setText *showOptionsButton* *hideOptionsMessage*)
      (.setVisible *optionsPanel* true))
    (do
      (.setText *showOptionsButton* *showOptionsMessage*)
      (.setVisible *optionsPanel* false)))
  (.setPreferredSize *searchScrollPane* (.getPreferredSize *searchPanel*))
  (update-scrollbar-size)
  (.revalidate *searchPanel*)
  ;; (.revalidate *searchScrollPane*)
  )

(defn- toggle-options-button []
  (set-options-visible (not (deref *state*))))

(defn- showOptionsButtonListener [event]
  (toggle-options-button))

(defn init-search []
  (set-options-visible (deref *state*))
  (add-action-listener *showOptionsButton* showOptionsButtonListener))

