;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.search
  (:use clojure.contrib.def
        clojure.contrib.swing-utils
        [clojure.string :only (trim)])
  (:import (javax.swing JScrollPane)
           (carneades.editor.uicomponents EditorApplicationView)))

(defvar- *frame* (EditorApplicationView/instance))

(defvar *searchButton* (.searchButton *frame*))
(defvar *searchComboBox* (.searchComboBox *frame*))

(defvar- *optionsPanel* (.optionsPanel *frame*))
(defvar- *showOptionsButton* (.showOptionsButton *frame*))
(defvar- *searchPanel* (.searchPanel *frame*))
(defvar- *searchScrollPane* (.searchScrollPane *frame*))
(defvar- *searchProgressBar* (.searchProgressBar *frame*))

(defvar- *showOptionsMessage* "Show options")
(defvar- *hideOptionsMessage* "Hide options")

(defvar- *searchActiveMessage* "Stop search")
(defvar- *searchInactiveMessage* "Search")

(defvar- *state* (atom false))
(defvar- *searchactive* (atom false))

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
    (.setSize scrollbar dim)))

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
  (.revalidate *searchPanel*))

(defn- toggle-options-button []
  (set-options-visible (not (deref *state*))))

(defn- showoptions-button-listener [event]
  (toggle-options-button))

(defn- add-item-to-search-box [item]
  (loop [n (dec (.getItemCount *searchComboBox*))]
    (cond (neg? n)
          (.addItem *searchComboBox* item)
    
          (not= (.getItemAt *searchComboBox* n) item)
          (recur (dec n)))))

(defn- set-search-button-active [active]
  (reset! *searchactive* active)
  (if active
    (do
     (.setText *searchButton* *searchActiveMessage*)
     (.setIndeterminate *searchProgressBar* true))
    (do
     (.setText *searchButton* *searchInactiveMessage*)
     (.setIndeterminate *searchProgressBar* false))))

(defn- toggle-search-button []
  (set-search-button-active (not (deref *searchactive*))))

(defn- search-button-listener [event]
  (if (deref *searchactive*)
    (toggle-search-button)
    (if-let [text (.getSelectedItem *searchComboBox*)]
      (do
        (add-item-to-search-box (trim text))
        (toggle-search-button))
      (.setSelected *searchButton* false))))

(defn init-search []
  (set-options-visible (deref *state*))
  (add-action-listener *showOptionsButton* showoptions-button-listener)
  (add-action-listener *searchButton* search-button-listener))

