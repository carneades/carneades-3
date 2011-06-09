;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Function to display argument properties in the panel properties."}
  carneades.editor.view.properties.argument
  (:use clojure.contrib.def
        clojure.contrib.swing-utils
        carneades.editor.utils.swing)
  (:import carneades.editor.uicomponents.ArgumentPropertiesView))

(defvar- *argumentProperties* (ArgumentPropertiesView.))

(defvar *titleText* (.titleText *argumentProperties*))
(defvar *proButton* (.proButton *argumentProperties*))
(defvar *conButton* (.conButton *argumentProperties*))
(defvar *weightSpinner* (.weightSpinner *argumentProperties*))

(defvar- *pathText* (.pathText *argumentProperties*))
(defvar- *mapTitleText* (.mapTitleText *argumentProperties*))
(defvar- *applicabilityText* (.applicabilityText *argumentProperties*))

(defvar *schemeText* (.schemeText *argumentProperties*))

(defvar- *change-listeners* (atom #{}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; private functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- set-spinner-value [val]
  ;; set the spinner value but without firing events
  (doseq [listener (deref *change-listeners*)]
    (.removeChangeListener *weightSpinner* listener)
    (.setValue *weightSpinner* (double val))
    (.addChangeListener *weightSpinner* listener)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn init-argument-properties [])

(defvar- *previous-argument-content* (atom {}))

(defn get-argument-properties-panel [path id graphtitle argid title
                                     applicable weight direction scheme]
  (reset! *previous-argument-content* {:id id
                                       :previous-title title
                                       :argid argid
                                       :previous-weight weight
                                       :previous-direction direction
                                       :previous-scheme scheme})
  (.setText *mapTitleText* graphtitle)
  (.setText *pathText* path)
  (.setText *titleText* title)
  (if applicable
    (.setText *applicabilityText* "Applicable")
    (.setText *applicabilityText* "Not Applicable"))
  (set-spinner-value weight)
  (.setSelected *proButton* (= direction :pro))
  (.setSelected *conButton* (= direction :con))
  (.setText *schemeText* scheme)
  *argumentProperties*)

(defn argument-being-edited-info []
  (merge
   {:path (.getText *pathText*)
    :title (.getText *titleText*)
    :weight (.getValue *weightSpinner*)
    :direction (if (.isSelected *proButton*) :pro :con)
    :scheme (.getText *schemeText*)}
   (deref *previous-argument-content*)))

(defn register-argument-weight-listener [l args]
  (let [changelistener (apply add-change-listener *weightSpinner* l args)]
   (swap! *change-listeners* conj changelistener)))

