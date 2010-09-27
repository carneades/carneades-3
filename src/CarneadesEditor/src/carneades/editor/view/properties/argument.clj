;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.properties.argument
  (:use clojure.contrib.def
        clojure.contrib.swing-utils
        carneades.editor.utils.swing)
  (:import carneades.editor.uicomponents.ArgumentPropertiesView))

(defvar- *argumentProperties* (ArgumentPropertiesView/instance))

(defvar *titleText* (.titleText *argumentProperties*))
(defvar *proButton* (.proButton *argumentProperties*))
(defvar *conButton* (.conButton *argumentProperties*))
(defvar *weightSpinner* (.weightSpinner *argumentProperties*))

(defvar- *pathText* (.pathText *argumentProperties*))
(defvar- *mapTitleText* (.mapTitleText *argumentProperties*))
(defvar- *applicabilityText* (.applicabilityText *argumentProperties*))

(defvar- *schemeText* (.schemeText *argumentProperties*))

(defvar- *id* (atom nil))
(defvar- *argid* (atom nil))
(defvar- *previous-title* (atom nil))
(defvar- *previous-weight* (atom nil))
(defvar- *previous-direction* (atom nil))
(defvar- *previous-scheme* (atom nil))

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

(defn init-argument-properties []
  (ArgumentPropertiesView/reset))

(defn get-argument-properties-panel [path id graphtitle argid title
                                     applicable weight direction scheme]
  (reset! *id* id)
  (reset! *previous-title* title)
  (reset! *argid* argid)
  (reset! *previous-weight* weight)
  (reset! *previous-direction* direction)
  (reset! *previous-scheme* scheme)
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
  {:path (.getText *pathText*)
   :id (deref *id*)
   :argid (deref *argid*)
   :previous-title (deref *previous-title*)
   :title (.getText *titleText*)
   :previous-weight (deref *previous-weight*)
   :weight (.getValue *weightSpinner*)
   :previous-direction (deref *previous-direction*)
   :direction (if (.isSelected *proButton*) :pro :con)
   :previous-scheme (deref *previous-scheme*)
   :scheme (.getText *schemeText*)
   })

(defn register-argument-weight-listener [l args]
  (let [changelistener (apply add-change-listener *weightSpinner* l args)]
   (swap! *change-listeners* conj changelistener)))

