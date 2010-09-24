;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.properties.argument
  (:use clojure.contrib.def
        clojure.contrib.swing-utils)
  (:import carneades.editor.uicomponents.ArgumentPropertiesView))

(defvar- *argumentProperties* (ArgumentPropertiesView/instance))

(defvar- *pathText* (.pathText *argumentProperties*))
(defvar *titleText* (.titleText *argumentProperties*))
(defvar- *mapTitleText* (.mapTitleText *argumentProperties*))
(defvar- *applicabilityText* (.applicabilityText *argumentProperties*))
(defvar *weightSpinner* (.weightSpinner *argumentProperties*))
(defvar- *proButton* (.proButton *argumentProperties*))
(defvar- *conButton* (.conButton *argumentProperties*))
(defvar- *schemeText* (.schemeText *argumentProperties*))

(defn init-argument-properties []
  (ArgumentPropertiesView/reset))

(defvar- *id* (atom nil))
(defvar- *argid* (atom nil))
(defvar- *previous-title* (atom nil))
(defvar- *previous-weight* (atom nil))
(defvar- *previous-direction* (atom nil))
(defvar- *previous-scheme* (atom nil))

(defn get-argument-properties-panel [path id graphtitle argid title applicable weight direction scheme]
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
  (.setValue *weightSpinner* weight)
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