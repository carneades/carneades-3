;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.properties.argument
  (:use clojure.contrib.def
        clojure.contrib.swing-utils)
  (:import carneades.editor.uicomponents.ArgumentPropertiesView))

(defvar- *argumentProperties* (ArgumentPropertiesView/instance))

(defvar- *pathText* (.pathText *argumentProperties*))
(defvar- *titleText* (.titleText *argumentProperties*))
(defvar- *mapTitleText* (.mapTitleText *argumentProperties*))
(defvar- *applicableCheckBox* (.applicableCheckBox *argumentProperties*))
(defvar- *weightSpinner* (.weightSpinner *argumentProperties*))
(defvar- *proButton* (.proButton *argumentProperties*))
(defvar- *conButton* (.conButton *argumentProperties*))
(defvar- *schemeText* (.schemeText *argumentProperties*))

(defn- applicable-checkbox-listener [event]
  (.setSelected *applicableCheckBox* (not (.isSelected *applicableCheckBox*))))

(defn init-argument-properties []
  (ArgumentPropertiesView/reset)
  (add-action-listener *applicableCheckBox* applicable-checkbox-listener))

(defn get-argument-properties-panel [path graphtitle title applicable weight direction scheme]
  (.setText *mapTitleText* graphtitle)
  (.setText *pathText* path)
  (.setText *titleText* title)
  (.setSelected *applicableCheckBox* applicable)
  (.setValue *weightSpinner* weight)
  (.setSelected *proButton* (= direction :pro))
  (.setSelected *conButton* (= direction :con))
  (.setText *schemeText* scheme)
  *argumentProperties*)
