;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.properties.premise
  (:use clojure.contrib.def
        clojure.contrib.swing-utils)
  (:import carneades.editor.uicomponents.PremisePropertiesView))

(defvar- *premiseProperties* (PremisePropertiesView/instance))
(defvar- *pathText* (.pathText *premiseProperties*))
(defvar- *mapTitleText* (.mapTitleText *premiseProperties*))
(defvar- *negatedCheckBox* (.negatedCheckBox *premiseProperties*))
(defvar- *typeComboBox* (.typeComboBox *premiseProperties*))

(defn init-premise-properties []
  (PremisePropertiesView/reset))

(defn get-premise-properties-panel [path maptitle polarity type]
  (.setText *pathText* path)
  (.setText *mapTitleText* maptitle)
  (.setSelected *negatedCheckBox* (not polarity))
  (.setSelectedItem *typeComboBox* type)
  *premiseProperties*)

