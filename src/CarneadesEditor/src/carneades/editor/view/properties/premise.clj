;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.properties.premise
  (:use clojure.contrib.def
        clojure.contrib.swing-utils
        carneades.editor.utils.seq)
  (:import carneades.editor.uicomponents.PremisePropertiesView))

(defvar- *premiseProperties* (PremisePropertiesView/instance))
(defvar- *pathText* (.pathText *premiseProperties*))
(defvar- *mapTitleText* (.mapTitleText *premiseProperties*))
(defvar *negatedCheckBox* (.negatedCheckBox *premiseProperties*))
(defvar *typeComboBox* (.typeComboBox *premiseProperties*))

(defn init-premise-properties []
  (PremisePropertiesView/reset))

(defvar- *previous-polarity* (atom nil))
(defvar- *id* (atom nil))
(defvar- *arg* (atom nil))
(defvar- *atom* (atom nil))
(defvar- *previous-type* (atom nil))
(defvar- *stmt* (atom nil))

(defvar- *type-to-str* {:carneades.engine.argument/ordinary-premise "Premise"
                        :carneades.engine.argument/assumption "Assumption"
                        :carneades.engine.argument/exception "Exception"})

(defvar- *str-to-type* (reverse-map *type-to-str*))

(defn get-premise-properties-panel [path id maptitle arg polarity type atom]
  (reset! *previous-polarity* polarity)
  (reset! *id* id)
  (reset! *previous-type* type)
  (reset! *arg* arg)
  (reset! *atom* atom)
  (.setText *pathText* path)
  (.setText *mapTitleText* maptitle)
  (.setSelected *negatedCheckBox* (not polarity))
  (.setSelectedItem *typeComboBox* (*type-to-str* type))
  *premiseProperties*)

(defn premise-being-edited-info []
  {:path (.getText *pathText*)
   :arg (deref *arg*)
   :id (deref *id*)
   :atom (deref *atom*)
   :previous-polarity (deref *previous-polarity*)
   :polarity (not (.isSelected *negatedCheckBox*))
   :previous-type (deref *previous-type*)
   :type (*str-to-type* (.getSelectedItem *typeComboBox*))})