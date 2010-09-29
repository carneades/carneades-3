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

(defvar- *type-to-str* {:carneades.engine.argument/ordinary-premise "Premise"
                        :carneades.engine.argument/assumption "Assumption"
                        :carneades.engine.argument/exception "Exception"})

(defvar- *str-to-type* (reverse-map *type-to-str*))

(defvar- *previous-premise-content* (atom {}))

(defn get-premise-properties-panel [path id maptitle arg polarity type atom]
  (reset! *previous-premise-content* {:id id
                                      :previous-polarity polarity
                                      :previous-type type
                                      :arg arg
                                      :atom atom})
  (.setText *pathText* path)
  (.setText *mapTitleText* maptitle)
  (.setSelected *negatedCheckBox* (not polarity))
  (.setSelectedItem *typeComboBox* (*type-to-str* type))
  *premiseProperties*)

(defn premise-being-edited-info []
  (merge
   {:path (.getText *pathText*)
    :polarity (not (.isSelected *negatedCheckBox*))
    :type (*str-to-type* (.getSelectedItem *typeComboBox*))}
   (deref *previous-premise-content*)))