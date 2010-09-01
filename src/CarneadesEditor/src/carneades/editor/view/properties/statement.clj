;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.properties.statement
  (:use clojure.contrib.def
        clojure.contrib.swing-utils)
  (:import carneades.editor.uicomponents.StatementPropertiesView))

(defvar- *statementProperties* (StatementPropertiesView/instance))
(defvar- *statementTextArea* (.statementTextArea *statementProperties*))
(defvar- *statusComboBox* (.statusComboBox *statementProperties*))
(defvar- *proofstandardComboBox* (.proofstandardComboBox *statementProperties*))
(defvar- *acceptableCheckBox* (.acceptableCheckBox *statementProperties*))
(defvar- *complementacceptableCheckBox* (.complementacceptableCheckBox *statementProperties*))

(defvar- *mapTitleText* (.mapTitleText *statementProperties*))
(defvar- *pathText* (.pathText *statementProperties*))

(defvar- *statuses* {:stated "Stated"
                     :accepted "Accepted"
                     :rejected "Rejected"
                     :questioned "Questioned"})

(defvar- *proofstandards* {:ba "Preponderance of Evidence"
                           :pe "Preponderance of Evidence"
                           :brd "Beyond Reasonable Doubt"
                           :cce "Clear and Convincing Evidence"
                           :dv "Dialectical Validity"
                           :se "Scintilla of Evidence"})

(defn- acceptable-checkbox-listener [event]
  ;; there is no read-only checkboxes in Swing, we cancel the change to make
  ;; a read-only-like checkbox:
  (.setSelected *acceptableCheckBox* (not (.isSelected *acceptableCheckBox*))))

(defn- complementacceptable-checkbox-listener [event]
  (.setSelected *complementacceptableCheckBox*
                (not (.isSelected *complementacceptableCheckBox*))))

(defn init-statement-properties []
  (StatementPropertiesView/reset)
  (add-action-listener *acceptableCheckBox* acceptable-checkbox-listener)
  (add-action-listener *complementacceptableCheckBox*
                       complementacceptable-checkbox-listener))

(defn get-statement-properties-panel [path maptitle stmt status proofstandard acceptable complement-acceptable]
  (.setText *pathText* path)
  (.setText *mapTitleText* maptitle)
  (.setText *statementTextArea* stmt)
  (.setSelectedItem *statusComboBox* (get *statuses* status))
  (.setSelectedItem *proofstandardComboBox* (get *proofstandards* proofstandard))
  (.setSelected *acceptableCheckBox* acceptable)
  (.setSelected *complementacceptableCheckBox* complement-acceptable)
  *statementProperties*)

