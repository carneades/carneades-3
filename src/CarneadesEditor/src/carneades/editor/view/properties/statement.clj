;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.properties.statement
  (:use clojure.contrib.def
        clojure.contrib.swing-utils
        carneades.editor.utils.seq)
  (:import carneades.editor.uicomponents.StatementPropertiesView))

(defvar- *statementProperties* (StatementPropertiesView/instance))
(defvar- *statementTextArea* (.statementTextArea *statementProperties*))
(defvar *statementEditButton* (.editButton *statementProperties*))
(defvar *statementStatusComboBox* (.statusComboBox *statementProperties*))
(defvar *statementProofstandardComboBox* (.proofstandardComboBox *statementProperties*))
(defvar- *acceptableCheckBox* (.acceptableCheckBox *statementProperties*))
(defvar- *complementacceptableCheckBox* (.complementacceptableCheckBox *statementProperties*))

(defvar- *mapTitleText* (.mapTitleText *statementProperties*))
(defvar- *pathText* (.pathText *statementProperties*))

(defvar- *statuses* {:stated "Stated"
                     :accepted "Accepted"
                     :rejected "Rejected"
                     :questioned "Questioned"})

(defvar- *txt-to-status* (reverse-map *statuses*))

(defvar- *proofstandards* {;; :ba "Preponderance of Evidence"
                           :pe "Preponderance of Evidence"
                           :brd "Beyond Reasonable Doubt"
                           :cce "Clear and Convincing Evidence"
                           :dv "Dialectical Validity"
                           :se "Scintilla of Evidence"})

(defvar- *txt-to-proofstandard* (reverse-map *proofstandards*))

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

(defvar- *previous-statement-content* (atom {}))

(defn get-statement-properties-panel [path id maptitle stmt stmt-str status proofstandard acceptable complement-acceptable]
  (.setText *pathText* path)
  (.setText *mapTitleText* maptitle)
  (.setText *statementTextArea* (stmt-str stmt))
  (reset! *previous-statement-content* {:path path :id id :previous-content stmt
                                        :previous-status status
                                        :previous-proofstandard proofstandard})
  (.setSelectedItem *statementStatusComboBox* (get *statuses* status))
  (.setSelectedItem *statementProofstandardComboBox* (get *proofstandards* proofstandard))
  (.setSelected *acceptableCheckBox* acceptable)
  (.setSelected *complementacceptableCheckBox* complement-acceptable)
  *statementProperties*)

(defn statement-being-edited-info []
  (prn "selected proofstandard =")
  (prn (.getSelectedItem *statementProofstandardComboBox*))
  (merge {:content (.getText *statementTextArea*)
          :status (*txt-to-status* (.getSelectedItem *statementStatusComboBox*))
          :proofstandard (*txt-to-proofstandard* (.getSelectedItem *statementProofstandardComboBox*))}
         (deref *previous-statement-content*)))
