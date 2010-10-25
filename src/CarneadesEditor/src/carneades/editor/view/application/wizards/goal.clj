;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.application.wizards.goal
  (:use clojure.contrib.def
        clojure.contrib.swing-utils
        carneades.editor.view.wizardsprotocol
        carneades.editor.view.application.wizards.messages
        carneades.editor.utils.swing)
  (:require [clojure.string :as str])
  (:import carneades.editor.view.wizardsprotocol.StatementItem
           (carneades.editor.uicomponents.wizards.goal ProponentPanel
                                                       AbductionPanel)))

(defvar- *proponentPanel* (ProponentPanel/instance))
(defvar- *mainIssueTextArea* (.mainIssueTextArea *proponentPanel*))

(defvar- *abductionPanel* (AbductionPanel/instance))
(defvar- *statementList* (.statementsList *abductionPanel*))
(defvar- *positionLabel* (.positionLabel *abductionPanel*))

(defvar- *firstPositionButton* (.firstPositionButton *abductionPanel*))
(defvar- *lastPositionButton* (.lastPositionButton *abductionPanel*))
(defvar- *previousPositionButton* (.previousPositionButton *abductionPanel*))
(defvar- *nextPositionButton* (.nextPositionButton *abductionPanel*))

(deftype EditorSwingGoalWizard []
  SwingGoalWizard
  (get-proponent-panel
   [this]
   *proponentPanel*)

  (get-abduction-panel
   [this]
   *abductionPanel*)

  (set-main-issue
   [this mainissue]
   (.setText *mainIssueTextArea* mainissue))

  (reset-position
   [this]
   (.setText *positionLabel* *position*)
   (.setListData *statementList* (to-array [""]))
   (.setSelectedIndex *statementList* 0))
  
  (display-position
   [this position posindex nbpos statement-formatted]
   (let [items (to-array (map #(StatementItem.
                                %
                                (str "<html>"
                                     (str/replace (statement-formatted %) "\n" "<br>")
                                     "<br>&nbsp"))
                              position))]
     (.setListData *statementList* items)
     (.setSelectedIndex *statementList* 0)
     (.setText *positionLabel* (format *position-n-of* (inc posindex) nbpos))))

  (set-first-position-button-listener
   [this f args]
   (remove-action-listeners *firstPositionButton*)
   (apply add-action-listener *firstPositionButton* f args))
  
  (set-last-position-button-listener
   [this f args]
   (remove-action-listeners *lastPositionButton*)
   (apply add-action-listener *lastPositionButton* f args))
  
  (set-previous-position-button-listener
   [this f args]
   (remove-action-listeners *previousPositionButton*)
   (apply add-action-listener *previousPositionButton* f args))
  
  (set-next-position-button-listener
   [this f args]
   (remove-action-listeners *nextPositionButton*)
   (apply add-action-listener *nextPositionButton* f args))
  
  )

