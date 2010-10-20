;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.application.wizards.goal
  (:use clojure.contrib.def
        [clojure.string :only (join)]
        carneades.editor.view.wizardsprotocol)
  (:import (carneades.editor.uicomponents.wizards.goal ProponentPanel
                                                       AbductionPanel
                                                       StatementPanel)))

(defvar- *proponentPanel* (ProponentPanel/instance))
(defvar- *mainIssueTextArea* (.mainIssueTextArea *proponentPanel*))

(defvar- *abductionPanel* (AbductionPanel/instance))
(defvar- *positionsList* (.positionsList *abductionPanel*))

(defvar- *statementPanel* (StatementPanel/instance))
(defvar- *statementsList* (.statementsList *statementPanel*))

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

  (get-statements-panel
   [this]
   *statementPanel*)

  (display-abduction-result
   [this positions statement-formatted]
   (prn "positions =")
   (prn positions)
   (let [data (map #(str (join ", " (concat (map statement-formatted (drop-last %))))
                         (statement-formatted (last %)))
                   positions)]
     (prn "data =")
     (prn data)
    (.setListData *positionsList*
                  (to-array data))))

  (display-statements
   [this statements statement-formatted]
   (.setListData *statementsList*
                 (to-array (map #(statement-formatted %) statements)))
   )
  )

