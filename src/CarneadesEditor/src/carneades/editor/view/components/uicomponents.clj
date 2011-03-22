;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Clojure variables to hold instances of 
            the Swing components defined in the NetBeans project."}
  carneades.editor.view.components.uicomponents
  (:use clojure.contrib.def)
  (:import carneades.editor.uicomponents.EditorApplicationView))

(defvar *frame* (EditorApplicationView.))

(defvar *openFileButton* (.openFileButton *frame*))
(defvar *openFileMenuItem* (.openFileMenuItem *frame*))
(defvar *closeFileMenuItem* (.closeFileMenuItem *frame*))
(defvar *undoButton* (.undoButton *frame*))
(defvar *redoButton* (.redoButton *frame*))
(defvar *refreshButton* (.refreshButton *frame*))
(defvar *quitFileMenuItem* (.quitFileMenuItem *frame*))

(defvar *exportFileMenuItem* (.exportFileMenuItem *frame*))
(defvar *printPreviewFileMenuItem* (.printPreviewFileMenuItem *frame*))
(defvar *printFileMenuItem* (.printFileMenuItem *frame*))
(defvar *aboutHelpMenuItem* (.aboutHelpMenuItem *frame*))

(defvar *closeLkifFileMenuItem* (.closeLkifFileMenuItem *frame*))
(defvar *exportLkifFileMenuItem* (.exportLkifFileMenuItem *frame*))

(defvar *openGraphMenuItem* (.openGraphMenuItem *frame*))
(defvar *closeGraphMenuItem* (.closeGraphMenuItem *frame*))
(defvar *exportGraphMenuItem* (.exportGraphMenuItem *frame*))
(defvar *copyGraphMenuItem* (.copyGraphMenuItem *frame*))

(defvar *newPremiseMenuItem* (.newPremiseMenuItem *frame*))
(defvar *deleteArgumentMenuItem* (.deleteArgumentMenuItem *frame*))
(defvar *deletePremiseMenuItem* (.deletePremiseMenuItem *frame*))
(defvar *deleteStatementMenuItem* (.deleteStatementMenuItem *frame*))

(defvar *editStatementMenuItem* (.editStatementMenuItem *frame*))
(defvar *statedMenuItem* (.statedMenuItem *frame*))
(defvar *questionedMenuItem* (.questionedMenuItem *frame*))
(defvar *acceptedMenuItem* (.acceptedMenuItem *frame*))
(defvar *rejectedMenuItem* (.rejectedMenuItem *frame*))

(defvar *zoomInButton* (.zoomInButton *frame*))
(defvar *zoomOutButton* (.zoomOutButton *frame*))
(defvar *zoomResetButton* (.zoomResetButton *frame*))
(defvar *saveButton* (.saveButton *frame*))
(defvar *copyClipboardEditMenuItem* (.copyClipboardEditMenuItem *frame*))
(defvar *selectAllEditMenuItem* (.selectAllEditMenuItem *frame*))

(defvar *saveFileMenuItem* (.saveFileMenuItem *frame*))
(defvar *saveAsFileMenuItem* (.saveAsFileMenuItem *frame*))

(defvar *undoEditMenuItem* (.undoEditMenuItem *frame*))

(defvar *redoEditMenuItem* (.redoEditMenuItem *frame*))
(defvar *mainIssueMenuItem* (.mainIssueMenuItem *frame*))

(defvar *argumentPopupMenu* (.argumentPopupMenu *frame*))
(defvar *premisePopupMenu* (.premisePopupMenu *frame*))
(defvar *statementPopupMenu* (.statementPopupMenu *frame*))
(defvar *mapPopupMenu* (.mapPopupMenu *frame*))
(defvar *addExistingPremiseMenuItem* (.addExistingPremiseMenuItem *frame*))
(defvar *newStatementMenuItem* (.newStatementMenuItem *frame*))
(defvar *newArgumentMenuItem* (.newArgumentMenuItem *frame*))
(defvar *newGraphMenuItem* (.newGraphMenuItem *frame*))
(defvar *deleteGraphMenuItem* (.deleteGraphMenuItem *frame*))
(defvar *newFileMenuItem* (.newFileMenuItem *frame*))

(defvar *findGoalAssistantMenuItem* (.findGoalAssistantMenuItem *frame*))
(defvar *findArgumentsAssistantMenuItem* (.findArgumentsAssistantMenuItem *frame*))
(defvar *instantiateSchemeAssistantMenuItem* (.instantiateSchemeAssistantMenuItem *frame*))
(defvar *formalizeStatementAssistantMenuItem* (.formalizeStatementAssistantMenuItem *frame*))

(defvar *preferencesEditMenuItem* (.preferencesEditMenuItem *frame*))

(defvar *premisePremiseMenuItem* (.premisePremiseMenuItem *frame*))
(defvar *assumptionPremiseMenuItem* (.assumptionPremiseMenuItem *frame*))
(defvar *exceptionPremiseMenuItem* (.exceptionPremiseMenuItem *frame*))

(defvar *negatedPremiseMenuItem* (.negatedPremiseMenuItem *frame*))

(defvar *proArgumentMenuItem* (.proArgumentMenuItem *frame*))
(defvar *conArgumentMenuItem* (.conArgumentMenuItem *frame*))