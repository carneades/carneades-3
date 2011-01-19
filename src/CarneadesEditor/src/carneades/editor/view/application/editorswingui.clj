;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.application.editorswingui
  (:use clojure.contrib.def
        clojure.contrib.pprint
        clojure.contrib.swing-utils
        carneades.editor.view.application.editorswingui-helpers
        carneades.editor.view.dialogs.statement-editor
        carneades.editor.utils.swing
        carneades.editor.view.swinguiprotocol
        carneades.editor.view.components.uicomponents
        (carneades.editor.view.properties lkif graph statement argument premise)
        (carneades.editor.view.components tabs tree search)
        carneades.mapcomponent.map)
  (:require [carneades.editor.view.components.tree :as tree])
  (:import java.awt.BorderLayout
           java.awt.Toolkit
           (org.netbeans.spi.wizard WizardPage WizardPage$WizardResultProducer
                                    WizardObserver WizardBranchController)
           org.netbeans.api.wizard.WizardDisplayer
           (carneades.mapcomponent.map StatementCell ArgumentCell PremiseCell)))

(defvar- do-once (System/setProperty "wizard.sidebar.image" "carneades-bright.png"))

(deftype EditorSwingUI []
  SwingUI

  (add-close-button-listener
   [this f args]
   (register-close-button-listener f args))

  (add-open-file-button-listener
   [this f args]
   (apply add-action-listener *openFileButton* f args))

  (add-mousepressed-tree-listener
   [this f args]
   (apply add-mousepressed-listener *lkifsTree* f args))

  (add-mousepressed-searchresult-listener
   [this f args]
   (apply add-mousepressed-listener *searchResultTable* f args))

  (add-keyenter-searchresult-listener
   [this f args]
   (register-keyenter-searchresult-listener f args))

  (add-open-file-menuitem-listener
   [this f args]
   (apply add-action-listener *openFileMenuItem* f args))

  (add-close-file-menuitem-listener
   [this f args]
   (apply add-action-listener *closeFileMenuItem* f args))

  (add-export-lkif-filemenuitem-listener
   [this f args]
   (apply add-action-listener *exportLkifFileMenuItem* f args))

  (add-export-graph-menuitem-listener
   [this f args]
   (apply add-action-listener *exportGraphMenuItem* f args))

  (add-copy-graph-menuitem-listener
   [this f args]
   (apply add-action-listener *copyGraphMenuItem* f args))

  (add-about-helpmenuitem-listener
   [this f args]
   (apply add-action-listener *aboutHelpMenuItem* f args))

  (add-printpreview-filemenuitem-listener
   [this f args]
   (apply add-action-listener *printPreviewFileMenuItem* f args))

  (add-close-lkif-filemenuitem-listener
   [this f args]
   (apply add-action-listener *closeLkifFileMenuItem* f args))

  (add-export-filemenuitem-listener
   [this f args]
   (apply add-action-listener *exportFileMenuItem* f args))

  (add-save-filemenuitem-listener
   [this f args]
   (apply add-action-listener *saveFileMenuItem* f args))
  
  (add-saveas-filemenuitem-listener
   [this f args]
   (apply add-action-listener *saveAsFileMenuItem* f args))

  (add-undo-editmenuitem-listener
   [this f args]
   (apply add-action-listener *undoEditMenuItem* f args))

  (add-redo-editmenuitem-listener
   [this f args]
   (apply add-action-listener *redoEditMenuItem* f args))
  
  (add-open-graph-menuitem-listener
   [this f args]
   (apply add-action-listener *openGraphMenuItem* f args))

  (add-close-graph-menuitem-listener
   [this f args]
   (apply add-action-listener *closeGraphMenuItem* f args))

  (add-print-filemenuitem-listener
   [this f args]
   (apply add-action-listener *printFileMenuItem* f args))

  (add-delete-premise-menuitem-listener
   [this f args]
   (apply add-action-listener *deletePremiseMenuItem* f args))

  (add-delete-argument-menuitem-listener
   [this f args]
   (apply add-action-listener *deleteArgumentMenuItem* f args))

  (add-edit-statement-menuitem-listener
   [this f args]
   (apply add-action-listener *editStatementMenuItem* f args))

  (add-delete-statement-menuitem-listener
   [this f args]
   (apply add-action-listener *deleteStatementMenuItem* f args))

  (add-new-premise-menuitem-listener
   [this f args]
   (apply add-action-listener *newPremiseMenuItem* f args))
  
  (add-searchresult-selection-listener
   [this f args]
   (register-searchresult-selection-listener f args))

  (add-statement-edit-listener
   [this f args]
   (register-statement-edit-listener f args))

  ;; (add-statement-editor-listener
  ;;  [this f args]
  ;;  (register-statement-editor-listener f args))

  (add-title-edit-listener
   [this f args]
   (register-graph-edit-listener f args))

  (get-graph-being-edited-info
   [this]
   (graph-being-edited-info))

  (get-argument-being-edited-info
   [this]
   (argument-being-edited-info))
  
  (add-statement-edit-status-listener
   [this f args]
   (apply add-action-listener *statementStatusComboBox* f args))

  (add-statement-edit-proofstandard-listener
   [this f args]
   (apply add-action-listener *statementProofstandardComboBox* f args))

  (add-premise-edit-type-listener
   [this f args]
   (apply add-action-listener *typeComboBox* f args))

  (add-premise-edit-role-listener
   [this f args]
   (apply add-action-listener *roleText* f args))

  (add-argument-edit-title-listener
   [this f args]
   (apply add-action-listener *titleText* f args))
  
  (add-argument-edit-weight-listener
   [this f args]
   (register-argument-weight-listener f args))

  (add-argument-edit-direction-listener
   [this f args]
   (apply add-action-listener *proButton* f args)
   (apply add-action-listener *conButton* f args))

  (add-argument-edit-scheme-listener
   [this f args]
   (apply add-action-listener *schemeText* f args))
  
  (add-undo-button-listener
   [this f args]
   (apply add-action-listener *undoButton* f args))

  (add-refresh-button-listener
   [this f args]
   (apply add-action-listener *refreshButton* f args))
  
  (add-redo-button-listener
   [this f args]
   (apply add-action-listener *redoButton* f args))

  (add-save-button-listener
   [this f args]
   (apply add-action-listener *saveButton* f args))

  (add-copyclipboard-button-listener
   [this f args]
   (apply add-action-listener *copyClipboardEditMenuItem* f args))

  (add-preferences-editmenuitem-listener
   [this f args]
   (apply add-action-listener *preferencesEditMenuItem* f args))
  
  (add-premise-edit-polarity-listener
   [this f args]
   (apply add-action-listener *negatedCheckBox* f args))

  (add-mainissue-menuitem-listener
   [this f args]
   (apply add-action-listener *mainIssueMenuItem* f args))

  (add-new-statement-menuitem-listener
   [this f args]
   (apply add-action-listener *newStatementMenuItem* f args))

  (add-new-argument-menuitem-listener
   [this f args]
   (apply add-action-listener *newArgumentMenuItem* f args))

  (add-new-graph-menuitem-listener
   [this f args]
   (apply add-action-listener *newGraphMenuItem* f args))

  (add-delete-graph-menuitem-listener
   [this f args]
   (apply add-action-listener *deleteGraphMenuItem* f args))

  (add-new-file-menuitem-listener
   [this f args]
   (apply add-action-listener *newFileMenuItem* f args))

  (add-windowclosing-listener
   [this f args]
   (apply add-windowclose-listener *frame* f args))

  (add-findgoal-assistantmenuitem-listener
   [this f args]
   (apply add-action-listener *findGoalAssistantMenuItem* f args))

  (add-findarguments-assistantmenuitem-listener
   [this f args]
   (apply add-action-listener *findArgumentsAssistantMenuItem* f args))

  (add-instantiatescheme-assistantmenuitem-listener
   [this f args]
   (apply add-action-listener *instantiateSchemeAssistantMenuItem* f args))

  (add-formalizestatement-assistantmenuitem-listener
   [this f args]
   (apply add-action-listener *formalizeStatementAssistantMenuItem* f args))
  
  (add-quit-filemenuitem-listener
   [this f args]
   (apply add-action-listener *quitFileMenuItem* f args))

  (add-import-button-listener
   [this f args]
   (register-import-button-listener f args))

  (add-remove-import-button-listener
   [this f args]
   (register-remove-import-button-listener f args))

  (add-stated-menuitem-listener
   [this f args]
   (apply add-action-listener *statedMenuItem* f args))
  
  (add-questioned-menuitem-listener
   [this f args]
   (apply add-action-listener *questionedMenuItem* f args))
  
  (add-accepted-menuitem-listener
   [this f args]
   (apply add-action-listener *acceptedMenuItem* f args))
  
  (add-rejected-menuitem-listener
   [this f args]
   (apply add-action-listener *rejectedMenuItem* f args))
  
  (get-statement-being-edited-info
   [this]
   (statement-being-edited-info))
  
  (get-selected-object-in-tree
   [this]
   (tree/selected-object))

  (get-selected-object-in-search-result
   [this]
   (selected-object-in-search-result))
  
  (get-graphinfo-being-closed
   [this event]
   (graphinfo-being-closed event))

  (get-statement-being-edited-menu-info
   [this]
   (deref *statement-being-edited-menu-info*))
  
  (get-premise-being-edited-info
   [this]
   (premise-being-edited-info))

  (get-lkif-being-edited-info
   [this]
   (lkif-being-edited-info))

  (get-selected-node
   [this path id]
   (when-let [component (get-component path id)]
     (when-let [obj (current-selected-object component)]
       (cond (instance? PremiseCell obj)
             {:arg (:arg obj) :pm (:pm obj)}

             (instance? ArgumentCell obj)
             (:arg obj)

             (instance? StatementCell obj)
             (:stmt obj)))))

  (create-wizard
   [this title panels]
   (create-wizard this title panels nil nil))

  (create-wizard
   [this title panels cancel-fn args]
   "cancel-fn must return true when cancel is possible
    cancel-fn is called with (apply cancel-fn settings args)

    panels is a vector of panel. A panel is a structmap with the following keys,
    :panel the Swing JPanel, :desc a string describing the panel 
    :id an optional string id, used for branched wizards
    :validator a validator function
    :args the argument to pass to the validator and the listener in addition to
    the first settings argument

    NOTE that the :listener of the first panel will not be called
    when the wizard is displayed, only when the user goes back
    from the second panel"
   (let [wizardpages (create-wizardpages panels)
         producer (reify WizardPage$WizardResultProducer
                    (finish [this data]
                            data)
                    (cancel [this settings]
                            (if cancel-fn
                              (apply cancel-fn settings args)
                              true)))
         wizard (WizardPage/createWizard title
                                         (into-array (map :page wizardpages)) producer)]
     (.addWizardObserver wizard (create-wizardobserver wizardpages))
     wizard))

  (display-wizard
   [this wizard]
   (WizardDisplayer/showWizard wizard))

  (display-wizard
   [this title panels]
   (display-wizard this (create-wizard this title panels)))

  (display-wizard
   [this wizard height width]
   (let [w width
         l height
         size (.getScreenSize (Toolkit/getDefaultToolkit))
         x (int (/ (- (.width size) width) 2))
         y (int (/ (- (.height size) height) 2))]
     (WizardDisplayer/showWizard wizard (java.awt.Rectangle. x y width height))))

  (display-branched-wizard
   [this basepanels selector args]
   (let [wizardpages (create-wizardpages basepanels)
         pages (into-array (map :page wizardpages))]
     (let [brancher (proxy [WizardBranchController] [pages]
                      (getWizardForStep
                       [step settings]
                       (apply selector settings step args)))
           wizard (.createWizard brancher)]
       (.addWizardObserver wizard (create-wizardobserver wizardpages))
       (display-wizard this wizard))))
)

