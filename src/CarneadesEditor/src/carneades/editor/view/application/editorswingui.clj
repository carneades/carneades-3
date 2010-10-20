;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.application.editorswingui
  (:use clojure.contrib.swing-utils
        carneades.editor.utils.swing
        carneades.editor.view.swinguiprotocol
        carneades.editor.view.components.uicomponents
        (carneades.editor.view.properties statement graph argument premise)
        (carneades.editor.view.components tabs tree search)
        carneades.mapcomponent.map)
  (:import (carneades.mapcomponent.map StatementCell ArgumentCell PremiseCell)))

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

  (add-open-file-menuitem-listener [this f args]
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
  
  (get-statement-being-edited-info
   [this]
   (statement-being-edited-info))
  
  (get-selected-object-in-tree
   [this]
   (selected-object-in-tree))

  (get-selected-object-in-search-result
   [this]
   (selected-object-in-search-result))
  
  (get-graphinfo-being-closed
   [this event]
   (graphinfo-being-closed event))

  (get-premise-being-edited-info
   [this]
   (premise-being-edited-info))

  (get-selected-node
   [this path id]
   (when-let [component (get-component path id)]
     (when-let [obj (current-selected-object component)]
       (cond (instance? PremiseCell obj)
             {:arg (:arg obj) :pm (:pm obj)}

             (instance? ArgumentCell obj)
             (:arg obj)

             (instance? StatementCell obj)
             (:stmt obj)
             )
       )))
  
  
  
  
  
  )