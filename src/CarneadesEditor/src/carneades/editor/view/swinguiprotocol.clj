;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.swinguiprotocol)

;; defines protocols and records required for the swing_listeners

(defprotocol SwingUI
  ;; functions to register some Swing listeners:
  (add-close-button-listener [this f args])
  (add-open-file-button-listener [this f args])
  (add-mousepressed-tree-listener [this f args])
  (add-mousepressed-searchresult-listener [this f args])
  (add-keyenter-searchresult-listener [this f args])
  (add-open-file-menuitem-listener [this f args])
  (add-close-file-menuitem-listener [this f args])
  (add-export-lkif-filemenuitem-listener [this f args])
  (add-export-graph-menuitem-listener [this f args])
  (add-copy-graph-menuitem-listener [this f args])
  (add-export-filemenuitem-listener [this f args])
  (add-about-helpmenuitem-listener [this f args])
  (add-printpreview-filemenuitem-listener [this f args])
  (add-close-lkif-filemenuitem-listener [this f args])
  (add-open-graph-menuitem-listener [this f args])
  (add-close-graph-menuitem-listener [this f args])
  (add-print-filemenuitem-listener [this f args])
  (add-searchresult-selection-listener [this f args])
  (add-undo-button-listener [this f args])
  (add-redo-button-listener [this f args])
  (add-save-button-listener [this f args])
  (add-refresh-button-listener [this f args])
  (add-undo-editmenuitem-listener [this f args])
  (add-redo-editmenuitem-listener [this f args])
  (add-save-filemenuitem-listener [this f args])
  (add-saveas-filemenuitem-listener [this f args])
  (add-copyclipboard-button-listener [this f args])
  (add-preferences-editmenuitem-listener [this f args])
  (add-delete-premise-menuitem-listener [this f args])
  (add-delete-statement-menuitem-listener [this f args])
  (add-edit-statement-menuitem-listener [this f args])
  (add-delete-argument-menuitem-listener [this f args])
  (add-mainissue-menuitem-listener [this f args])
  (add-premise-premisemenuitem-listener [this f args])
  (add-assumption-premisemenuitem-listener [this f args])
  (add-exception-premisemenuitem-listener [this f args])
  (add-negated-premisemenuitem-listener [this f args])
  (add-new-statement-menuitem-listener [this f args])
  (add-new-argument-menuitem-listener [this f args])
  (add-new-premise-menuitem-listener [this f args])
  (add-new-graph-menuitem-listener [this f args])
  (add-delete-graph-menuitem-listener [this f args])
  (add-new-file-menuitem-listener [this f args])
  (add-windowclosing-listener [this f args])
  (add-findgoal-assistantmenuitem-listener [this f args])
  (add-findarguments-assistantmenuitem-listener [this f args])
  (add-instantiatescheme-assistantmenuitem-listener [this f args])
  (add-formalizestatement-assistantmenuitem-listener [this f args])
  (add-quit-filemenuitem-listener [this f args])
  (add-import-button-listener [this f args])
  (add-remove-import-button-listener [this f args])
  (add-stated-menuitem-listener [this f args])
  (add-questioned-menuitem-listener [this f args])
  (add-accepted-menuitem-listener [this f args])
  (add-rejected-menuitem-listener [this f args])
  
  ;; properties edit listeners
  ;; (add-statement-editor-listener [this f args])
  (add-statement-edit-listener [this f args])
  (add-title-edit-listener [this f args])
  (add-statement-edit-status-listener [this f args])
  (add-statement-edit-proofstandard-listener [this f args])
  (add-premise-edit-polarity-listener [this f args])
  (add-premise-edit-type-listener [this f args])
  (add-premise-edit-role-listener [this f args])
  (add-argument-edit-title-listener [this f args])
  (add-argument-edit-weight-listener [this f args])
  (add-argument-edit-direction-listener [this f args])
  (add-argument-edit-scheme-listener [this f args])
  
  ;; functions to get information from the Swing UI
  (get-selected-object-in-tree [this])
  (get-selected-object-in-search-result [this])
  (get-graphinfo-being-closed [this event])
  (get-statement-being-edited-info [this])
  (get-statement-being-edited-menu-info [this])
  (get-premise-being-edited-menu-info [this])
  (get-graph-being-edited-info [this])
  (get-premise-being-edited-info [this])
  (get-argument-being-edited-info [this])
  (get-lkif-being-edited-info [this])
  (get-selected-node [this path id])

  ;; wizards:
  (create-wizard [this title panels]
                 [this title panels cancel-fn args])
  (display-wizard [this wizard]
                  [this title panels]
                  [this wizard height width])
  (display-branched-wizard [this basepanels selector args])
  )

;; records stored in the element of the tree:
(defrecord LkifFileInfo [path filename dirty] Object
  (toString
   [this]
   (str (when dirty "*") filename)))

(defrecord GraphInfo [lkifinfo id title dirty] Object
  (toString
   [this]
   (if (empty? title)
     (str (when dirty "*") (format "%s [title missing]" id))
     (str (when dirty "*") title))))

;; stored in the search result:
(defrecord StatementInfo [path id stmt stmt-fmt]
  Object
  (toString
   [this]
   (str (stmt-fmt stmt))))

(defrecord ArgumentInfo [path id arg title]
  Object
  (toString
   [this]
   title))