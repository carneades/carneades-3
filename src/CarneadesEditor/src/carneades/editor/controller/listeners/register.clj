;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.controller.listeners.register
  (:use clojure.contrib.def
        clojure.contrib.swing-utils
        carneades.editor.view.viewprotocol
        carneades.editor.view.swinguiprotocol
        carneades.editor.controller.listeners.swing-listeners
        carneades.editor.controller.listeners.swing-wizards-listeners
        carneades.editor.controller.handlers.handlers
        carneades.editor.utils.swing))

;;
;; For the seperation of concerns, we follow here the MVC pattern,
;; with the controller acting as a mediator between the View and the Model.
;; The View does not have direct access to the Model.
;;
;; http://java.sun.com/developer/technicalArticles/javase/mvc/
;;
;;
;; This namespace directly access the GUI to register Swing listeners and
;; dispatch the calls in an UI-independent way to the listeners in listeners.clj
;;
;; This is the only namespace, with the swing-listeners,
;; that should be given direct access to the Swing UI and only through
;; the SwingUI protocol.
;;
;; All other accesses must be made within listeners.clj
;; and only through the View protocol.
;;
;; This allow to keep the model and the listeners logic independant
;; from the specific Swing GUI implementation.
;;

(defn register-listeners [view]
  ;; we need to extract some information from the UI,
  ;; dispatch to the swing_listeners:
  (add-close-graph-menuitem-listener view close-graph-listener [view])
  (add-open-graph-menuitem-listener view open-graph-listener [view])
  (add-close-lkif-filemenuitem-listener view close-file-listener [view])
  (add-printpreview-filemenuitem-listener view printpreview-listener [view])
  (add-print-filemenuitem-listener view print-listener [view])
  (add-export-graph-menuitem-listener view export-element-listener [view])
  (add-copy-graph-menuitem-listener view copy-graph-listener [view])
  (add-export-lkif-filemenuitem-listener view export-element-listener [view])
  (add-export-filemenuitem-listener view export-file-listener [view])
  (add-close-file-menuitem-listener view close-file-listener [view])
  (add-close-button-listener view close-button-listener [view])
  (add-mousepressed-tree-listener view mouse-click-in-tree-listener [view])
  (add-searchresult-selection-listener view search-result-selection-listener
                                       [view])
  (add-mousepressed-searchresult-listener view mouse-click-in-searchresult
                                          [view])
  (add-keyenter-searchresult-listener view keyenter-in-searchresult
                                          [view])

  ;; properties listeners:
  (add-statement-editor-listener view statement-editor-listener [view])
  (add-statement-edit-listener view statement-button-edit-listener [view])
  (add-statement-edit-status-listener view statement-status-edit-listener [view])
  (add-statement-edit-proofstandard-listener view statement-proofstandard-edit-listener [view])
  (add-title-edit-listener view title-edit-listener [view])
  (add-premise-edit-polarity-listener view premise-edit-polarity-listener [view])
  (add-premise-edit-type-listener view premise-edit-type-listener [view])
  (add-premise-edit-role-listener view premise-edit-role-listener [view])
  (add-argument-edit-title-listener view argument-edit-title-listener [view])
  (add-argument-edit-weight-listener view argument-edit-weight-listener [view])
  (add-argument-edit-direction-listener view argument-edit-direction-listener [view])

  (add-undo-button-listener view undo-button-listener [view])
  (add-redo-button-listener view redo-button-listener [view])
  (add-save-button-listener view save-button-listener [view])
  (add-refresh-button-listener view refresh-button-listener [view])
  (add-copyclipboard-button-listener view copyclipboard-button-listener [view])
  (add-save-filemenuitem-listener view save-filemenuitem-listener [view])
  (add-saveas-filemenuitem-listener view saveas-filemenuitem-listener [view])
  (add-undo-editmenuitem-listener view undo-editmenuitem-listener [view])
  (add-redo-editmenuitem-listener view redo-editmenuitem-listener [view])
  (add-delete-premise-menuitem-listener view delete-premise-menuitem-listener [view])
  (add-new-premise-menuitem-listener view new-premise-menuitem-listener [view])
  (add-delete-statement-menuitem-listener view delete-statement-menuitem-listener [view])
  (add-delete-argument-menuitem-listener view delete-argument-menuitem-listener [view])
  (add-mainissue-menuitem-listener view mainissue-menuitem-listener [view])
  (add-new-statement-menuitem-listener view new-statement-menuitem-listener [view])
  (add-new-argument-menuitem-listener view new-argument-menuitem-listener [view])
  (add-new-graph-menuitem-listener view new-graph-menuitem-listener [view])
  (add-delete-graph-menuitem-listener view delete-graph-menuitem-listener [view])
  (add-new-file-menuitem-listener view new-file-menuitem-listener [view])
  (add-windowclosing-listener view windowclosing-listener [view])
  (add-quit-filemenuitem-listener view quit-filemenuitem-listener [view])
  (add-findgoal-assistantmenuitem-listener view findgoal-assistantmenuitem-listener [view])
  (add-findarguments-assistantmenuitem-listener
   view findarguments-assistantmenuitem-listener [view])
  (add-instantiatescheme-assistantmenuitem-listener
   view instantiatescheme-assistantmenuitem-listener [view])
  
  (add-import-button-listener view import-button-listener [view])
  (add-remove-import-button-listener view remove-import-button-listener [view])
  (add-stated-menuitem-listener view stated-menuitem-listener [view])
  (add-questioned-menuitem-listener view questioned-menuitem-listener [view])
  (add-accepted-menuitem-listener view accepted-menuitem-listener [view])
  (add-rejected-menuitem-listener view rejected-menuitem-listener [view])
 
  ;; we don't need to extract information from the UI,
  ;; dispatch to the listeners:
  (add-about-helpmenuitem-listener view (fn [event] (on-about view)) [])
  (add-open-file-menuitem-listener view (fn [event] (on-open-file view)) [])
  (add-open-file-button-listener view (fn [event] (on-open-file view)) [])

  ;; non-swing listeners:
  (register-statement-selection-listener view on-select-statement [view])
  (register-argument-selection-listener view on-select-argument [view])
  (register-premise-selection-listener view on-select-premise [view])
  (register-search-listener view (fn [inprogress searchinfo]
                                   (if inprogress
                                     (on-search-begins view searchinfo)
                                     (on-search-ends view))) [])
  (register-add-existing-premise-listener view on-add-existing-premise [])
  
)