;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.viewprotocol)

;; defines functions that must be implemented by the UI
;; and are independant of a specific GUI library

(defprotocol View
  (init [this] "init the view")
  (show [this] "display the main view, take the command lines arguments
                       as second argument")
  (open-graph [this path ag stmt-fmt] "open the graph for edition")
  (redisplay-graph [this path ag stmt-fmt])
  (close-graph [this path id])
  (current-graph [this] "returns [path id] for the graph currently edited")
  (ask-lkif-file-to-open [this] "ask the user the LKIF file to open. 
                                 Returns File or nil")
  (ask-file-to-save [this descriptions suggested])
  (export-graph-to-svg [this ag stmt-fmt filename])
  (export-graph-to-dot [this ag statement-formatted filename])
  (export-graph-to-graphviz-svg [this ag statement-formatted filename])
  (display-lkif-content [this file graphinfos]
                        "display information relative to an LKIF file. 
                         graphinfos is a seq of [id title] ")
  (hide-lkif-content [this path])
  (print-preview [this path ag stmt-fmt])
  (print-graph [this path ag stmt-fmt])
  (display-lkif-property [this path])
  (display-graph-property [this path id title mainissue])
  (display-about [this])
  (ask-confirmation [this title content])
  (display-error [this title content])
  (display-statement-property
   [this path id maptitle stmt stmt-fmt status proofstandard acceptable complement-acceptable])
  (display-premise-property
   [this path id maptitle arg polarity type atom])
  (display-argument-property
   [this path id maptitle argid title applicable weight direction scheme])
  (display-search-state [this inprogress])
  (display-statement-search-result
   [this path id stmt stmt-fmt])
  (display-argument-search-result
   [this path id arg title])
  (display-statement
   [this path ag stmt stmt-fmt])
  (display-argument
   [this path ag arg stmt-fmt])
  (statement-content-changed
   [this path ag oldstmt newstmt])

  ;; notifications:
  ;; these fine grained modifications avoid to redisplay the whole
  ;; argument graph each time, which takes too much time with
  ;; big graphs
  (statement-status-changed
   [this path ag stmt])
  (statement-proofstandard-changed
   [this path ag stmt])
  (title-changed [this path ag title])
  (premise-polarity-changed [this path ag old arg pm])
  (premise-type-changed [this path ag oldarg arg pm])
  (argument-title-changed [this path ag arg title])
  (argument-weight-changed [this path ag arg weight])
  (argument-direction-changed [this path ag arg direction])
  (premise-added [this path ag arg stmt])
  
  (set-busy
   [this isbusy])
  (edit-undone [this path id])
  (edit-redone [this path id])
  (set-can-undo [this path id state])
  (set-can-redo [this path id state])
  (set-dirty [this path ag state])
  (copyselection-clipboard [this path id])
  
  
  ;; non-swing listeners:
  (register-statement-selection-listener [this l args])
  (register-argument-selection-listener [this l args])
  (register-premise-selection-listener [this l args])
  (register-search-listener [this l args]
                            "calls l with searchinfo searchbegins args")
  (register-add-existing-premise-listener [this l args] "calls l with view path id arg stmt args")
  )