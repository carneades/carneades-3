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
  (close-graph [this path id])
  (current-graph [this] "returns [path id] for the graph currently edited")
  (ask-lkif-file-to-open [this] "ask the user the LKIF file to open. 
                                 Returns File or nil")
  (ask-file-to-save [this description extensions suggested])
  (export-graph-to-svg [this ag stmt-fmt filename])
  (display-lkif-content [this file graphinfos]
                        "display information relative to an LKIF file. 
                         graphinfos is a seq of [id title] ")
  (hide-lkif-content [this path])
  (print-preview [this path ag stmt-fmt])
  (print-graph [this path ag stmt-fmt])
  (display-lkif-property [this path])
  (display-graph-property [this path title mainissue])
  (display-about [this])
  (ask-confirmation [this title content])
  (display-error [this title content])
  (display-statement-property
   [this path maptitle stmt status proofstandard acceptable complement-acceptable])
  (display-premise-property
   [this path maptitle polarity type])
  (display-argument-property
   [this path maptitle title applicable weight direction scheme])
  (display-search-state [this inprogress])
  (display-statement-search-result
   [this path id stmt stmt-fmt])
  (display-statement
   [this path ag stmt stmt-fmt])
  (set-busy
   [this isbusy])
  
  ;; non-swing listeners:
  (register-statement-selection-listener [this l args])
  (register-argument-selection-listener [this l args])
  (register-premise-selection-listener [this l args])
  (register-search-listener [this l args]
                            "calls l with s earchinfo searchbegins args")
  )