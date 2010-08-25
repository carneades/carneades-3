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
  (ask-file-to-save [this description extension suggested])
  (export-graph-to-svg [this ag stmt-fmt filename])
  (display-lkif-content [this file graphids]
                        "display information relative to an LKIF file")
  (hide-lkif-content [this path])
  (print-preview [this path ag stmt-fmt])
  (print-graph [this path ag stmt-fmt])
  (display-lkif-property [this path])
  (display-graph-property [this id title mainissue])
  (display-about [this])
  (ask-confirmation [this title content])
  (display-error [this title content]))