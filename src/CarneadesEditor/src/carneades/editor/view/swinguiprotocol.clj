;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.swinguiprotocol)

;; defines protocols and records required for the swing_listeners

(defprotocol SwingUI
  ;; functions to register some Swing listeners:
  (add-close-button-listener [this f args])
  (add-open-file-button-listener [this f args])
  (add-mousepressed-tree-listener [this f args])
  (add-open-file-menuitem-listener [this f args])
  (add-close-file-menuitem-listener [this f args])
  (export-file-menuitem-listener [this f args])
  (add-export-lkif-filemenuitem-listener [this f args])
  (add-export-graph-menuitem-listener [this f args])
  (add-export-filemenuitem-listener [this f args])
  (add-about-helpmenuitem-listener [this f args])
  (add-printpreview-filemenuitem-listener [this f args])
  (add-close-lkif-filemenuitem-listener [this f args])
  (add-open-graph-menuitem-listener [this f args])
  (add-close-graph-menuitem-listener [this f args])
  (add-print-filemenuitem-listener [this f args])
  (add-search-button-listener [this f args])

  ;; functions to get information from the Swing UI
  (get-selected-object-in-tree [this])
  (get-graphinfo-being-closed [this event])
  (get-searched-info [this]) ;; returns [text options] or nil if search is empty
  )

;; records stored in the element of the tree:
(defrecord LkifFileInfo [path filename] Object
  (toString [this] filename))

(defrecord GraphInfo [lkifinfo id] Object
  (toString [this] id))
