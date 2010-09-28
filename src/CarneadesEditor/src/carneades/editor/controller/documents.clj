;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.controller.documents
  (:use clojure.contrib.def
        carneades.editor.model.docmanager
        carneades.editor.view.viewprotocol))

(defvar *docmanager* (create-docmanager))
(defvar- *dirtyags* (atom #{}))

(defn set-ag-dirty [path id isdirty]
  (if isdirty
    (swap! *dirtyags* conj [path id])
    (swap! *dirtyags* disj [path id])))

(defn update-dirty-state [view path ag isdirty]
  (set-ag-dirty path (:id ag) isdirty)
  (set-dirty view path ag isdirty))

(defn update-undo-redo-statuses [view path id]
  (prn "update-undo-redo-statuses")
  (set-can-undo view path id (can-undo-section? *docmanager* [path :ags id]))
  (set-can-redo view path id (can-redo-section? *docmanager* [path :ags id])))

(defn is-ag-dirty [path id]
  (contains? (deref *dirtyags*) [path id]))

(defn get-ag [lkifpath id]
  (get-section-content *docmanager* [lkifpath :ags id]))

(defn get-ags-id [lkifpath]
  (let [agsid (get-all-sectionskeys *docmanager* [lkifpath :ags])]
    agsid))
