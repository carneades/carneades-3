;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.context
  (:use clojure.contrib.def
        carneades.editor.view.tabs
        carneades.editor.view.menu.mainmenu)
  (:import (javax.swing.event ChangeListener)))

;; represents the context within which the user interacts:
;; active/inactive menus, dirty markers, active/inactive buttons,
;; current selected graph etc.
;; registers a listener to update the context when the tab changes

(defvar- *dirty-ags* (atom #{}))
(defvar- *undoable-ags* (atom #{}))
(defvar- *redoable-ags* (atom #{}))

(defvar- *current-ag* (atom nil))

(defn- update-menu [])

(defn- update-undo-button [canundo]
  (if canundo
    (enable-undo-button)
    (disable-undo-button)))

(defn- update-redo-button [canredo]
  (if canredo
    (enable-redo-button)
    (disable-redo-button)))

(defn- update-undo-redo-buttons [canundo canredo]
  (update-undo-button canundo)
  (update-redo-button canredo))

(defn- update-save-button [isdirty]
  (if isdirty
    (enable-save-button)
    (disable-save-button)))

(defn- update-buttons [isdirty canundo canredo]
  (update-save-button isdirty)
  (update-undo-redo-buttons canundo canredo))

(defn- update-tree [])

(defn- update-tab [path id isdirty]
  (set-tab-dirty path id isdirty))

(defn current-ag-context []
  "returns [path id]")

(defn set-ag-dirty [path id isdirty]
  (prn "context.set-ag-dirty")
  (prn "dirty?")
  (prn isdirty)
  (if isdirty
    (swap! *dirty-ags* conj [path id])
    (swap! *dirty-ags* disj [path id]))
  ;; TODO: update buttons only when the current graph is the one
  ;; passed as argument
  (update-save-button isdirty)
  (update-tab path id isdirty))

(defn set-ag-canundo [path id canundo]
  (prn "set-ag-canundo")
  (prn canundo)
  (prn "path")
  (prn path)
  (prn "id")
  (prn id)
  (if canundo
   (swap! *undoable-ags* conj [path id])
   (swap! *undoable-ags* disj [path id]))
  ;; TODO: only if on the current graph
  (update-undo-button canundo))

(defn set-ag-canredo [path id canredo]
  (if canredo
   (swap! *redoable-ags* conj [path id])
   (swap! *redoable-ags* disj [path id]))
  (update-redo-button canredo))

(defn can-undo? [path id]
  (contains? (deref *undoable-ags*) [path id]))

(defn can-redo? [path id]
  (contains? (deref *redoable-ags*) [path id]))

(defn is-dirty? [path id]
  (contains? (deref *dirty-ags*) [path id]))

(defn set-current-context-empty []
  (disable-save-button)
  (disable-undo-button)
  (disable-redo-button)
  (disable-diagram-buttons-and-menus))

(defn set-current-ag-context [path id]
  {:pre [(not (nil? path))]}
  (let [isdirty (is-dirty? path id)
        canundo (can-undo? path id)
        canredo (can-redo? path id)]
    (update-buttons isdirty canundo canredo)
    (enable-diagram-buttons-and-menus)))
    ;; TODO update-menus))

(defvar- *tab-change-listener*
  (proxy [ChangeListener] []
    (stateChanged
     [evt]
     (let [idx (.getSelectedIndex *mapPanel*)]
       (if (= idx -1)
         (set-current-context-empty)
         (let [component (.getComponentAt *mapPanel* idx)]
           (when-let [[path id] (get-graphinfo component)]
             (set-current-ag-context path id))))))))

(defn init-context []
  (disable-diagram-buttons-and-menus)
  (disable-undo-button)
  (disable-redo-button)
  (disable-file-items)
  (disable-save-button)
  (register-tab-change-listener *tab-change-listener*))

