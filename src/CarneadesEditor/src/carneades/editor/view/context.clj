;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.context
  (:use clojure.contrib.def
        (carneades.editor.view tabs tree)
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

(defn- update-file-menu [isdirty]
  (if isdirty
    (do
      (enable-save-filemenuitem)
      (enable-saveas-filemenuitem))
    (do
      (disable-save-filemenuitem)
      (disable-saveas-filemenuitem))))

(defn- update-undo-item [canundo]
  (if canundo
    (enable-undo-editmenuitem)
    (disable-undo-editmenuitem)))

(defn- update-redo-item [canredo]
  (if canredo
    (enable-redo-editmenuitem)
    (disable-redo-editmenuitem)))

(defn- update-edit-menu [canundo canredo]
  (update-undo-item canundo)
  (update-redo-item canredo))

(defn- update-menus [isdirty canundo canredo]
  (update-file-menu isdirty)
  (update-edit-menu canundo canredo))

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

(defn- update-tree [path id isdirty]
  (set-ag-in-tree-dirty path id isdirty))

(defn- update-tab [path id isdirty]
  (set-tab-dirty path id isdirty))

(defn current-ag-context []
  "returns [path id]"
  (deref *current-ag*))

(defn set-ag-dirty [path id isdirty]
  (if isdirty
    (swap! *dirty-ags* conj [path id])
    (swap! *dirty-ags* disj [path id]))
  (when (= (current-ag-context) [path id])
    (update-save-button isdirty)
    (update-tab path id isdirty)
    (update-tree path id isdirty)
    (update-file-menu isdirty)))

(defn set-ag-canundo [path id canundo]
  (if canundo
   (swap! *undoable-ags* conj [path id])
   (swap! *undoable-ags* disj [path id]))
  (when (= (current-ag-context) [path id])
    (update-undo-button canundo)
    (update-undo-item canundo)))

(defn set-ag-canredo [path id canredo]
  (if canredo
   (swap! *redoable-ags* conj [path id])
   (swap! *redoable-ags* disj [path id]))
  (when (= (current-ag-context) [path id])
    (update-redo-button canredo)
    (update-redo-item canredo)))

(defn can-undo? [path id]
  (contains? (deref *undoable-ags*) [path id]))

(defn can-redo? [path id]
  (contains? (deref *redoable-ags*) [path id]))

(defn is-dirty? [path id]
  (contains? (deref *dirty-ags*) [path id]))

(defn set-current-context-empty []
  (reset! *current-ag* nil)
  (disable-save-button)
  (disable-undo-button)
  (disable-redo-button)
  (disable-diagram-buttons-and-menus))

(defn set-current-ag-context [path id]
  {:pre [(not (nil? path))]}
  (reset! *current-ag* [path id])
  (let [isdirty (is-dirty? path id)
        canundo (can-undo? path id)
        canredo (can-redo? path id)]
    (update-buttons isdirty canundo canredo)
    (enable-diagram-buttons-and-menus)
    (update-menus isdirty canundo canredo)))

(defn remove-ag-context [path id]
  (swap! *dirty-ags* disj [path id])
  (swap! *undoable-ags* disj [path id])
  (swap! *redoable-ags* disj [path id]))

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
  (disable-undo-button)
  (disable-redo-button)
  (register-tab-change-listener *tab-change-listener*))

