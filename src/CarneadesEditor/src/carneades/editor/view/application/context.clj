;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.application.context
  (:use clojure.contrib.def
        (carneades.editor.view.components uicomponents tabs)
        carneades.editor.utils.swing)
  (:require [carneades.editor.view.components.tree :as tree])
  (:import (javax.swing.event ChangeListener)
           (carneades.editor.view.swinguiprotocol GraphInfo
                                               LkifFileInfo)))

;; represents the context within which the user interacts:
;; active/inactive menus, dirty markers, active/inactive buttons,
;; current selected graph etc.
;; registers a listener to update the context when the tab changes

(defvar- *dirty-ags* (atom #{}))
(defvar- *current-lkif* (atom nil))
(defvar- *undoable-ags* (atom #{}))
(defvar- *redoable-ags* (atom #{}))

(defvar- *current-ag* (atom nil))

(defn- update-undo-item [canundo]
  (if canundo
    (enable-items *undoEditMenuItem*)
    (disable-items *undoEditMenuItem*)))

(defn- update-redo-item [canredo]
  (if canredo
    (enable-items *redoEditMenuItem*)
    (disable-items *redoEditMenuItem*)))

(defn- update-edit-menu [canundo canredo]
  (update-undo-item canundo)
  (update-redo-item canredo))

(defn- update-menus [isdirty canundo canredo]
  (update-edit-menu canundo canredo))

(defn- update-undo-button [canundo]
  (if canundo
    (enable-items *undoButton*)
    (disable-items *undoButton*)))

(defn- update-redo-button [canredo]
  (if canredo
    (enable-items *redoButton*)
    (disable-items *redoButton*)))

(defn- update-undo-redo-buttons [canundo canredo]
  (update-undo-button canundo)
  (update-redo-button canredo))

(defn- update-buttons [isdirty canundo canredo]
  (enable-items *saveButton*)
  (update-undo-redo-buttons canundo canredo))

(defn- update-tree [path id isdirty]
  (tree/set-ag-dirty path id isdirty))

(defn- update-tab [path id isdirty]
  (set-tab-dirty path id isdirty))

(defn current-ag-context []
  "returns [path id]"
  (deref *current-ag*))

(defn set-ag-dirty [path id isdirty]
  (if isdirty
    (swap! *dirty-ags* conj [path id])
    (swap! *dirty-ags* disj [path id]))
  (update-tab path id isdirty)
  (update-tree path id isdirty))

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

(defvar- *ag-items* [*zoomInButton*
                     *zoomOutButton*
                     *zoomResetButton*
                     *undoButton*
                     *redoButton*
                     *refreshButton*
                     *exportFileMenuItem*
                     *undoEditMenuItem*
                     *redoEditMenuItem*
                     *copyClipboardEditMenuItem*
                     *printPreviewFileMenuItem*
                     *printFileMenuItem*
                     *findGoalAssistantMenuItem*
                     *findArgumentsAssistantMenuItem*
                     *instantiateSchemeAssistantMenuItem*
                     *formalizeStatementAssistantMenuItem*
                     *selectAllEditMenuItem*])

(defvar- *lkif-items* [*saveButton*
                       *saveFileMenuItem*
                       *saveAsFileMenuItem*
                       *closeFileMenuItem*])

(defn set-current-ag-context-empty []
  (when (not (deref *current-lkif*))
    (apply disable-items *lkif-items*))
  (reset! *current-ag* nil)
  (apply disable-items *ag-items*))

(defn set-current-ag-context [path id]
  {:pre [(not (nil? path))]}
  (reset! *current-ag* [path id])
  (let [isdirty (is-dirty? path id)
        canundo (can-undo? path id)
        canredo (can-redo? path id)]
    (apply enable-items *ag-items*)
    (update-buttons isdirty canundo canredo)
    (update-menus isdirty canundo canredo)))

(defn set-current-lkif-context [lkif]
  (reset! *current-lkif* lkif)
  (apply enable-items *lkif-items*))

(defn set-current-lkif-context-empty []
  (reset! *current-lkif* nil)
  (when-not (deref *current-ag*)
    (apply disable-items *lkif-items*)))

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
         (set-current-ag-context-empty)
         (let [component (.getComponentAt *mapPanel* idx)]
           (when-let [[path id] (get-graphinfo component)]
             (set-current-ag-context path id))))))))

(defn- on-tree-selection [_]
  (if-let [info (tree/selected-object)]
    (condp instance? info
      LkifFileInfo (set-current-lkif-context (:path info))
      GraphInfo (set-current-lkif-context (-> info :lkifinfo :path))
      (set-current-lkif-context-empty))
    (set-current-lkif-context-empty)))

(defn init-context []
  (apply disable-items *saveFileMenuItem* *saveAsFileMenuItem* *saveButton*
         *closeFileMenuItem* *ag-items*)
  (register-tab-change-listener *tab-change-listener*)
  (add-treeselection-listener tree/*lkifsTree* on-tree-selection))

