;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.menus.mainmenu
  (:use clojure.contrib.def
        clojure.contrib.swing-utils
        carneades.editor.view.components.uicomponents
        carneades.mapcomponent.map)
  (:import carneades.editor.uicomponents.EditorApplicationView))

(defn- set-enable-diagram-buttons-and-menus [state]
  (.setEnabled *refreshButton* state)
  (.setEnabled *zoomInButton* state)
  (.setEnabled *zoomOutButton* state)
  (.setEnabled *zoomResetButton* state)
  (.setEnabled *printFileMenuItem* state)
  (.setEnabled *printPreviewFileMenuItem* state))

(defn enable-diagram-buttons-and-menus []
  (set-enable-diagram-buttons-and-menus true))

(defn disable-diagram-buttons-and-menus []
  (set-enable-diagram-buttons-and-menus false))

(defn init-menu []
  )

(defn- set-enable-file-items [state]
  (.setEnabled *closeFileMenuItem* state)
  (.setEnabled *saveAsFileMenuItem* state)
  (.setEnabled *saveFileMenuItem* state)
  (.setEnabled *exportFileMenuItem* state)
  )

(defn disable-file-items []
  (set-enable-file-items false))

(defn enable-file-items []
  (set-enable-file-items true))

(defn enable-undo-button []
  (.setEnabled *undoButton* true))

(defn disable-undo-button []
  (.setEnabled *undoButton* false))

(defn enable-redo-button []
  (.setEnabled *redoButton* true))

(defn disable-redo-button []
  (.setEnabled *redoButton* false))

(defn enable-save-filemenuitem []
  (.setEnabled *saveFileMenuItem* true))

(defn disable-save-filemenuitem []
  (.setEnabled *saveFileMenuItem* false))

(defn enable-saveas-filemenuitem []
  (.setEnabled *saveAsFileMenuItem* true))

(defn disable-saveas-filemenuitem []
  (.setEnabled *saveAsFileMenuItem* false))

(defn enable-save-button []
  (.setEnabled *saveButton* true))

(defn disable-save-button []
  (.setEnabled *saveButton* false))

(defn enable-undo-editmenuitem []
  (.setEnabled *undoEditMenuItem* true))

(defn disable-undo-editmenuitem []
  (.setEnabled *undoEditMenuItem* false))

(defn enable-redo-editmenuitem []
  (.setEnabled *redoEditMenuItem* true))

(defn disable-redo-editmenuitem []
  (.setEnabled *redoEditMenuItem* false))
