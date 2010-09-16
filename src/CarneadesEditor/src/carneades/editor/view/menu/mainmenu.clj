;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.menu.mainmenu
  (:use clojure.contrib.def
        clojure.contrib.swing-utils
        carneades.mapcomponent.map)
  (:import carneades.editor.uicomponents.EditorApplicationView))

(defvar- *viewinstance* (EditorApplicationView/instance))

(defvar *zoomInButton* (.zoomInButton *viewinstance*))
(defvar *zoomOutButton* (.zoomOutButton *viewinstance*))
(defvar *zoomResetButton* (.zoomResetButton *viewinstance*))
(defvar *saveButton* (.saveButton *viewinstance*))
(defvar *copyClipboardEditMenuItem* (.copyClipboardEditMenuItem *viewinstance*))
(defvar *selectAllEditMenuItem* (.selectAllEditMenuItem *viewinstance*))

(defvar- *undoButton* (.undoButton *viewinstance*))
(defvar- *redoButton* (.redoButton *viewinstance*))


(defvar- *closeFileMenuItem* (.closeFileMenuItem *viewinstance*))
(defvar- *saveFileMenuItem* (.saveFileMenuItem *viewinstance*))
(defvar- *saveAsFileMenuItem* (.saveAsFileMenuItem *viewinstance*))
(defvar- *exportFileMenuItem* (.exportFileMenuItem *viewinstance*))
(defvar- *printPreviewFileMenuItem* (.printPreviewFileMenuItem *viewinstance*))
(defvar- *printFileMenuItem* (.printFileMenuItem *viewinstance*))


(defn- set-enable-diagram-buttons-and-menus [state]
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
  (.setEnabled *exportFileMenuItem* state))

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

(defn enable-save-button []
  (.setEnabled *saveButton* true))

(defn disable-save-button []
  (.setEnabled *saveButton* false))
