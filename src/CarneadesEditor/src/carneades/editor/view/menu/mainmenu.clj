;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.menu.mainmenu
  (:use clojure.contrib.def
        clojure.contrib.swing-utils
        carneades.mapcomponent.map
        carneades.editor.view.tabs)
  (:import carneades.editor.uicomponents.EditorApplicationView))

(defvar- *viewinstance* (EditorApplicationView/instance))

(defvar- *zoomInButton*
  (.zoomInButton *viewinstance*))
(defvar- *zoomOutButton*
  (.zoomOutButton *viewinstance*))
(defvar- *zoomResetButton*
  (.zoomResetButton *viewinstance*))

(defvar- *closeFileMenuItem* (.closeFileMenuItem *viewinstance*))
(defvar- *saveFileMenuItem* (.saveFileMenuItem *viewinstance*))
(defvar- *saveAsFileMenuItem* (.saveAsFileMenuItem *viewinstance*))
(defvar- *exportFileMenuItem* (.exportFileMenuItem *viewinstance*))
(defvar- *printPreviewFileMenuItem* (.printPreviewFileMenuItem *viewinstance*))
(defvar- *printFileMenuItem* (.printFileMenuItem *viewinstance*))

(defvar- *saveFileButton* (.saveFileButton *viewinstance*))

(defn- on-zoom-in [event]
  (zoom-in (.getSelectedComponent *mapPanel*)))

(defn- on-zoom-out [event]
  (zoom-out (.getSelectedComponent *mapPanel*)))

(defn- on-zoom-reset [event]
  (zoom-reset (.getSelectedComponent *mapPanel*)))

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
  (add-action-listener *zoomInButton* on-zoom-in)
  (add-action-listener *zoomOutButton* on-zoom-out)
  (add-action-listener *zoomResetButton* on-zoom-reset))

(defn- set-enable-file-items [state]
  (.setEnabled *closeFileMenuItem* state)
  (.setEnabled *saveAsFileMenuItem* state)
  (.setEnabled *saveFileMenuItem* state)
  (.setEnabled *exportFileMenuItem* state)
  (.setEnabled *saveFileButton* state))

(defn disable-file-items []
  (set-enable-file-items false))

(defn enable-file-items []
  (set-enable-file-items true))

