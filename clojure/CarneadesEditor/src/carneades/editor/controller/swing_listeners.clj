(ns carneades.editor.controller.swing-listeners
  (:use clojure.contrib.def
        clojure.contrib.swing-utils
        carneades.editor.view.editorapplication
        carneades.editor.view.tree
        carneades.editor.controller.listeners
        carneades.editor.utils.swing)
  (:import (carneades.editor.uicomponents EditorApplicationView)
           (carneades.editor.view.tree GraphInfo LkifFileInfo)))

(defvar *viewinstance* (EditorApplicationView/instance))

(defvar *openFileMenuItem* (.openFileMenuItem *viewinstance*))
(defvar *closeTabMenuItem* (.closeTabMenuItem *viewinstance*))
(defvar *closeFileMenuItem* (.closeFileMenuItem *viewinstance*))
(defvar *exportFileMenuItem* (.exportFileMenuItem *viewinstance*))
(defvar *printPreviewFileMenuItem* (.printPreviewFileMenuItem *viewinstance*))
(defvar *aboutHelpMenuItem* (.aboutHelpMenuItem *viewinstance*))

(defvar *closeLkifFileMenuItem* (.closeLkifFileMenuItem *viewinstance*))
(defvar *exportLkifFileMenuItem* (.exportLkifFileMenuItem *viewinstance*))

(defvar *openGraphMenuItem* (.openGraphMenuItem *viewinstance*))
(defvar *closeGraphMenuItem* (.closeGraphMenuItem *viewinstance*))
(defvar *exportGraphMenuItem* (.exportGraphMenuItem *viewinstance*))

(defvar *openFileButton* (.openFileButton *viewinstance*))

(defn- get-tree-select-object [event]
  (when-let [node (.getLastSelectedPathComponent *lkifsTree*)]
    (.getUserObject node)))

(defn mouse-click-in-tree-listener [event view]
  (let [clickcount (.getClickCount event)]
    (when-let [info (get-tree-select-object event)]
      (prn "info")
      (prn info)
      (case clickcount
            1 (condp instance? info 
                GraphInfo (on-select-graphid view (:path (:lkifinfo info))
                                             (:id info))
                LkifFileInfo (on-select-lkif-file view (:path info))
                nil)
            2 (condp instance? info
                GraphInfo (on-edit-graphid view (:path (:lkifinfo info))
                                           (:id info))
                nil)
            nil))))

(defn close-file-listener [event view]
  (prn "close file listener")
  (when-let [info (get-tree-select-object event)]
    (condp instance? info
      LkifFileInfo (on-close-file view (:path info))
      GraphInfo (on-close-file view (:path (:lkifinfo info)))
      nil)))

(defn close-listener [event view]
  (let [[path id] (current-graph view)]
    (on-close-graph view path id)))

(defn open-graph-listener [event view]
  (prn "open-graph-listener")
  (when-let [info (get-tree-select-object event)]
    (condp instance? info
      GraphInfo (on-open-graph view (:path (:lkifinfo info)) (:id info))
      nil)))

(defn close-graph-listener [event view]
  (when-let [info (get-tree-select-object event)]
    (condp instance? info
      GraphInfo (on-close-graph view (:path (:lkifinfo info)) (:id info))
      nil)))

(defn export-file-listener [event view]
  (when-let [info (get-tree-select-object event)]
    (condp instance? info
      GraphInfo (if-let [[path id] (current-graph view)]
                  (on-export-graph view path id)
                  (on-export-graph view (:path (:lkifinfo info)) (:id info)))
      LkifFileInfo (if-let [[path id] (current-graph view)]
                     (on-export-graph view path id)
                     (on-export-file view (:path info)))
      nil)))

(defn export-element-listener [event view]
  (when-let [info (get-tree-select-object event)]
    (condp instance? info
      GraphInfo (on-export-graph view (:path (:lkifinfo info)) (:id info))
      LkifFileInfo (on-export-file view (:path info))
      nil)))

(defn printpreview-listener [event view]
  (if-let [[path id] (current-graph view)]
    (on-printpreview-graph view path id)))
