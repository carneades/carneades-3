;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.tree
  (:use clojure.contrib.def
        [carneades.editor.view.tabs :only (get-component)]
        carneades.editor.view.menu.mainmenu
        carneades.editor.utils.swing)
  (:import (javax.swing.tree DefaultMutableTreeNode
                     TreePath)
           (java.awt.event MouseAdapter)
           (carneades.editor.uicomponents EditorApplicationView)))

(defvar- *viewinstance* (EditorApplicationView/instance))

(defvar- *lkifFilePopupMenu* (.lkifFilePopupMenu *viewinstance*))
(defvar- *graphPopupMenu* (.graphPopupMenu *viewinstance*))
(defvar- *closeGraphMenuItem* (.closeGraphMenuItem *viewinstance*))

(defvar *lkifsTree* (.lkifsTree *viewinstance*))

(defrecord LkifFileInfo [path filename] Object
  (toString [this] filename))

(defrecord GraphInfo [lkifinfo id] Object
  (toString [this] id))

(defn add-lkif-content [file graphids]
  (with-tree *lkifsTree*
    (let [model (.getModel *lkifsTree*)
          root (.getRoot model)
          lkifinfo (LkifFileInfo. (.getPath file)
                                  (.getName file))
          lkif-file (DefaultMutableTreeNode. lkifinfo)]
      (.add root lkif-file)
      (doseq [id graphids]
        (.add lkif-file (DefaultMutableTreeNode. (GraphInfo. lkifinfo id))))
      (.reload model root)
      (let [path (TreePath. (.getPath lkif-file))]
        (.expandPath *lkifsTree* path)
        (.scrollPathToVisible *lkifsTree* path)
        (.setSelectionPath (.getSelectionModel *lkifsTree*) path)))))

(defn remove-lkif-content [path]
  (prn "remove-lkif-content")
  (with-tree *lkifsTree*
    (let [model (.getModel *lkifsTree*)
          root (.getRoot model)
          childcount (.getChildCount root)]
      (prn "childcount")
      (prn childcount)
      (loop [nb (range childcount)]
        (let[i (first nb)
             child (.getChildAt root i)
             lkif-file (.getUserObject child)]
          (if (= path (:path lkif-file))
            (.remove root i)
            (recur (rest nb)))))
      (.reload model root))))

(defn tree-has-content []
  (let [model (.getModel *lkifsTree*)
        root (.getRoot model)]
    (pos? (.getChildCount root))))

(defn get-selected-object []
  (when-let [node (.getLastSelectedPathComponent *lkifsTree*)]
    (when-let [info (.getUserObject node)]
      info)))

(defn on-tree-selection [event]
  (let [node (.getLastSelectedPathComponent *lkifsTree*)]
    (if (nil? node)
      (disable-file-items)
      (when (tree-has-content)
        (enable-file-items)))))

(defn create-tree-mouse-listener []
  (letfn [(showpopup
           [event]
           (let [x (.getX event)
                 y (.getY event)]
             (when-let [path (.getPathForLocation *lkifsTree* x y)]
               (let [selectedpath (.getSelectionPath *lkifsTree*)]
                 (when (= path selectedpath)
                   (when-let [info (get-selected-object)]
                     (condp instance? info
                       GraphInfo
                       (do
                         (.setEnabled
                          *closeGraphMenuItem*
                          (not (nil?
                                (get-component
                                 (:path (:lkifinfo info))
                                 (:id info)))))
                         (.show *graphPopupMenu* *lkifsTree* x y))
                       LkifFileInfo (.show *lkifFilePopupMenu* *lkifsTree* x y)
                       nil)))))))]
    (proxy [MouseAdapter] []
     (mousePressed
      [event]
      (when (.isPopupTrigger event)
        (when-let [path (.getPathForLocation *lkifsTree*
                                             (.getX event) (.getY event))]
          (.setSelectionPath (.getSelectionModel *lkifsTree*) path)
          (showpopup event))))
     (mouseReleased
      [event]
      (when (.isPopupTrigger event)
        (showpopup event))))))
