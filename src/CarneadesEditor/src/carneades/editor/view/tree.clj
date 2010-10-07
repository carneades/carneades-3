;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.tree
  (:use clojure.contrib.def
        carneades.editor.view.swinguiprotocol
        [carneades.editor.view.tabs :only (get-component)]
        carneades.editor.view.menu.mainmenu
        carneades.editor.utils.swing)
  (:import (javax.swing.tree DefaultMutableTreeNode
                             TreePath
                             DefaultMutableTreeNode
                             TreeSelectionModel)
           (java.awt.event MouseAdapter)
           (carneades.editor.uicomponents EditorApplicationView)
           (carneades.editor.view.swinguiprotocol GraphInfo LkifFileInfo)))

(defvar- *viewinstance* (EditorApplicationView/instance))

(defvar- *lkifFilePopupMenu* (.lkifFilePopupMenu *viewinstance*))
(defvar- *graphPopupMenu* (.graphPopupMenu *viewinstance*))
(defvar- *closeGraphMenuItem* (.closeGraphMenuItem *viewinstance*))

(defvar *lkifsTree* (.lkifsTree *viewinstance*))

(defn selected-object-in-tree []
  (when-let [node (.getLastSelectedPathComponent *lkifsTree*)]
    (.getUserObject node)))

(defn tree-has-content []
  (let [model (.getModel *lkifsTree*)
        root (.getRoot model)]
    (pos? (.getChildCount root))))

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
                   (when-let [info (selected-object-in-tree)]
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

(defn init-tree []
  (add-treeselection-listener *lkifsTree* on-tree-selection)
  (.addMouseListener *lkifsTree* (create-tree-mouse-listener))
  (.setRootVisible *lkifsTree* false)
  (.setSelectionMode (.getSelectionModel *lkifsTree*)
                     TreeSelectionModel/SINGLE_TREE_SELECTION)
  (.setShowsRootHandles *lkifsTree* true))

(defn add-lkif-content [file graphinfos]
  (with-tree *lkifsTree*
    (let [model (.getModel *lkifsTree*)
          root (.getRoot model)
          lkifinfo (LkifFileInfo. (.getPath file)
                                  (.getName file))
          lkif-file (DefaultMutableTreeNode. lkifinfo)]
      (.add root lkif-file)
      (doseq [[id title] graphinfos]
        (.add lkif-file (DefaultMutableTreeNode. (GraphInfo. lkifinfo id title false))))
      (.reload model root)
      (let [path (TreePath. (.getPath lkif-file))]
        (.expandPath *lkifsTree* path)
        (.scrollPathToVisible *lkifsTree* path)
        (.setSelectionPath (.getSelectionModel *lkifsTree*) path)))))

(defn add-ag-in-tree [path id title]
  (with-tree *lkifsTree*
    (let [model (.getModel *lkifsTree*)
          root (.getRoot model)
          childcount (.getChildCount root)]
      (loop [nb (range childcount)]
        (when-let [i (first nb)]
          (let[child (.getChildAt root i)
               lkif-file (.getUserObject child)]
            (if (= path (:path lkif-file))
              (let [agnode (DefaultMutableTreeNode. (GraphInfo. lkif-file id title false))]
                (.add child agnode)
                (.reload model root)
                (let [path (TreePath. (.getPath agnode))]
                  (.expandPath *lkifsTree* path)
                  (.scrollPathToVisible *lkifsTree* path)
                  (.setSelectionPath (.getSelectionModel *lkifsTree*) path))))
            (recur (rest nb))))))))

(defn remove-ag-in-tree [path id]
  (letfn [(remove-ag
           [lkifnode id]
           (loop [childcount (range (.getChildCount lkifnode))]
             (when-let [j (first childcount)]
               (let [graph (.getChildAt lkifnode j)]
                 (let [graphinfo (.getUserObject graph)]
                   (if (= (:id graphinfo) id)
                     (.remove lkifnode j)
                     (recur (rest childcount))))))))]
      (with-tree *lkifsTree*
        (let [model (.getModel *lkifsTree*)
              root (.getRoot model)
              childcount (.getChildCount root)]
          (loop [nb (range childcount)]
            (let[i (first nb)
                 lkifnode (.getChildAt root i)
                 lkifobj (.getUserObject lkifnode)]
              (if (= path (:path lkifobj))
                (remove-ag lkifnode id)
                (recur (rest nb)))))
          (.reload model root)))))

(defn remove-lkif-content [path]
  (with-tree *lkifsTree*
    (let [model (.getModel *lkifsTree*)
          root (.getRoot model)
          childcount (.getChildCount root)]
      (loop [nb (range childcount)]
        (let[i (first nb)
             child (.getChildAt root i)
             lkif-file (.getUserObject child)]
          (if (= path (:path lkif-file))
            (.remove root i)
            (recur (rest nb)))))
      (.reload model root))))

(defn- change-ag-object-in-tree [path id f]
  "change the object identified by path and id and set it's value to 
  (f olduserobjectvalue)"
  (with-tree *lkifsTree*
   (let [model (.getModel *lkifsTree*)
         root (.getRoot model)]
     (doseq [lkif (enumeration-seq (.children root))]
       (doseq [ag (enumeration-seq (.children lkif))]
         (when-let [userobject (.getUserObject ag)]
           (when (and (instance? GraphInfo userobject)
                      (= path (:path (:lkifinfo userobject)))
                      (= id (:id userobject)))
             (.setUserObject ag (f userobject))))))
     (.reload model root))))

(defn set-ag-in-tree-dirty [path id isdirty]
  (letfn [(update
           [userobject]
           (let [dirty (:dirty userobject)
                 lkifinfo (:lkifinfo userobject)
                 id (:id userobject)
                 title (:title userobject)]
             (if (not= dirty isdirty)
               (GraphInfo. lkifinfo id title isdirty)
               (GraphInfo. lkifinfo id title dirty))))]
    (change-ag-object-in-tree path id update)))

(defn change-ag-in-tree-title [path id newtitle]
  (letfn [(update
           [userobject]
           (let [dirty (:dirty userobject)
                 lkifinfo (:lkifinfo userobject)
                 id (:id userobject)
                 dirty (:dirty userobject)]
             (GraphInfo. lkifinfo id newtitle dirty)))]
   (change-ag-object-in-tree path id update)))