;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.components.tree
  (:use clojure.contrib.def
        clojure.contrib.pprint
        carneades.editor.view.swinguiprotocol
        carneades.editor.view.components.uicomponents
        [carneades.editor.view.components.tabs :only (get-component)]
        (carneades.editor.utils swing swing-tree seq))
  (:require [clojure.zip :as zip])
  (:import (javax.swing.tree DefaultMutableTreeNode
                             TreePath
                             TreeSelectionModel
                             DefaultTreeModel)
           (java.awt.event MouseAdapter)
           javax.swing.JFrame
           javax.swing.JTree
           (carneades.editor.uicomponents EditorApplicationView)
           (carneades.editor.view.swinguiprotocol GraphInfo LkifFileInfo)))

(defvar- *viewinstance* (EditorApplicationView/instance))

(defvar- *lkifFilePopupMenu* (.lkifFilePopupMenu *viewinstance*))
(defvar- *graphPopupMenu* (.graphPopupMenu *viewinstance*))

(defvar *lkifsTree* (.lkifsTree *viewinstance*))

(defn selected-object []
  (when-let [node (.getLastSelectedPathComponent *lkifsTree*)]
    (.getUserObject node)))

(defn has-content []
  (let [model (.getModel *lkifsTree*)
        root (.getRoot model)]
    (pos? (.getChildCount root))))

(defn create-mouse-listener []
  (letfn [(showpopup
           [event]
           (let [x (.getX event)
                 y (.getY event)]
             (when-let [path (.getPathForLocation *lkifsTree* x y)]
               (let [selectedpath (.getSelectionPath *lkifsTree*)]
                 (when (= path selectedpath)
                   (when-let [info (selected-object)]
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

(defn make-node [userobject]
  (DefaultMutableTreeNode. userobject))

(defn init-tree []
  ;; (add-treeselection-listener *lkifsTree* on-tree-selection)
  (.addMouseListener *lkifsTree* (create-mouse-listener))
  (.setRootVisible *lkifsTree* false)
  (let [model (.getModel *lkifsTree*)]
    (.setRoot model (make-node "root")))
  (.setSelectionMode (.getSelectionModel *lkifsTree*)
                     TreeSelectionModel/SINGLE_TREE_SELECTION)
  (.setShowsRootHandles *lkifsTree* true))

(defn expand-select-path [path]
  (.expandPath *lkifsTree* path)
  (.setSelectionPath (.getSelectionModel *lkifsTree*) path)
  (.scrollPathToVisible *lkifsTree* path))


;; the TreeModel API is poor so we convert the model's data to a zipper
;; back-and-forth to have a better way of manipulating the tree
;; (see clojure.zip)

(defn add-lkif-content [path filename graphinfos]
  (with-tree *lkifsTree*
   (let [model (.getModel *lkifsTree*)
         lkifinfo (LkifFileInfo. path filename false)
         ztree (jtreemodel-zip model)
         ztree (zip/rightmost (zip/down (zip/append-child ztree (list lkifinfo))))
         ztree (reduce
                (fn [ztree graphinfo]
                  (let [[id title] graphinfo]
                    (zip/append-child ztree (list (GraphInfo. lkifinfo id title false)))))
                ztree graphinfos)
         newroot (seq-jtreenodes (zip/root ztree) make-node)]
     (.setRoot model newroot)
     (let [added (find-node model #(= lkifinfo %))
           path (make-path newroot added)]
       (expand-select-path path)))))

(defn sort-children-by [loc keyfn]
  "Sorts the children of the location by (keyfn item)"
  ;; (prn "sort-children-by")
  ;; (prn "node =")
  ;; (prn (zip/node loc))
  (let [elements (zip/children loc)
        parent (first elements)
        children (rest elements)
        children (sort-by (comp keyfn first) children)
        newnode (cons parent children)]
    ;; (prn "children =")
    ;; (pprint children)
    ;; (prn "replace =")
    ;; (pprint (zip/root (zip/replace loc newnode)))
    ;; (prn)
    (zip/replace loc newnode)))

(defn add-ag [path id title]
  (prn "add-ag")
  (prn "path =")
  (prn path)
  (with-tree *lkifsTree*
    (let [model (.getModel *lkifsTree*)]
      ;; find the parent to insert the new child:
      (let [[loc graphinfo]
            (loop [loc (jtreemodel-zip model)]
              (if (zip/end? loc)
                [loc nil]
                (let [userobject (zip/node loc)]
                  (if (and (instance? LkifFileInfo userobject)
                           (= path (:path userobject)))
                    (let [graphinfo (GraphInfo. userobject id title false)
                          loc (zip/up loc)
                          ;; xyz (prn "node up =")
                          ;; xyz (prn (zip/node loc))
                          loc (zip/append-child loc (list graphinfo))
                          loc (sort-children-by loc :title)]
                      [loc graphinfo])
                    (recur (zip/next loc))))))
            root (seq-jtreenodes (zip/root loc) make-node)]
        ;; (prn "after addition, root =")
        ;; (pprint (zip/root loc))
        ;; (prn)
        (.setRoot model root)
        ;; select the inserted item
        (let [added (find-node model #(= graphinfo %))
              lkifnode (find-node model #(= (:lkifinfo graphinfo) %))
              path (make-path root lkifnode added)]
          (.setSelectionPath (.getSelectionModel *lkifsTree*) path)
          (.scrollPathToVisible *lkifsTree* path))))))

(defn- graphinfo-pred [path id userobject]
  (and (instance? GraphInfo userobject)
       (= path (-> userobject :lkifinfo :path))
       (= id (:id userobject))))

(defn- lkifinfo-pred [path userobject]
  (and (instance? LkifFileInfo userobject)
       (= path (-> userobject :path))))

(defn remove-ag [path id]
  (with-tree *lkifsTree*
    (let [model (.getModel *lkifsTree*)
          loc (jtreemodel-zip model)]
      (when-let [loc (first (filter
                             (fn [loc]
                               (let [userobject (zip/node loc)]
                                 (graphinfo-pred path id userobject)))
                             (zipper-seqloc loc)))]
        (let [loc (zip/remove (zip/up loc))
              root (seq-jtreenodes (zip/root loc) make-node)]
          (.setRoot model root))))))

(defn remove-lkif-content [path]
  (with-tree *lkifsTree*
    (let [model (.getModel *lkifsTree*)
          loc (jtreemodel-zip model)]
      (when-let [loc (first (filter
                             (fn [loc]
                               (let [userobject (zip/node loc)]
                                 (and (instance? LkifFileInfo userobject)
                                      (= path (:path userobject)))))
                             (zipper-seqloc loc)))]
        (let [loc (zip/remove (zip/up loc))
              root (seq-jtreenodes (zip/root loc) make-node)]
          (.setRoot model root))))))

(defn- change-object [f pred]
  (with-tree *lkifsTree*
    (let [model (.getModel *lkifsTree*)]
      (when-let [node (find-node model pred)]
        (let [userobject (.getUserObject node)]
          (.setUserObject node (f userobject))
          (.reload model))))))

(defn- change-ag-object [path id f]
  "change the object identified by path and id and set it's value to 
  (f olduserobjectvalue)"
  (change-object f #(graphinfo-pred path id %)))

(defn- change-lkif-object [path f]
  "change the object identified by path and set it's value to 
  (f olduserobjectvalue)"
  (change-object f #(lkifinfo-pred path %)))

(defn set-lkif-dirty [path isdirty]
  (prn "setting path dirty, path =")
  (prn path)
  (letfn [(update
           [userobject]
           (assoc userobject :dirty isdirty))]
    (change-lkif-object path update)))

(defn set-ag-dirty [path id isdirty]
  (letfn [(update
           [userobject]
           (let [dirty (:dirty userobject)
                 lkifinfo (:lkifinfo userobject)
                 id (:id userobject)
                 title (:title userobject)]
             (if (not= dirty isdirty)
               (GraphInfo. lkifinfo id title isdirty)
               (GraphInfo. lkifinfo id title dirty))))]
    (change-ag-object path id update)))

(defn change-ag-title [path id newtitle]
  (with-tree *lkifsTree*
    (letfn [(update
             [userobject]
             (let [dirty (:dirty userobject)
                   lkifinfo (:lkifinfo userobject)
                   id (:id userobject)
                   dirty (:dirty userobject)]
               (GraphInfo. lkifinfo id newtitle dirty)))]
      (let [model (.getModel *lkifsTree*)
            loc (jtreemodel-zip model)]
        (when-let [loc (first (filter
                               (fn [loc]
                                 (let [userobject (zip/node loc)]
                                   (graphinfo-pred path id userobject)))
                               (zipper-seqloc loc)))]
          (let [loc (zip/edit loc update)
                loc (-> loc zip/up zip/up)
                loc (sort-children-by loc :title)
                root (seq-jtreenodes (zip/root loc) make-node)
                _ (.setRoot model root)
                added (find-node model #(and (graphinfo-pred path id %)
                                             (= (:title %) newtitle)))
                graphinfo (.getUserObject added)
                lkifnode (find-node model #(= (:lkifinfo graphinfo) %))
                path (make-path root lkifnode added)]
            ;; select renamed node:
            (.setSelectionPath (.getSelectionModel *lkifsTree*) path)
            (.scrollPathToVisible *lkifsTree* path)))))))

(defn select-ag [path id]
  (let [model (.getModel *lkifsTree*)
        root (.getRoot model)]
    (when-let [nodelkif (find-node model #(= path (:path %)))]
      (when-let [node (find-node model #(graphinfo-pred path id %))]
        (let [path (make-path root nodelkif node)]
          (.setSelectionPath (.getSelectionModel *lkifsTree*) path)
          (.scrollPathToVisible *lkifsTree* path))))))