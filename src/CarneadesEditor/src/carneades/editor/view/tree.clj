;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.view.tree
  (:use clojure.contrib.def
        clojure.contrib.pprint
        carneades.editor.view.swinguiprotocol
        [carneades.editor.view.tabs :only (get-component)]
        carneades.editor.view.menu.mainmenu
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
(defvar- *closeGraphMenuItem* (.closeGraphMenuItem *viewinstance*))

(defvar *lkifsTree* (.lkifsTree *viewinstance*))

(defn selected-object-in-tree []
  (when-let [node (.getLastSelectedPathComponent *lkifsTree*)]
    (.getUserObject node)))

(defn tree-has-content []
  (let [model (.getModel *lkifsTree*)
        root (.getRoot model)]
    (pos? (.getChildCount root))))

;; (defn on-tree-selection [event]
;;   (let [node (.getLastSelectedPathComponent *lkifsTree*)]
;;     (if (nil? node)
;;       (disable-file-items)
;;       (when (tree-has-content)
;;         (enable-file-items)))))

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

(defn make-node [userobject]
  (DefaultMutableTreeNode. userobject))

(defn init-tree []
  ;; (add-treeselection-listener *lkifsTree* on-tree-selection)
  (.addMouseListener *lkifsTree* (create-tree-mouse-listener))
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

(defn add-lkif-content [file graphinfos]
  (with-tree *lkifsTree*
   (let [model (.getModel *lkifsTree*)
         lkifinfo (LkifFileInfo. (.getPath file)
                                 (.getName file))
         ztree (jtreemodel-zip model)
         ;; xyz (prn "before edition, root =")
         ;; xyz (prn (zip/root ztree))
         ztree (zip/rightmost (zip/down (zip/append-child ztree (list lkifinfo))))
         ;; xyz (prn "after adding lkifinfo, =")
         ;; xyz (prn (zip/root ztree))
         ;; xyz (prn "after adding, node =")
         ;; xyz (prn (zip/node ztree))
         ztree (reduce
                (fn [ztree graphinfo]
                  (let [[id title] graphinfo]
                    (zip/append-child ztree (list (GraphInfo. lkifinfo id title false)))))
                ztree graphinfos)
         newroot (seq-jtreenodes (zip/root ztree) make-node)]
     ;; (prn "root =")
     ;; (pprint (zip/root ztree))
     ;; (prn)
     (.setRoot model newroot)
     (let [added (find-node model #(= lkifinfo %))
           path (.pathByAddingChild (TreePath. newroot) added)]
       (expand-select-path path)))))

(defn sort-children-by [loc keyfn]
  "Sorts the children of the zipper by (keyfn item)"
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

(defn add-ag-in-tree [path id title]
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
              path (.. (TreePath. root) (pathByAddingChild lkifnode)
                       (pathByAddingChild added))]
          (.setSelectionPath (.getSelectionModel *lkifsTree*) path)
          (.scrollPathToVisible *lkifsTree* path))))))

(defn- graphinfo-pred [path id userobject]
  (and (instance? GraphInfo userobject)
       (= path (-> userobject :lkifinfo :path))
       (= id (:id userobject))))

(defn remove-ag-in-tree [path id]
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

(defn- change-ag-object-in-tree [path id f]
  "change the object identified by path and id and set it's value to 
  (f olduserobjectvalue)"
  (with-tree *lkifsTree*
    (let [model (.getModel *lkifsTree*)]
     (when-let [node (find-node model #(graphinfo-pred path id %))]
       (let [userobject (.getUserObject node)]
        (.setUserObject node (f userobject))
        (.reload model))))))

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

