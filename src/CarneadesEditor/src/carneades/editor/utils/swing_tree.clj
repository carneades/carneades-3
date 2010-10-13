;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.editor.utils.swing-tree
  (:require [clojure.zip :as zip])
  (:import (javax.swing.tree DefaultMutableTreeNode
                             TreePath
                             TreeSelectionModel
                             DefaultTreeModel)))

(defn children-seq [node]
  "returns a sequence of children for the TreeNode node"
  (enumeration-seq (.children node)))

(defn rebuild-treepath [path model]
  "rebuilds a TreePath that has being invalided by a change to the model
   by comparing the userobjects of the old path and of the new model
   Returns nil if the path cannot be rebuilt"
  (let [oldnodes (seq (.getPath path))
        root (.getRoot model)]
    (loop [path (TreePath. root)
           oldnodes (rest oldnodes)
           currentnode root]
      (if-let [oldnode (first oldnodes)]
        (let [userobject (.getUserObject oldnode)]
          (when-let [corresponding (first (filter #(= (.getUserObject %) userobject)
                                           (children-seq currentnode)))]
            (recur (.pathByAddingChild path corresponding) (rest oldnodes) corresponding)))
        path))))

(defmacro with-tree [tree & body]
  "works on a JTree and restores its expanded paths after executing body
   Paths are retrieved using the userobjects, making them robust 
   to nodes changes in the model as long as the objects are the same"
  `(let [tree# ~tree
         root# (.getRoot (.getModel tree#))
         expanded# (if-let [x# (.getExpandedDescendants
                                tree# (TreePath. root#))]
                     (enumeration-seq x#)
                     ())
         selectionmodel# (.getSelectionModel tree#)
         selectionpaths# (. selectionmodel# getSelectionPaths)]
     ~@body
     (doseq [path# expanded#]
       (when-let [path# (rebuild-treepath path# (.getModel tree#))]
        (.expandPath tree# path#)))))

(defn find-node [model pred]
  "finds, in the model of a Jtree, the first node for 
   which (pred (.getUserObject node)) is true
   returns nil on failure"
  (let [nodes (tree-seq (fn [x] true) children-seq (.getRoot model))]
    (first (filter #(pred (.getUserObject %)) nodes))))

(defn jtreemodel-seq
  "takes a JTree and returns a nested seq representing the
   tree of its userobjects"
  ([model]
     (let [root (.getRoot model)]
       (jtreemodel-seq model root)))
  ([model node]
     (let [children (children-seq node)
           userobject (.getUserObject node)]
       (if (empty? children)
         (list (.getUserObject node))
         (cons userobject (map #(jtreemodel-seq model %) children))))))

(defn seq-jtreenodes [col fn]
  "Converts a nested sequence of userobjects 
   into a tree of DefaultMutableTreeNode containing the objects

   (fn userobject) is a constructor that returns an instance of DefaultMutableTreeNode 
   (or descendant) with the given userobject"
  (let [root (first col)
        children (rest col)]
    (let [rootnode (fn root)]
      (.setUserObject rootnode root)
      (doseq [x children]
        (if (seq? x)
          (let [nodechild (seq-jtreenodes x fn)]
            (.add rootnode nodechild))
          (let [node (fn x)]
            (.add rootnode node))))
      rootnode)))

;; (defn seq-jtreemodel [col]
;;   "converts a nested sequence of userobjects 
;;    into a DefaultTreeModel with DefaultMutableTreeNodes containing the objects"
;;   (let [root (seq-jtreenodes col)]
;;     (DefaultTreeModel. root)))

(defn jtreemodel-zip [model]
  "returns a zipper from the userobjects of a JTree model. See clojure.zip"
  (zip/seq-zip (jtreemodel-seq model)))

;; (defn upper-case [loc]
;;   (if (zip/end? loc)
;;     (zip/root loc)
;;     (upper-case
;;      (zip/next
;;       (let [content (zip/node loc)]
;;        (if (string? content)
;;          (zip/replace loc (.toUpperCase content))
;;          loc))))))
