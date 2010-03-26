;;; Carneades Argumentation Library and Tools.
;;; Copyright (C) 2010 Thomas F. Gordon, Fraunhofer FOKUS, Berlin
;;; 
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License version 3 (LGPL-3)
;;; as published by the Free Software Foundation.
;;; 
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for details.
;;; 
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(ns carneades.ui.diagram.jgraphviewer
  (:use clojure.contrib.def
        clojure.contrib.pprint
        carneades.ui.diagram.viewerdef
        carneades.engine.argument)
  (:import javax.swing.JFrame
           javax.swing.SwingConstants
           com.mxgraph.layout.mxCompactTreeLayout
           com.mxgraph.layout.mxOrganicLayout
           com.mxgraph.layout.hierarchical.mxHierarchicalLayout
           com.mxgraph.layout.mxStackLayout
           com.mxgraph.swing.mxGraphComponent
           com.mxgraph.util.mxConstants
           com.mxgraph.view.mxGraph
           com.mxgraph.view.mxStylesheet))

(defvar- *title* "Carneades")

(defn applicable-argument-style []
  {mxConstants/STYLE_SHAPE mxConstants/SHAPE_ELLIPSE
   mxConstants/STYLE_PERIMETER mxConstants/PERIMETER_ELLIPSE
   mxConstants/STYLE_STROKECOLOR "#000000"
   mxConstants/STYLE_FILLCOLOR "#cdcdcd"
   mxConstants/STYLE_SPACING_TOP 10
   mxConstants/STYLE_SPACING_BOTTOM 10
   mxConstants/STYLE_SPACING_LEFT 10
   mxConstants/STYLE_SPACING_RIGHT 10})

(defn statement-style []
  {mxConstants/STYLE_SHAPE mxConstants/SHAPE_RECTANGLE
   mxConstants/STYLE_STROKECOLOR "#000000"
   mxConstants/STYLE_FILLCOLOR "#adb7ff"
   mxConstants/STYLE_PERIMETER mxConstants/PERIMETER_RECTANGLE
   mxConstants/STYLE_SPACING_TOP 10
   mxConstants/STYLE_SPACING_BOTTOM 10
   mxConstants/STYLE_SPACING_LEFT 10
   mxConstants/STYLE_SPACING_RIGHT 10})

(defn- disable-interactions [g]
  (doto g
    (.setCellsEditable false)
    (.setEdgeLabelsMovable false)
    (.setVertexLabelsMovable false)
    (.setCellsDisconnectable false)))

(defn create-styles [stylesheet]
  (doto stylesheet
   (.putCellStyle "applicableArgument" (applicable-argument-style))
   (.putCellStyle "statement" (statement-style))))

(defn- insert-vertex [g parent name style]
  (let [v (.insertVertex g parent nil name 10 10 40 40 style)]
    (.updateCellSize g v)
    v))

(defn- insert-edge [g parent begin end style]
  (.insertEdge g parent nil nil begin end))

(defn- hierarchicallayout [g p]
  (let [layout (mxHierarchicalLayout. g SwingConstants/WEST)]
    (doto layout
      (.execute p))))

(defn- treelayout [g p]
  (let [treelayout (mxCompactTreeLayout. g)]
    (doto treelayout
      (.setLevelDistance  50) 
      (.setNodeDistance 50)
      (.setHorizontal true)
      (.execute p))))

(defn- organiclayout [g p]
  (let [organiclayout (mxOrganicLayout. g)]
    (doto organiclayout
      (.execute p))))

(defn- stacklayout [g p]
  (let [stacklayout (mxStackLayout. g true 30 10 10 10)]
    (doto stacklayout
      ;ph, boolean horizontal, int spacing, int x0, int y0, int border) 
      (.execute p))))

(defn- layout [g p]
  (hierarchicallayout g p))

(defn- get-statement-style [s]
  "statement")

(defn- add-statement [g p statement vertices stmt-str] 
   (assoc vertices
     statement
     (insert-vertex g p (stmt-str statement) (get-statement-style statement))))

(defn- add-statements [g p statements stmt-str]
  "add statements and returns a map statement -> vertex"
  (reduce (fn [vertices statement]
            (add-statement g p statement vertices stmt-str))
          {} statements))

(defn- get-edge-style [arg]
  "")

(defn- add-argument-edge [g p arg statement vertices]
  (insert-edge g p (vertices arg) (vertices statement) (get-edge-style arg)))

(defn- add-argument-edges [g p ag arg vertices]
  (add-argument-edge g p (argument-id arg) (argument-conclusion arg) vertices)
  (dorun
   (map #(add-argument-edge g p (premise-atom %) (argument-id arg) vertices)
        (argument-premises arg))))

(defn- add-edges [g p ag vertices]
  (dorun
   (map #(add-argument-edges g p ag % vertices) (arguments ag))))

(defn- get-argument-style [ag argument]
  "applicableArgument")

(defn- add-argument [g p ag arg vertices]
  (assoc vertices
    (argument-id arg)
    (insert-vertex g p (argument-id arg) (get-argument-style ag argument))))

(defn- add-arguments [g p ag vertices]
  (reduce (fn [vertices arg]
            (add-argument g p ag arg vertices))
          vertices (arguments ag)))

(defn- create-graph [ag stmt-str]
  (let [g (mxGraph.)
        p (.getDefaultParent g)]
    (try
     (create-styles (.getStylesheet g))
     (disable-interactions g)
     (.. g getModel beginUpdate)

     (add-edges g p ag
                (add-arguments g p ag
                               (add-statements g p
                                               (map node-statement
                                                    (get-nodes ag))
                                               stmt-str)))
     (layout g p)
     (finally
      (.. g getModel endUpdate)))
    g))

(defn- create-graph-component [ag stmt-str]
  (let [graphcomponent (mxGraphComponent. (create-graph ag stmt-str))]
    (.setConnectable graphcomponent false)
    graphcomponent))

(defn view-jgraph [ag stmt-str]
  (let [frame (JFrame. *title*)]
    (doto frame
      (.. getContentPane (add (create-graph-component ag stmt-str)))
      ;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setSize 800 800)
      (.setVisible true))))

(defmethod view-graph "jgraph" [viewer ag stmt-str]
  (view-jgraph ag stmt-str))

;(view-jgraph nil nil)
