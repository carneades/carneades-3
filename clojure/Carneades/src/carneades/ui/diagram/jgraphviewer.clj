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
  (:import (javax.swing JFrame JMenu JFileChooser JMenuBar JMenuItem
                        JWindow SwingConstants UIManager filechooser.FileFilter)
           (java.awt.event KeyEvent ActionListener)
           (com.mxgraph.util mxConstants mxUtils mxCellRenderer)
           (com.mxgraph.view mxGraph mxStylesheet)
           com.mxgraph.layout.hierarchical.mxHierarchicalLayout
           com.mxgraph.swing.mxGraphComponent))

(defvar- *title* "Carneades")

(defvar- *argument-style*
  {mxConstants/STYLE_SHAPE mxConstants/SHAPE_ELLIPSE
   mxConstants/STYLE_PERIMETER mxConstants/PERIMETER_ELLIPSE
   mxConstants/STYLE_GRADIENT_DIRECTION mxConstants/DIRECTION_SOUTH
   mxConstants/STYLE_FILLCOLOR "#c2c9ff"
   mxConstants/STYLE_GRADIENTCOLOR "#ffffff"
   mxConstants/STYLE_STROKECOLOR "#000000"
   mxConstants/STYLE_SHADOW true
   mxConstants/W3C_SHADOWCOLOR "gray"
   mxConstants/STYLE_SPACING_TOP 10
   mxConstants/STYLE_SPACING_BOTTOM 10
   mxConstants/STYLE_SPACING_LEFT 10
   mxConstants/STYLE_SPACING_RIGHT 10})

(defvar- *applicable-argument-style*
  (merge *argument-style*
         {mxConstants/STYLE_FILLCOLOR "#9cff89"}))

(defvar- *not-applicable-argument-style* *argument-style*)

(defvar- *statement-style*
  {mxConstants/STYLE_SHAPE mxConstants/SHAPE_RECTANGLE
   mxConstants/STYLE_STROKECOLOR "#000000"
   mxConstants/STYLE_FILLCOLOR "#c2c9ff"
   mxConstants/STYLE_GRADIENT_DIRECTION mxConstants/DIRECTION_SOUTH
   mxConstants/STYLE_GRADIENTCOLOR "#ffffff"
   mxConstants/STYLE_SHADOW true
   ;;mxConstants/STYLE_OPACITY 75
   mxConstants/W3C_SHADOWCOLOR "gray"
   mxConstants/STYLE_PERIMETER mxConstants/PERIMETER_RECTANGLE
   mxConstants/STYLE_SPACING_TOP 10
   mxConstants/STYLE_SPACING_BOTTOM 10
   mxConstants/STYLE_SPACING_LEFT 10
   mxConstants/STYLE_SPACING_RIGHT 10})

(defvar- *acceptable-statement-style*
  (merge *statement-style*
         {mxConstants/STYLE_FILLCOLOR "#a8ff97"}))

(defvar- *complement-acceptable-statement-style*
  (merge *statement-style*
         {mxConstants/STYLE_DASHED true}))

(defvar- *acceptable-and-complement-acceptable-statement-style*
  (merge *complement-acceptable-statement-style*
         *acceptable-statement-style*))

(defvar- *edge-style*
  {mxConstants/STYLE_ENDARROW mxConstants/ARROW_CLASSIC
   mxConstants/STYLE_STROKEWIDTH 1.25
   mxConstants/STYLE_ROUNDED true})
;; mxConstants/STYLE_EDGE mxConstants/EDGESTYLE_ENTITY_RELATION

(defvar- *conclusion-edge-style* *edge-style*)

(defvar- *pro-conclusion-edge-style*
        (merge *conclusion-edge-style*
               {mxConstants/STYLE_ENDARROW mxConstants/ARROW_CLASSIC
                mxConstants/STYLE_STROKECOLOR "#37b71d"}))

(defvar- *con-conclusion-edge-style*
  (merge *conclusion-edge-style*
         {mxConstants/STYLE_ENDARROW mxConstants/ARROW_OPEN
          mxConstants/STYLE_STROKECOLOR "#e50e0e"}))

(defvar- *premise-edge-style* *edge-style*)

(defvar- *assumption-edge-style*
  (merge *premise-edge-style*
         {mxConstants/STYLE_DASHED "true"}))

(defvar- *exception-edge-style*
  (merge *premise-edge-style*
         {mxConstants/STYLE_DASHED "true"
          mxConstants/STYLE_STROKECOLOR "#994fa4"}))

(defvar- *neg-premise-edge-style*
  (merge *edge-style*
         {mxConstants/STYLE_ENDARROW mxConstants/ARROW_DIAMOND}))

(defvar- *neg-assumption-edge-style*
  (merge *assumption-edge-style*
         {mxConstants/STYLE_ENDARROW mxConstants/ARROW_DIAMOND}))

(defvar- *neg-exception-edge-style*
  (merge *exception-edge-style*
         {mxConstants/STYLE_ENDARROW mxConstants/ARROW_DIAMOND}))

(defvar- *styles* {"applicableArgument" *applicable-argument-style*
                   "notApplicableArgument" *not-applicable-argument-style*
                   "statement" *statement-style*
                   "acceptableStatement" *acceptable-statement-style*
                   "complementAcceptableStatement"
                   *complement-acceptable-statement-style*
                   "acceptableAndComplementAcceptableStatement"
                   *acceptable-and-complement-acceptable-statement-style*
                   "edge" *edge-style*
                   "proConclusionEdge" *pro-conclusion-edge-style*
                   "conConclusionEdge" *con-conclusion-edge-style*
                   "premiseEdge" *premise-edge-style*
                   "assumptionEdge" *assumption-edge-style*
                   "exceptionEdge" *exception-edge-style*
                   "negPremiseEdge" *neg-premise-edge-style*
                   "negAssumptionEdge" *neg-assumption-edge-style*
                   "negExceptionEdge" *neg-exception-edge-style*})

(defn- configure-graph [g]
  (doto g
    ;; (.setAllowNegativeCoordinates false)
    (.setCellsEditable false)
    (.setEdgeLabelsMovable false)
    (.setVertexLabelsMovable false)
    (.setCellsDisconnectable false)))

(defn register-styles [stylesheet]
  (dorun (map (fn [[k v]] (.putCellStyle stylesheet k v)) *styles*)))

(defn- insert-vertex [g parent name style]
  (let [v (.insertVertex g parent nil name 10 10 40 40 style)]
    (.updateCellSize g v)
    v))

(defn- insert-edge [g parent begin end style]
  (.insertEdge g parent nil nil begin end style))

(defn- translate-right [g p vertices]
  (letfn [(getx [vertice]
                (.. vertice getGeometry getX))
          (setx [vertice x]
                (. (.. vertice getGeometry) setX x))]
    (let [minx (reduce (fn [acc vertice]
                         (min (getx vertice) acc))
                       0
                       (vals vertices))
          margin 10
          translation (+ margin (- minx))]
      (.. g getView (scaleAndTranslate 1 translation 0)))))

(defn- hierarchicallayout [g p vertices]
  (let [layout (mxHierarchicalLayout. g SwingConstants/EAST)]
    (.setAllowNegativeCoordinates g false)
    (doto layout
      (.setFineTuning true)
      (.execute p))
    ;; negative coordinates are used by the layout algorithm
    ;; even with setAllowNegativeCoordinates set to false.
    ;; we translate to make all edges and vertices visible
    (translate-right g p vertices)))

(defn- layout [g p vertices]
  (hierarchicallayout g p vertices))

(defn- get-statement-style [ag stmt]
  (cond (acceptable? ag stmt) "acceptableStatement"
        :else "statement"))

(defn- add-statement [g p ag stmt vertices stmt-str]
  (letfn [(prefix [s]
                      (cond (questioned? ag stmt)
                            (str "? " s)
                            (accepted? ag stmt)
                            (str "+ " s)
                            (rejected? ag stmt)
                            (str "-" s)
                            :else s))]
    (assoc vertices
      stmt
      (insert-vertex g p (prefix (stmt-str stmt))
                     (get-statement-style ag stmt)))))

(defn- add-statements [g p ag stmt-str]
  "add statements and returns a map statement -> vertex"
  (reduce (fn [vertices statement]
            (add-statement g p ag statement vertices stmt-str))
          {} (map node-statement (get-nodes ag))))

(defn- get-edge-style [p]
  (if (premise-neg? p)
    (cond (assumption? p) "negAssumptionEdge"
          (exception? p) "negExceptionEdge"
          :else "negPremiseEdge")
    (cond (assumption? p) "assumptionEdge"
          (exception? p) "exceptionEdge"
          :else "premiseEdge")))

(defn- get-conclusion-edge-style [arg]
  (if (= (argument-direction arg) :pro)
    "proConclusionEdge"
    "conConclusionEdge"))

(defn- add-conclusion-edge [g p arg statement vertices]
  (insert-edge g p (vertices (argument-id arg)) (vertices statement)
               (get-conclusion-edge-style arg)))

(defn- add-argument-edge [g p premise argid vertices]
  (insert-edge g p (vertices (premise-atom premise))
               (vertices argid) (get-edge-style premise)))

(defn- add-argument-edges [g p ag arg vertices]
  (add-conclusion-edge g p arg (argument-conclusion arg) vertices)
  (dorun
   (map #(add-argument-edge g p % (argument-id arg) vertices)
        (argument-premises arg))))

(defn- add-edges [g p ag vertices]
  (dorun
   (map #(add-argument-edges g p ag % vertices) (arguments ag)))
  vertices)

(defn- get-argument-style [ag arg]
  (if (applicable? ag arg)
    "applicableArgument"
    "notApplicableArgument"))

(defn- add-argument [g p ag arg vertices]
  (assoc vertices
    (argument-id arg)
    (insert-vertex g p (argument-id arg) (get-argument-style ag arg))))

(defn- add-arguments [g p ag vertices]
  (reduce (fn [vertices arg]
            (add-argument g p ag arg vertices))
          vertices (arguments ag)))

(defn- export-graph [g filename]
  "Saves the graph on disk. Only SVG format is supported now.

   Throws java.io.Exception"
  (mxUtils/writeFile (mxUtils/getXml
                      (.. (mxCellRenderer/createSvgDocument g nil 1 nil nil)
                          getDocumentElement))
                     filename))

(defn- create-graph [ag stmt-str]
  (let [g (mxGraph.)
        p (.getDefaultParent g)]
    (try
     (register-styles (.getStylesheet g))
     (configure-graph g)
     (.. g getModel beginUpdate)
     (->> (add-statements g p ag stmt-str)
          (add-arguments g p ag)
          (add-edges g p ag)
          (layout g p))
     (finally
      (.. g getModel endUpdate)))
    g))

(defn- create-graph-component [ag stmt-str]
  (let [graphcomponent (mxGraphComponent. (create-graph ag stmt-str))]
    (.setConnectable graphcomponent false)
    graphcomponent))

;; code to create the menu
(defn- create-file-filter []
  (letfn [(extension [filename]
                     (last (.split filename "\\.")))]
    (proxy [FileFilter] []
      (getDescription []
                      "SVG Files")
      (accept [f]
              (or (.isDirectory f)
                  (= "svg" (extension (.getName f))))))))

(defn- on-export-as-svg [frame]
  (let [graph (.. frame getContentPane (getComponent 0) getGraph)
        filechooser (JFileChooser.)]
    (doto filechooser
      (.setDialogTitle "Export as SVG")
      (.setFileFilter (create-file-filter))
      (.showSaveDialog frame))
    (if-let [file (.getSelectedFile filechooser)]
      (export-graph graph (.getPath file)))))

(defn- on-exit-item [frame]
  (doto frame
    (.setVisible false)
    (.dispose)))

(defn- create-menubar [frame]
  (let [menuBar (JMenuBar.)
        fileMenu (JMenu. "File")
        exportItem (JMenuItem. "Export as SVG..." KeyEvent/VK_E)
        exitItem (JMenuItem. "Exit" KeyEvent/VK_X)]
    (.setMnemonic fileMenu KeyEvent/VK_F)
    (.addActionListener exitItem
                        (proxy [ActionListener] []
                          (actionPerformed [e]
                                           (on-exit-item frame))))
    (.addActionListener exportItem
                        (proxy [ActionListener] []
                          (actionPerformed [e]
                                           (on-export-as-svg frame))))
    (.add fileMenu exportItem)
    (.insertSeparator fileMenu 1)
    (.add fileMenu exitItem)
    (.add menuBar fileMenu)
    menuBar))

(defn- set-native-look-and-feel []
  (try
   (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
   (catch Exception e)))

(defn view-jgraph [ag stmt-str]
  (set-native-look-and-feel)
  (let [frame (JFrame. *title*)]
    (doto frame
      (.setJMenuBar (create-menubar frame))
      (.. getContentPane (add (create-graph-component ag stmt-str)))
      ;;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setSize 800 800)
      (.setVisible true))))

(defmethod view-graph "jgraph" [viewer ag stmt-str]
  (view-jgraph ag stmt-str))
