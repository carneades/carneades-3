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

(ns carneades.mapcomponent.map
  (:use clojure.contrib.def
        carneades.engine.argument
        carneades.engine.statement)
  (:import javax.swing.SwingConstants
           (com.mxgraph.util mxConstants mxUtils mxCellRenderer mxPoint mxEvent
                             mxEventSource$mxIEventListener)
           (com.mxgraph.view mxGraph mxStylesheet)
           (com.mxgraph.model mxCell mxGeometry)
           com.mxgraph.layout.hierarchical.mxHierarchicalLayout
           com.mxgraph.layout.mxStackLayout
           com.mxgraph.swing.mxGraphComponent
           com.mxgraph.swing.mxGraphOutline
           java.awt.event.MouseWheelListener
           java.awt.print.PrinterJob))

(defvar- *global-style*
  {mxConstants/STYLE_FONTCOLOR "#000000"
   mxConstants/STYLE_STROKEWIDTH 1.5
   ;; mxConstants/STYLE_FONTSTYLE mxConstants/FONT_BOLD
   })

(defvar- *argument-style*
  (merge *global-style*
         {;; mxConstants/STYLE_VERTICAL_LABEL_POSITION mxConstants/ALIGN_TOP
          ;; mxConstants/STYLE_VERTICAL_ALIGN mxConstants/ALIGN_BOTTOM
          mxConstants/STYLE_STROKEWIDTH 2
          mxConstants/STYLE_FONTSIZE 16
          mxConstants/STYLE_SHAPE mxConstants/SHAPE_ELLIPSE
          mxConstants/STYLE_PERIMETER mxConstants/PERIMETER_ELLIPSE
          mxConstants/STYLE_GRADIENT_DIRECTION mxConstants/DIRECTION_SOUTH
          mxConstants/STYLE_FILLCOLOR "#ffffff"
          ;; mxConstants/STYLE_GRADIENTCOLOR "#ffffff"
          mxConstants/STYLE_STROKECOLOR "#000000"
          ;; mxConstants/STYLE_SHADOW true
          ;; mxConstants/W3C_SHADOWCOLOR "gray"
          ;; mxConstants/STYLE_SPACING_TOP 10
          ;; mxConstants/STYLE_SPACING_BOTTOM 2
          ;; mxConstants/STYLE_SPACING_LEFT 2
          ;; mxConstants/STYLE_SPACING_RIGHT 2
          }))

(defvar- *pro-arg-color* "#0e5200")
(defvar- *con-arg-color* "#e10005")

(defvar- *applicable-argument-style*
  (merge *argument-style*
         {mxConstants/STYLE_FILLCOLOR "#8ee888"}))

(defvar- *pro-applicable-argument-style*
  (merge *applicable-argument-style*
         {mxConstants/STYLE_STROKECOLOR *pro-arg-color*}))

(defvar- *con-applicable-argument-style*
  (merge *applicable-argument-style*
         {mxConstants/STYLE_STROKECOLOR "#ff383d"}))

(defvar- *not-applicable-argument-style* *argument-style*)

(defvar- *pro-not-applicable-argument-style*
  (merge *not-applicable-argument-style*
         {mxConstants/STYLE_STROKECOLOR *pro-arg-color*}))

(defvar- *con-not-applicable-argument-style*
  (merge *not-applicable-argument-style*
         {mxConstants/STYLE_STROKECOLOR *con-arg-color*}))

(defvar- *statement-style* ;; out and not out
  (merge *global-style*
         {mxConstants/STYLE_SHAPE mxConstants/SHAPE_RECTANGLE
          mxConstants/STYLE_STROKECOLOR "#000000"
          ;; mxConstants/STYLE_FILLCOLOR "#c2c9ff"
          mxConstants/STYLE_FILLCOLOR "#ffffff"
          ;; mxConstants/STYLE_GRADIENT_DIRECTION mxConstants/DIRECTION_SOUTH
          ;; mxConstants/STYLE_GRADIENTCOLOR "#ffffff"
          ;; mxConstants/STYLE_SHADOW true
          ;;mxConstants/STYLE_OPACITY 75
          ;; mxConstants/W3C_SHADOWCOLOR "gray"
          mxConstants/STYLE_PERIMETER mxConstants/PERIMETER_RECTANGLE
          mxConstants/STYLE_SPACING_TOP 10
          mxConstants/STYLE_SPACING_BOTTOM 10
          mxConstants/STYLE_SPACING_LEFT 10
          mxConstants/STYLE_SPACING_RIGHT 10}))

(defvar- *in-and-not-out-statement-style*
  (merge *statement-style*
         {mxConstants/STYLE_FILLCOLOR "#8ee888"}))

(defvar- *in-and-not-in-statement-style*
  (merge *statement-style*
         {mxConstants/STYLE_FILLCOLOR "#ffe955"}))

(defvar- *out-and-not-in-statement-style*
  (merge *statement-style*
         {mxConstants/STYLE_FILLCOLOR "#ff383d"}))

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
  {mxConstants/STYLE_ENDARROW mxConstants/NONE
   mxConstants/STYLE_STROKECOLOR "#000000"
   mxConstants/STYLE_STROKEWIDTH 1.25
   mxConstants/STYLE_ROUNDED true})
;; mxConstants/STYLE_EDGE mxConstants/EDGESTYLE_ENTITY_RELATION

(defvar- *conclusion-edge-style* *edge-style*)

(defvar- *pro-conclusion-edge-style*
        (merge *conclusion-edge-style*
               {mxConstants/STYLE_ENDARROW mxConstants/ARROW_CLASSIC}))

(defvar- *con-conclusion-edge-style*
  (merge *conclusion-edge-style*
         ;; {mxConstants/STYLE_ENDARROW mxConstants/ARROW_OVAL}
         {mxConstants/STYLE_ENDARROW mxConstants/ARROW_CLASSIC}))

(defvar- *premise-edge-style* *edge-style*)

(defvar- *assumption-edge-style*
  (merge *premise-edge-style*
         {mxConstants/STYLE_STARTARROW mxConstants/ARROW_DIAMOND}))

(defvar- *exception-edge-style*
  (merge *premise-edge-style*
         {mxConstants/STYLE_DASHED "true"}))

(defvar- *neg-premise-edge-style*
  (merge *premise-edge-style*
         {mxConstants/STYLE_ENDARROW mxConstants/ARROW_OVAL}))

(defvar- *neg-assumption-edge-style*
  (merge *assumption-edge-style*
         {mxConstants/STYLE_ENDARROW mxConstants/ARROW_OVAL}))

(defvar- *neg-exception-edge-style*
  (merge *exception-edge-style*
         {mxConstants/STYLE_ENDARROW mxConstants/ARROW_OVAL}))

(defvar- *styles* {"proApplicableArgument" *pro-applicable-argument-style*
                   "conApplicableArgument" *con-applicable-argument-style*
                   "proNotApplicableArgument" *pro-not-applicable-argument-style*
                   "conNotApplicableArgument" *con-not-applicable-argument-style*
                   "OutOutStatement" *statement-style*
                   "acceptableStatement" *acceptable-statement-style*
                   "complementAcceptableStatement"
                   *complement-acceptable-statement-style*
                   "acceptableAndComplementAcceptableStatement"
                   *acceptable-and-complement-acceptable-statement-style*
                   "InOutStatement" *in-and-not-out-statement-style*
                   "InInStatement" *in-and-not-in-statement-style*
                   "OutInStatement" *out-and-not-in-statement-style*
                   "edge" *edge-style*
                   "proConclusionEdge" *pro-conclusion-edge-style*
                   "conConclusionEdge" *con-conclusion-edge-style*
                   "premiseEdge" *premise-edge-style*
                   "assumptionEdge" *assumption-edge-style*
                   "exceptionEdge" *exception-edge-style*
                   "negPremiseEdge" *neg-premise-edge-style*
                   "negAssumptionEdge" *neg-assumption-edge-style*
                   "negExceptionEdge" *neg-exception-edge-style*})

(defrecord StatementCell [ag stmt stmt-str] Object
  (toString
   [this]
   (let [formatted (stmt-str stmt)]
     (cond (questioned? ag stmt)
                 (str "? " formatted)
                 (accepted? ag stmt)
                 (str "+ " formatted)
                 (rejected? ag stmt)
                 (str "-" formatted)
                 :else formatted))))

(defrecord ArgumentCell [arg] Object
  (toString
   [this]
   (if (= (:direction arg) :pro) "+" "-")))

(defn- configure-graph [#^mxGraph g]
  (doto g
    ;; (.setAllowNegativeCoordinates false)
    ;; seems there is a bug with stacklayout and setCellsLocked
    ;; so setCellsLocked is called after the layout
    ;; (.setCellsLocked true)
    (.setEdgeLabelsMovable false)
    (.setVertexLabelsMovable false)
    (.setCellsDisconnectable false)
    (.setCellsBendable false)
    ))

(defn register-styles [#^mxStylesheet stylesheet]
  (dorun (map (fn [[k v]] (.putCellStyle stylesheet k v)) *styles*)))

(defn- insert-vertex [#^mxGraph g parent name style]
  (let [v (.insertVertex g parent nil name 10 10 40 40 style)]
    (.updateCellSize g v)
    v))

(defn- insert-edge [#^mxGraph g parent begin end style]
  (.insertEdge g parent nil nil begin end style))

(defn- getx [#^mxCell vertex]
  (.. vertex getGeometry getX))

(defn- setx [#^mxCell vertex x]
  (.. vertex getGeometry (setX x)))

(defn- gety [#^mxCell vertex]
  (.. vertex getGeometry getY))

(defn- sety [#^mxCell vertex y]
  (.. vertex getGeometry (setY y)))

(defvar- *ymargin* 10)
(defvar- *xmargin* 10)

(defn- translate-right [#^mxGraph g p vertices]
  (let [model (.getModel g)
        defaultparent (.getDefaultParent g)
        cells (vals vertices)
        minx (reduce (fn [acc vertex]
                       (min (getx vertex) acc))
                     0
                     cells)
        translation (+ *xmargin* (- minx))]
    (doseq [cell cells]
      (setx cell (+ (getx cell) translation))
      (sety cell (+ (gety cell) *ymargin*)))
    (doseq [edge (.getChildCells g defaultparent false true)]
      (let [controlpoints (or (.. edge getGeometry getPoints) ())]
        (doseq [point controlpoints]
          (let [x (.getX point)
                y (.getY point)]
            (.setX point (+ x translation))
            (.setY point (+ y *ymargin*))))))
    ;; (.. g getView (scaleAndTranslate 1 translation *ymargin*))
))

(defn- print-debug [g]
  (let [defaultparent (.getDefaultParent g)
        edges (.getChildCells g defaultparent false true)]
    (doseq [edge edges]
      (let [x (getx edge)
            y (gety edge)
            controlpoints (or (.. edge getGeometry getPoints) ())]
        (printf "edge %s [%s %s] \n" edge x y)
        (printf "control points = {")
        (doseq [point controlpoints]
          (printf "[%s %s], " (.getX point) (.getY point)))
        (printf "}\n")))))

(defn- hierarchicallayout [#^mxGraph g p vertices]
  (let [layout (mxHierarchicalLayout. g SwingConstants/EAST)]
    (.setAllowNegativeCoordinates g false)
    (doto layout
      (.setFineTuning true)
      (.execute p)
      )
    ;; negative coordinates are used by the layout algorithm
    ;; even with setAllowNegativeCoordinates set to false.
    ;; we translate to make all edges and vertices visible
    (translate-right g p vertices)))

(defn- align-orphan-cells [#^mxGraph g p vertices]
  "align orphan cells on the right of the graph, with a stacklayout"
  (letfn [(isorphan?
           [vertex]
           (empty? (.getEdges g vertex)))]
    (let [[orphans maxx-notorphan]
          (reduce (fn [acc vertex]
                    (let [[orphans maxx-notorphan] acc
                          x (getx vertex)]
                      (cond (isorphan? vertex)
                            [(conj orphans vertex) maxx-notorphan]
                            (> x maxx-notorphan) [orphans x]
                            :else acc)))
                  ['() 0]
                  (vals vertices))
          stackspacing 20
          groupparent (.insertVertex g p nil "" 0 0 0 0 "opacity=0")
          group (. g groupCells groupparent 0 (to-array orphans))
          stacklayout (mxStackLayout. g false stackspacing 0 0 0)]
      (.execute stacklayout groupparent)
      ;; make the parent invisible
      (.setGeometry group (mxGeometry. 0 0 0 0)))))

(defn- layout [g p vertices]
  (hierarchicallayout g p vertices)
  (align-orphan-cells g p vertices))

(defn- get-statement-style [ag stmt]
  (let [comp (statement-complement stmt)]
    (cond (and (in? ag stmt) (out? ag comp)) "InOutStatement"
          (and (in? ag stmt) (in? ag comp)) "InInStatement"
          (and (out? ag stmt) (in? ag comp)) "OutInStatement"
          :else "OutOutStatement")))

(defn- add-statement [g p ag stmt vertices stmt-str]
  (assoc vertices
    stmt
    (insert-vertex g p (StatementCell. ag stmt stmt-str)
                   (get-statement-style ag stmt))))

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
  (prn "argument")
  (prn arg)
  (cond (and (applicable? ag arg) (= (:direction arg) :pro))
        (do
          (prn "proApplicableArgument")
          "proApplicableArgument")
        (and (applicable? ag arg) (= (:direction arg) :con))
        (do
          (prn "conApplicableArgument")
         "conApplicableArgument")
        (= (:direction arg) :pro)
        (do
          (prn "proNotApplicableArgument")
         "proNotApplicableArgument")
        :else (do
                prn "conNotApplicableArgument"
               "conNotApplicableArgument")))

(defvar- *argument-width* 32)
(defvar- *argument-height* 32)

(defn- add-argument [g p ag arg vertices]
  (let [vertex
        (insert-vertex g p (ArgumentCell. arg)
                       (get-argument-style ag arg))]
    (.. vertex getGeometry (setWidth *argument-width*))
    (.. vertex getGeometry (setHeight *argument-height*))
    (assoc vertices (argument-id arg) vertex)))

(defn- add-arguments [g p ag vertices]
  (reduce (fn [vertices arg]
            (add-argument g p ag arg vertices))
          vertices (arguments ag)))

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
     (.setCellsLocked g true)
     (finally
      (.. g getModel endUpdate)))
    g))

(defn export-graph [graphcomponent filename]
  "Saves the graph on disk. Only SVG format is supported now.

   Throws java.io.IOException"
  (let [g (.getGraph graphcomponent)]
    (mxUtils/writeFile (mxUtils/getXml
                        (.. (mxCellRenderer/createSvgDocument g nil 1 nil nil)
                            getDocumentElement))
                       filename)))

;; (defmacro with-restore-translate [g & body]
;;   "executes body and restores the initial translation of the graph after"
;;   `(do
;;      (let [point# (.. ~g getView getTranslate)
;;            x# (.getX point#)
;;            y# (.getY point#)]
;;        ~@body
;;        (let [scale# (.. ~g getView getScale)]
;;          (.. ~g getView (scaleAndTranslate scale# x# y#))))))

(defn zoom-in [graphcomponent]
  (let [g (.getGraph graphcomponent)]
    (.zoomIn graphcomponent)))

(defn zoom-out [graphcomponent]
  (let [g (.getGraph graphcomponent)]
    (when (> (.. g getView getScale) 0.1)
      (.zoomOut graphcomponent))))

(defn zoom-reset [graphcomponent]
  (let [g (.getGraph graphcomponent)]
    (.. g getView (scaleAndTranslate 1 0 0))))

(deftype MouseListener [g graphcomponent] MouseWheelListener
  (mouseWheelMoved
   [this event]
   (when (or (instance? mxGraphOutline (.getSource event))
             (.isControlDown event))
     (if (neg? (.getWheelRotation event))
       (zoom-in graphcomponent)
       (zoom-out graphcomponent)))))

(defn- add-mouse-zoom [g graphcomponent]
  (.addMouseWheelListener graphcomponent (MouseListener. g graphcomponent)))

(defn create-graph-component [ag stmt-str]
  (let [g (create-graph ag stmt-str)
        graphcomponent (proxy [mxGraphComponent] [g]
                         ;; no icon for groups
                         ;; allow invisible groups
                         (getFoldingIcon
                          [state]
                          nil))]
    (.setConnectable graphcomponent false)
    (add-mouse-zoom g graphcomponent)
    graphcomponent))

(defn add-node-selected-listener [graphcomponent listener]
  (let [selectionmodel (.getSelectionModel (.getGraph graphcomponent))]
    (.addListener selectionmodel mxEvent/CHANGE
                  (proxy [mxEventSource$mxIEventListener] []
                    (invoke [sender event]
                            (prn "invoke!"))))))
