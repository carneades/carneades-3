;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.mapcomponent.map
  (:use clojure.contrib.def
        carneades.mapcomponent.map-styles
        carneades.engine.argument
        carneades.engine.statement)
  (:import javax.swing.SwingConstants
           (com.mxgraph.util mxConstants mxUtils mxCellRenderer mxPoint mxEvent
                             mxEventSource$mxIEventListener)
           com.mxgraph.swing.util.mxGraphTransferable
           (com.mxgraph.view mxGraph mxStylesheet)
           (com.mxgraph.model mxCell mxGeometry)
           com.mxgraph.layout.hierarchical.mxHierarchicalLayout
           com.mxgraph.layout.mxStackLayout
           com.mxgraph.swing.mxGraphComponent
           com.mxgraph.swing.mxGraphOutline
           java.awt.event.MouseWheelListener
           java.awt.print.PrinterJob
           (java.awt Color BasicStroke)))

(defrecord StatementCell [ag stmt stmt-str] Object
  (toString
   [this]
   (let [formatted (stmt-str stmt)]
     (cond (and (in? ag stmt) (in? ag (statement-complement stmt)))
           (str "✔✘ " formatted)
           
           (in? ag stmt)
           (str "✔ " formatted)
           
           (in? ag (statement-complement stmt))
           (str "✘ " formatted)

           (questioned? ag stmt)
           (str "? " formatted)
           
           :else formatted))))

(defrecord ArgumentCell [arg] Object
  (toString
   [this]
   (if (= (:direction arg) :pro) "+" "‒")))

(defrecord PremiseCell [pm] Object
  (toString
   [this]
   (str pm)))

(defn- configure-graph [#^mxGraph g]
  (let [stroke (BasicStroke. 5 BasicStroke/CAP_BUTT,
                             BasicStroke/JOIN_MITER
                             10.0
                             (into-array Float/TYPE [3 3])
                             0.0)
        color Color/orange]
    (set! mxConstants/VERTEX_SELECTION_COLOR color)
    (set! mxConstants/EDGE_SELECTION_COLOR color)
    (set! mxConstants/VERTEX_SELECTION_STROKE stroke)
    (set! mxConstants/EDGE_SELECTION_STROKE stroke))
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

(defvar- *mincellwidth* 70)
(defvar- *mincellheight* 40)

(defn- getx [#^mxCell vertex]
  (.. vertex getGeometry getX))

(defn- setx [#^mxCell vertex x]
  (.. vertex getGeometry (setX x)))

(defn- gety [#^mxCell vertex]
  (.. vertex getGeometry getY))

(defn- sety [#^mxCell vertex y]
  (.. vertex getGeometry (setY y)))

(defn- insert-vertex [#^mxGraph g parent name style]
  (let [v (.insertVertex g parent nil name 10 10 40 40 style)]
    (.updateCellSize g v)
    (let [geo (.getGeometry v)
          w (.getWidth geo)
          h (.getHeight geo)]
      (when (< h *mincellheight*)
        (.setHeight geo *mincellheight*))
      (when (< w *mincellwidth*)
        (.setWidth geo *mincellwidth*)))
    v))

(defn- insert-edge [#^mxGraph g parent userobject begin end style]
  (.insertEdge g parent nil userobject begin end style))

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

(defn- add-conclusion-edge [g p arg statement vertices]
  (insert-edge g p nil (vertices (argument-id arg)) (vertices statement)
               (get-conclusion-edge-style arg)))

(defn- add-argument-edge [g p premise argid vertices]
  (insert-edge g p (PremiseCell. premise) (vertices (premise-atom premise))
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

(defn select-statement [component stmt stmt-fmt]
  (let [graph (.getGraph component)]
    (loop [vertices (seq (.getChildCells graph
                                         (.getDefaultParent graph) true false))]
      (if-let [cell (first vertices)]
        (let [userobject (.getValue cell)]
          (if (and (= (:stmt userobject) stmt)
                   (= (:stmt-str userobject) stmt-fmt))
            (do
             (.setSelectionCell graph cell)
             (.scrollCellToVisible component cell))
            (recur (rest vertices))))))))

(defn add-node-selection-listener [graphcomponent listener & args]
  "Adds a selection listener to the map. When a cell is selected, listener 
   will be invoked with the userobject of the cell as its first argument 
   followed by args.
   Userobject can be StatementCell, ArgumentCell, PremiseCell or nil"
  (let [selectionmodel (.getSelectionModel (.getGraph graphcomponent))]
    (.addListener selectionmodel mxEvent/CHANGE
                  (proxy [mxEventSource$mxIEventListener] []
                    (invoke
                     [sender event]
                     (when-let [cell (.getCell selectionmodel)]
                       (let [userobject (.getValue cell)]
                         (apply listener userobject args))))))))
