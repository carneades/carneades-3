;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.mapcomponent.map
  (:use clojure.contrib.def
        carneades.mapcomponent.map-styles
        carneades.engine.argument
        carneades.engine.statement)
  (:import (javax.swing SwingConstants SwingUtilities)
           (com.mxgraph.util mxConstants mxUtils mxCellRenderer mxPoint mxEvent
                             mxEventSource$mxIEventListener mxUndoManager)
           com.mxgraph.swing.util.mxGraphTransferable
           com.mxgraph.swing.handler.mxRubberband
           (com.mxgraph.view mxGraph mxStylesheet)
           (com.mxgraph.model mxCell mxGeometry)
           com.mxgraph.layout.hierarchical.mxHierarchicalLayout
           com.mxgraph.layout.mxStackLayout
           com.mxgraph.swing.mxGraphComponent
           com.mxgraph.swing.mxGraphOutline
           java.awt.print.PrinterJob
           java.awt.Toolkit
           java.io.ByteArrayOutputStream
           javax.imageio.ImageIO
           javax.swing.ImageIcon
           (java.awt.event MouseAdapter MouseWheelListener)
           (java.awt.datatransfer Transferable DataFlavor)
           (java.awt Color BasicStroke)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; private functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-transaction [component & body]
  `(let [comp# ~component
         model# (.. comp# getGraph getModel)]
     (try
       (.. model# beginUpdate)
       ~@body
       (finally
        (.. model# endUpdate)))))

(defn stmt-to-str [ag stmt stmt-str]
  (let [formatted (stmt-str stmt)]
    (cond (and (in? ag stmt) (in? ag (statement-complement stmt)))
          (str "✔✘ " formatted)
           
          (in? ag stmt)
          (str "✔ " formatted)
           
          (in? ag (statement-complement stmt))
          (str "✘ " formatted)

          (questioned? ag stmt)
          (str "? " formatted)
           
          :else formatted)))

(defrecord StatementCell [ag stmt stmt-str formatted] Object
  (toString
   [this]
   formatted))

(defrecord ArgumentCell [arg] Object
  (toString
   [this]
   (if (= (:direction arg) :pro) "+" "‒")))

(defrecord PremiseCell [arg pm] Object
  (toString
   [this]
   (str pm)))

(defn- configure-graph [^mxGraph g]
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

(defn getx [^mxCell vertex]
  (.. vertex getGeometry getX))

(defn- setx [^mxCell vertex x]
  (.. vertex getGeometry (setX x)))

(defn gety [^mxCell vertex]
  (.. vertex getGeometry getY))

(defn- sety [^mxCell vertex y]
  (.. vertex getGeometry (setY y)))

(defn adjust-size [v]
  "the the vertex to minimum size"
  (let [geo (.getGeometry v)
        w (.getWidth geo)
        h (.getHeight geo)]
    (when (< h *mincellheight*)
      (.setHeight geo *mincellheight*))
    (when (< w *mincellwidth*)
      (.setWidth geo *mincellwidth*))))

(defn insert-vertex [^mxGraph g parent name style]
  (let [v (.insertVertex g parent (str (gensym)) name 10 10 40 40 style)]
    (.updateCellSize g v)
    (adjust-size v)
    v))

(defn insert-edge [^mxGraph g parent userobject begin end style]
  (.insertEdge g parent (str (gensym)) userobject begin end style))

(defvar- *ymargin* 10)
(defvar- *xmargin* 10)

(defn- translate-right [^mxGraph g p cells]
  (let [model (.getModel g)
        defaultparent (.getDefaultParent g)
        minx (reduce (fn [acc vertex]
                       (min (getx vertex) acc))
                     0
                     cells)
        translation (+ *xmargin* (- minx))]
    (doseq [cell cells]
      (let [geometry (.getGeometry cell)
            width (.getWidth geometry)
            height (.getHeight geometry)
            x (.getX geometry)
            y (.getY geometry)
            newx (+ x translation)
            newy (+ y *ymargin*)]
        (.setGeometry model cell (mxGeometry. newx newy width height))))
    (doseq [edge (.getChildCells g defaultparent false true)]
      (let [controlpoints (or (.. edge getGeometry getPoints) ())]
        (doseq [point controlpoints]
          (let [x (.getX point)
                y (.getY point)]
            (.setX point (+ x translation))
            (.setY point (+ y *ymargin*))))))))

(defn print-debug [g]
  (let [defaultparent (.getDefaultParent g)
        edges (.getChildCells g defaultparent false true)]
    (doseq [edge edges]
      (let [x (getx edge)
            y (gety edge)
            controlpoints (or (.. edge getGeometry getPoints) ())]
        (printf "edge %s [%s %s] = " edge x y)
        (prn (.getValue edge))
        (printf "control points = {")
        (doseq [point controlpoints]
          (printf "[%s %s], " (.getX point) (.getY point)))
        (printf "}\n")))))

(defn move-cell [graph cell x y]
  (let [layout (mxHierarchicalLayout. graph SwingConstants/EAST)]
    (.moveCell layout cell x y)
    ))

(defn- hierarchicallayout [^mxGraph g p vertices roots]
  (let [layout (mxHierarchicalLayout. g SwingConstants/EAST)]
    (.setAllowNegativeCoordinates g false)
    (doto layout
      (.setFineTuning true)
      (.setMoveParent true)
      (.setResizeParent true))
    (if (empty? roots)
      (.execute layout p)
      (.execute layout p (to-array roots)))
    ;; negative coordinates are used by the layout algorithm
    ;; even with setAllowNegativeCoordinates set to false.
    ;; we translate to make all edges and vertices visible
    (translate-right g p vertices)
    ))

(defvar- *orphan-offset* 50)

(defn align-orphan-cells [^mxGraph g p cells]
  "align orphan cells on the right of the graph"
  (letfn [(isorphan?
           [vertex]
           (empty? (.getEdges g vertex)))]
    (let [model (.getModel g)
          [orphans maxx-notorphan]
          (reduce (fn [acc vertex]
                    (let [[orphans maxx-notorphan] acc
                          width (.. vertex getGeometry getWidth)
                          x (+ width (getx vertex))]
                      (cond (isorphan? vertex)
                            [(conj orphans vertex) maxx-notorphan]
                            
                            (> x maxx-notorphan)
                            [orphans x]
                            
                            :else acc)))
                  [() 0]
                  cells)
          yorigin 20
          stackspacing 20]
      (loop [orphans orphans
             y yorigin]
        (when-not (empty? orphans)
          (let [orphan (first orphans)
                width (.. orphan getGeometry getWidth)
                height (.. orphan getGeometry getHeight)
                newx (+ maxx-notorphan *orphan-offset*)]
            (.setGeometry model orphan (mxGeometry. newx y width height))
            (recur (rest orphans) (+ y height stackspacing))))))))

(defn test-layout [g p]
  (let [layout (mxHierarchicalLayout. g SwingConstants/EAST)]
    (.run layout p)
    ))

(defn do-layout
  ([g p cells roots]
     (hierarchicallayout g p cells roots))
  ([g p cells]
     (hierarchicallayout g p cells ())
     (align-orphan-cells g p cells)
     ))

(defn- layout [g p vertices]
  (do-layout g p (vals vertices)))

(defn- add-statement [g p ag stmt vertices stmt-str]
  (assoc vertices
    stmt
    (insert-vertex g p (StatementCell. ag stmt stmt-str (stmt-to-str ag stmt stmt-str))
                   (get-statement-style ag stmt))))

(defn- add-statements [g p ag stmt-str]
  "add statements and returns a map statement -> vertex"
  (reduce (fn [vertices statement]
            (add-statement g p ag statement vertices stmt-str))
          {} (map node-statement (get-nodes ag))))

(defn- add-conclusion-edge [g p arg statement vertices]
  (insert-edge g p (ArgumentCell. arg) (vertices (argument-id arg))
               (vertices statement)
               (get-conclusion-edge-style arg)))

(defn- add-argument-edge [g p arg premise argid vertices]
  (insert-edge g p (PremiseCell. arg premise) (vertices (premise-atom premise))
               (vertices argid) (get-edge-style premise)))

(defn- add-argument-edges [g p ag arg vertices]
  (add-conclusion-edge g p arg (argument-conclusion arg) vertices)
  (dorun
   (map #(add-argument-edge g p arg % (argument-id arg) vertices)
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

(defn- add-undo-manager [g]
  (let [undomanager (mxUndoManager.)
        undo-handler (proxy [mxEventSource$mxIEventListener] []
                       (invoke
                        [sender event]
                        (prn "undo-handler!")
                        (.undoableEditHappened undomanager
                                               (.getProperty event "edit"))))
        undo-sync-handler (proxy [mxEventSource$mxIEventListener] []
                            (invoke
                             [sender event]
                             ;; Keeps the selection in sync with the command history
                             (let [changes (.getChanges (.getProperty event "edit"))]
                               (prn "changes = ")
                               (prn changes)
                              (.setSelectionCells
                               g (.getSelectionCellsForChanges g changes)))))]
    (.. g getModel (addListener mxEvent/UNDO undo-handler))
    (.. g getView (addListener mxEvent/UNDO undo-handler))
    ;; (.addListener undomanager mxEvent/UNDO undo-sync-handler)
    ;; (.addListener undomanager mxEvent/REDO undo-sync-handler)
    undomanager))

(defn- select-current-cell [graphcomponent]
  ;; refresh the selection to refresh the panel properties
  (let [g (.getGraph graphcomponent)
        selectionmodel (.getSelectionModel g)]
    (when-let [cell (.getCell selectionmodel)]
      (.setCell selectionmodel cell))))

(deftype ImageSelection [data]
  Transferable
  (getTransferDataFlavors
   [this]
   (into-array DataFlavor [DataFlavor/imageFlavor]))

  (isDataFlavorSupported
   [this flavor]
   (= DataFlavor/imageFlavor flavor))

  (getTransferData
   [this flavor]
   (when (= DataFlavor/imageFlavor flavor)
     (.getImage (ImageIcon. data)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defn export-graph [graphcomponent filename]
  "Saves the graph on disk. Only SVG format is supported now.

   Throws java.io.IOException"
  (let [g (.getGraph (:component graphcomponent))]
    (mxUtils/writeFile (mxUtils/getXml
                        (.. (mxCellRenderer/createSvgDocument g nil 1 nil nil)
                            getDocumentElement))
                       filename)))


(defn undo [graphcomponent]
  (prn "map-undo!")
  (.undo (:undomanager graphcomponent))
  (select-current-cell (:component graphcomponent)))

(defn redo [graphcomponent]
  (prn "map-redo!")
  (.redo (:undomanager graphcomponent))
  (select-current-cell (:component graphcomponent)))

(defn create-graph-component [ag stmt-str]
  (let [g (create-graph ag stmt-str)
        graphcomponent (proxy [mxGraphComponent] [g]
                         ;; no icon for groups
                         ;; allow invisible groups
                         ;; (getFoldingIcon
                         ;;  [state]
                         ;;  nil)
                         )
        undomanager (add-undo-manager g)
        rubberband (mxRubberband. graphcomponent)]
    (.setConnectable graphcomponent false)
    (add-mouse-zoom g graphcomponent)
    {:component graphcomponent :undomanager undomanager}))

(defn- find-vertex [graph pred]
  (let [vertices (seq (.getChildVertices
                       graph
                       (.getDefaultParent graph)))]
    (first (filter pred vertices))))

(defn find-statement-cell [graph stmt]
  (letfn [(stmt-pred
           [cell]
           (let [userobject (.getValue cell)]
             (and (instance? StatementCell userobject)
                  (= (:stmt userobject) stmt))))]
    (find-vertex graph stmt-pred)))

(defn find-argument-cell [graph argid]
  (letfn [(arg-pred
           [cell]
           (let [userobject (.getValue cell)]
            (and (instance? ArgumentCell userobject)
                 (= (:id (:arg userobject)) argid))))]
    (find-vertex graph arg-pred)))

(defn find-premise-cell [graph argid pm]
  (when-let [argcell (find-argument-cell graph argid)]
    (let [edges (seq (.getEdges graph argcell))]
      (first (filter #(when-let [obj (.getValue %)]
                        (and (instance? PremiseCell obj)
                             (= (:pm obj) pm))) edges)))))

(defn select-statement [component stmt stmt-fmt]
  (let [component (:component component)
        graph (.getGraph component)]
    (when-let [cell (find-statement-cell graph stmt)]
      (.setSelectionCell graph cell)
      (.scrollCellToVisible component cell))))

(defn select-argument [component arg]
  (let [component (:component component)
        graph (.getGraph component)]
    (when-let [cell (find-argument-cell graph (:id arg))]
      (.setSelectionCell graph cell)
      (.scrollCellToVisible component cell))))

(defn add-node-selection-listener [graphcomponent listener & args]
  "Adds a selection listener to the map. When a cell is selected, listener 
   will be invoked with the userobject of the cell as its first argument 
   followed by args.
   Userobject can be StatementCell, ArgumentCell, PremiseCell or nil"
  (let [component (:component graphcomponent)
        selectionmodel (.getSelectionModel (.getGraph component))]
    (.addListener selectionmodel mxEvent/CHANGE
                  (proxy [mxEventSource$mxIEventListener] []
                    (invoke
                     [sender event]
                     (when-let [cell (.getCell selectionmodel)]
                       (let [userobject (.getValue cell)]
                         (apply listener userobject args))))))))

(defn scale-page [swingcomponent scale]
  (if (nil? scale)
    (.setPageScale swingcomponent mxGraphComponent/DEFAULT_PAGESCALE)
    (.setPageScale swingcomponent scale)))

(defn get-vertices [g p]
  (seq (.getChildVertices g p)))

(defn copyselection-toclipboard [graphcomponent]
  (let [component (:component graphcomponent)
        graph (.getGraph component)
        selectionmodel (.getSelectionModel graph)
        selectedcells (.getCells selectionmodel)
        bufferedimg (mxCellRenderer/createBufferedImage
             graph selectedcells 1 nil (.isAntiAlias component) nil (.getCanvas component))
        os (ByteArrayOutputStream.)
        res (ImageIO/write bufferedimg "png" os)
        imgselection (ImageSelection. (.toByteArray os))
        clipboard (.getSystemClipboard (.getToolkit component))]
    (.setContents clipboard imgselection nil)))

(defn select-all [graphcomponent]
  (let [component (:component graphcomponent)
        graph (.getGraph component)
        cells (.getChildCells graph (.getDefaultParent graph) true true)
        selectionmodel (.getSelectionModel graph)]
    (.setCells selectionmodel cells)))

(defn current-selected-object [graphcomponent]
  (let [component (:component graphcomponent)]
    (when-let [cell (.. component getGraph getSelectionCell)]
      (.getValue cell))))

(defn add-right-click-listener [graphcomponent listener]
  (let [component (:component graphcomponent)
        graphcontrol (.getGraphControl component)]
    (.addMouseListener graphcontrol
                       (proxy [MouseAdapter] []
                         (mouseReleased
                          [event]
                          (.mousePressed this event))
    
                         (mousePressed
                          [event]
                          (when (.isPopupTrigger event)
                            (when-let [userobject (current-selected-object graphcomponent)]
                              (listener event userobject))))))))

(defn layout-map [graphcomponent]
  (let [component (:component graphcomponent)
        graph (.getGraph component)
        p (.getDefaultParent graph)
        vertices (get-vertices graph p)]
    (with-transaction component
      (do-layout graph p vertices))))