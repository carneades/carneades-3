;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.mapcomponent.map-styles
  (:use clojure.contrib.def
        ;; carneades.mapcomponent.negconnectorshape
        carneades.engine.argument
        carneades.engine.statement)
  (:import com.mxgraph.util.mxConstants
           com.mxgraph.view.mxStylesheet
           com.mxgraph.canvas.mxGraphics2DCanvas
           ;; carneades.mapcomponent.negconnectorshape
           ))

(defn- make-style [name style]
  {:name name :style style})

(defvar- *global-style*
  (make-style
   "globalStyle"
   {mxConstants/STYLE_STROKEWIDTH 1.5
    ;; mxConstants/STYLE_WHITE_SPACE "wrap"
    ;; mxConstants/STYLE_LABEL_BACKGROUNDCOLOR "#ccd151"
    mxConstants/STYLE_SPACING 0
    ;; mxConstants/STYLE_EXIT_PERIMETER false
    ;; mxConstants/STYLE_ENTRY_PERIMETER false
    }))

(defvar- *argument-style*
  (make-style
   "argumentStyle"
   (merge (:style *global-style*)
          {mxConstants/STYLE_VERTICAL_ALIGN mxConstants/ALIGN_BOTTOM
           ;; mxConstants/STYLE_STROKEWIDTH 2
           ;; mxConstants/STYLE_FONTSIZE 16
           ;; mxConstants/STYLE_FONTSTYLE mxConstants/FONT_BOLD
           mxConstants/STYLE_GRADIENT_DIRECTION mxConstants/DIRECTION_SOUTH
           mxConstants/STYLE_FILLCOLOR "#ffffff"
           mxConstants/STYLE_STROKECOLOR "#000000"
           mxConstants/STYLE_NOLABEL false
           mxConstants/STYLE_VERTICAL_LABEL_POSITION mxConstants/ALIGN_BOTTOM
           mxConstants/STYLE_ROUNDED true
           mxConstants/STYLE_SHAPE mxConstants/SHAPE_IMAGE
           })))

(defvar- *pro-arg-color* "#0e5200")
(defvar- *con-arg-color* "#e10005")

(defvar- *pro-argument-style*
  (make-style "proArgumentStyle"
              {mxConstants/STYLE_IMAGE "/pro-arg.png"
               mxConstants/STYLE_STROKECOLOR *pro-arg-color*}))

(defvar- *con-argument-style*
  (make-style "conArgumentStyle"
              {mxConstants/STYLE_IMAGE "/con-arg.png"
               mxConstants/STYLE_STROKECOLOR *con-arg-color*}))

(defvar- *applicable-argument-style*
  (make-style
   "applicableArgumentStyle"
   (merge (:style *argument-style*)
          {mxConstants/STYLE_FILLCOLOR "#8ee888"})))

(defvar- *pro-applicable-argument-style*
  (make-style
   "proApplicableArgumentStyle"
   (merge (:style *applicable-argument-style*)
          (:style *pro-argument-style*)
          {mxConstants/STYLE_IMAGE "/pro-applicable-arg.png"})))

(defvar- *tomato* "#ff7e7e")

(defvar- *con-applicable-argument-style*
  (make-style "conApplicableArgumentStyle"
   (merge (:style *applicable-argument-style*)
          (:style *con-argument-style*)
          {mxConstants/STYLE_IMAGE "/con-applicable-arg.png"
           mxConstants/STYLE_STROKECOLOR "#ff383d"
           mxConstants/STYLE_FILLCOLOR *tomato*})))

(defvar- *not-applicable-argument-style*
  (make-style
   "notApplicableArgumentStyle"
   (:style *argument-style*)))

(defvar- *pro-not-applicable-argument-style*
  (make-style
   "proNotApplicableArgumentStyle"
   (merge (:style *not-applicable-argument-style*)
          (:style *pro-argument-style*)
          {mxConstants/STYLE_IMAGE "/pro-notapplicable-arg.png"
           mxConstants/STYLE_STROKECOLOR *pro-arg-color*})))

(defvar- *con-not-applicable-argument-style*
  (make-style
   "notApplicableArgumentStyle"
   (merge (:style *not-applicable-argument-style*)
          (:style *con-argument-style*)
          {mxConstants/STYLE_IMAGE "/con-notapplicable-arg.png"
           mxConstants/STYLE_STROKECOLOR *con-arg-color*})))

(defvar- *statement-style*
  (make-style
   "statementStyle"
   (merge (:style *global-style*)
          {mxConstants/STYLE_FONTCOLOR "#000000"
           mxConstants/STYLE_SHAPE mxConstants/SHAPE_RECTANGLE
           ;; mxConstants/STYLE_ALIGN mxConstants/ALIGN_LEFT
           ;; mxConstants/STYLE_OVERFLOW "fill"
           mxConstants/STYLE_STROKECOLOR "#000000"
           mxConstants/STYLE_FILLCOLOR "#ffffff"
           mxConstants/STYLE_PERIMETER mxConstants/PERIMETER_RECTANGLE
           mxConstants/STYLE_SPACING_TOP 0
           mxConstants/STYLE_SPACING_BOTTOM 0
           mxConstants/STYLE_SPACING_LEFT 0
           mxConstants/STYLE_SPACING_RIGHT 0
           })))

(defvar- *out-out-statement-style*
  (make-style
   "outOutStatementStyle"
   (:style *statement-style*)))

(defvar- *in-out-statement-style*
  (make-style
   "inOutStatementStyle"
   (merge (:style *statement-style*)
          {mxConstants/STYLE_FILLCOLOR "#8ee888"})))

(defvar- *in-in-statement-style*
  (make-style
   "inInStatementStyle"
   (merge (:style *statement-style*)
          {mxConstants/STYLE_FILLCOLOR "#ffe955"})))

(defvar- *out-in-statement-style*
  (make-style
   "outInStatementStyle"
   (merge (:style *statement-style*)
          ;; red
          {mxConstants/STYLE_FILLCOLOR *tomato*})))

(defvar- *acceptable-statement-style*
  (make-style
   "acceptableArgumentStyle"
   (merge (:style *statement-style*)
          {mxConstants/STYLE_FILLCOLOR "#a8ff97"})))

(defvar- *complement-acceptable-statement-style*
  (make-style
   "complementAcceptableStatementStyle"
   (merge (:style *statement-style*)
          {mxConstants/STYLE_DASHED true})))

(defvar- *acceptable-and-complement-acceptable-statement-style*
  (make-style
   "acceptableAndComplementAcceptableStatementStyle"
   (merge (:style *complement-acceptable-statement-style*)
          *acceptable-statement-style*)))

(defvar- *edge-style*
  (make-style
   "edgeStyle"
   (merge (:style *global-style*)
          {;; mxConstants/STYLE_EDGE mxConstants/EDGESTYLE_ENTITY_RELATION
           mxConstants/STYLE_EDGE mxConstants/EDGESTYLE_LOOP
           ;; mxConstants/STYLE_SEGMENT 1000
           ;; mxConstants/STYLE_ORTHOGONAL false
           mxConstants/STYLE_ENDARROW mxConstants/NONE
           mxConstants/STYLE_STROKECOLOR "#000000"
           mxConstants/STYLE_STROKEWIDTH 1.25
           mxConstants/STYLE_ROUNDED true
           mxConstants/STYLE_NOLABEL true})))

(defvar- *conclusion-edge-style*
  (make-style
   "conclusionEdgeStyle"
   (:style *edge-style*)))

(defvar- *pro-conclusion-edge-style*
  (make-style
   "proConclusionEdgeStyle"
   (merge (:style *conclusion-edge-style*)
          {mxConstants/STYLE_ENDARROW mxConstants/ARROW_CLASSIC
           mxConstants/STYLE_STROKECOLOR *pro-arg-color*})))

(defvar- *con-conclusion-edge-style*
  (make-style "conConclusionEdgeStyle"
   (merge (:style *conclusion-edge-style*)
          {mxConstants/STYLE_ENDARROW mxConstants/ARROW_CLASSIC
           mxConstants/STYLE_STROKECOLOR *con-arg-color*})))

(defvar- *premise-edge-style*
  (make-style
   "premiseEdgeStyle"
   (merge (:style *edge-style*)
          {mxConstants/STYLE_STROKECOLOR *pro-arg-color*})))

(defvar- *assumption-edge-style*
  (make-style
   "assumptionEdgeStyle"
   (merge (:style *premise-edge-style*)
          {mxConstants/STYLE_DASHED "true"
           mxConstants/STYLE_DASH_PATTERN "2.0,2.0"})))

(defvar- *exception-edge-style*
  (make-style
   "exceptionEdgeStyle"
   (merge (:style *premise-edge-style*)
          {mxConstants/STYLE_DASHED "true"
           mxConstants/STYLE_DASH_PATTERN "8.0,2.0"
           mxConstants/STYLE_STROKECOLOR *con-arg-color*})))

(defvar- *neg-premise-edge-style*
  (make-style
   "negPremiseEdgeStyle"
   (merge (:style *premise-edge-style*)
          {;; mxConstants/STYLE_SHAPE *carneades-shape-connector*
           mxConstants/STYLE_STARTARROW mxConstants/ARROW_OVAL
                                        ;;  *carneades-negarrow*
           mxConstants/STYLE_STROKECOLOR *con-arg-color*})))

(defvar- *neg-assumption-edge-style*
  (make-style
   "negAssumptionEdgeStyle"
   (merge (:style *assumption-edge-style*)
          (:style *neg-premise-edge-style*)
          {mxConstants/STYLE_STROKECOLOR *con-arg-color*})))

(defvar- *neg-exception-edge-style*
  (make-style
   "negExceptionEdgeStyle"
   (merge (:style *exception-edge-style*)
          (:style *neg-premise-edge-style*)
          {mxConstants/STYLE_STROKECOLOR *pro-arg-color*})))

(defvar- *styles*
  [*global-style*
   *argument-style*
   *applicable-argument-style*
   *pro-applicable-argument-style*
   *con-applicable-argument-style*
   *not-applicable-argument-style*
   *pro-not-applicable-argument-style*
   *con-not-applicable-argument-style*
   *statement-style*
   *out-out-statement-style*
   *in-out-statement-style*
   *in-in-statement-style*
   *out-in-statement-style*
   *acceptable-statement-style*
   *complement-acceptable-statement-style*
   *acceptable-and-complement-acceptable-statement-style*
   *edge-style*
   *conclusion-edge-style*
   *pro-conclusion-edge-style*
   *con-conclusion-edge-style*
   *premise-edge-style*
   *assumption-edge-style*
   *exception-edge-style*
   *neg-premise-edge-style*
   *neg-assumption-edge-style*
   *neg-exception-edge-style*])

;; (defvar- *shape-registered* (mxGraphics2DCanvas/putShape
;;                              *carneades-shape-connector*
;;                              (negconnectorshape.)))

(defn register-styles [#^mxStylesheet stylesheet]
  (dorun (map (fn [{:keys [name style]}] (.putCellStyle stylesheet name style))
              *styles*)))

(defn get-statement-style [ag stmt]
  (let [comp (statement-complement stmt)]
    (cond (and (in? ag stmt) (out? ag comp)) (:name *in-out-statement-style*)
          (and (in? ag stmt) (in? ag comp)) (:name *in-in-statement-style*)
          (and (out? ag stmt) (in? ag comp)) (:name *out-in-statement-style*)
          :else (:name *out-out-statement-style*))))

(defn get-argument-style [ag arg]
  (cond (and (applicable? ag arg) (= (:direction arg) :pro))
        (:name *pro-applicable-argument-style*)
        (and (applicable? ag arg) (= (:direction arg) :con))
        (:name *con-applicable-argument-style*)
        (= (:direction arg) :pro)
        (:name *pro-not-applicable-argument-style*)
        :else
        (:name *con-not-applicable-argument-style*)))

(defn get-edge-style [p]
  (if (premise-neg? p)
    (cond (assumption? p) (:name *neg-assumption-edge-style*)
          (exception? p) (:name *neg-exception-edge-style*)
          :else (:name *neg-premise-edge-style*))
    (cond (assumption? p) (:name *assumption-edge-style*)
          (exception? p) (:name *exception-edge-style*)
          :else (:name *premise-edge-style*))))

(defn get-conclusion-edge-style [arg]
  (if (= (argument-direction arg) :pro)
    (:name *pro-conclusion-edge-style*)
    (:name *con-conclusion-edge-style*)))
