;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.mapcomponent.negconnectorshape
  (:use clojure.contrib.def)
  (:import (com.mxgraph.util mxUtils mxPoint mxConstants)
           java.awt.Polygon)
  (:gen-class :extends com.mxgraph.shape.mxConnectorShape
              :main false
              :exposes-methods {paintMarker superPaintMarker}))

(def *carneades-shape-connector* "carneades.shape_connector")
(def *carneades-negarrow* "carneades.negarrow")

(defn -paintMarker [this canvas points style source]
  (let [strokewidth (* (mxUtils/getFloat style mxConstants/STYLE_STROKEWIDTH 1)
                       (.getScale canvas))
        type (mxUtils/getString style (if source mxConstants/STYLE_STARTARROW
                                          mxConstants/STYLE_ENDARROW) "")
        size 6
        color (mxUtils/getColor style mxConstants/STYLE_STROKECOLOR)
        abssize (* size (.getScale canvas))
        markervector (.getMarkerVector this points source abssize)
        p0 (mxPoint. (.getX markervector) (.getY markervector))
        pe (.getEndPoint markervector)
        dx (- (.getX pe) (.getX p0))
        dy (- (.getY pe) (.getY p0))
        dist (max 1 (Math/sqrt (+ (* dx dx) (* dy dy))))
        unitx (/ dx dist)
        unity (/ dy dist)
        nx (* abssize unitx)
        ny (* abssize unity)
        strokex (* unitx strokewidth)
        strokey (* unity strokewidth)
        pe (.clone pe)]
    (.setX pe (- (.getX pe) strokex))
    (.setY pe (- (.getY pe) strokey))
    (.. canvas getGraphics (setColor color))

    (if (= type *carneades-negarrow*)
      (do
       (let [xoffset 4
             yoffset 4
             ;; vector perpendicular to the normal vector
             perx (* (- unity) abssize)
             pery (* unitx abssize)
             offsetfromend (int (/ dist 3)) ;; (* abssize 6)
             xanchor (- (.getX pe) (* offsetfromend unitx))
             yanchor (- (.getY pe) (* offsetfromend unity))
             poly (Polygon. )]
         (doto poly
           (.addPoint (int (- xanchor perx))
                      (int (- yanchor pery)))
           (.addPoint (int (- xanchor perx unitx))
                      (int (- yanchor pery unity)))
           (.addPoint (int (+ xanchor perx (- unitx)))
                      (int (+ yanchor pery (- unity))))
           (.addPoint (int (+ xanchor perx))
                      (int (+ yanchor pery))))
         (.fillShape canvas poly)
         (.. canvas getGraphics (draw poly))
         (mxPoint. (- (/ strokex 2.0)) (- (/ strokey 2.0)))))
      ;; else
      (.superPaintMarker this canvas points style source))))

