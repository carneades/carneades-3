;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.maps.lacij-params
  (:use lacij.view.core)
  (:require [analemma.svg :as svg]
            [analemma.xml :as xml]))


(defrecord PlusDecorator
    []
    Decorator

  (decorate
    [this view context]
    (let [width (:width view)
          height (:height view)
          margin 3]
      (-> (svg/path [:M [(double (/ width 2)) 0]
                     :L [(double (/ width 2)) height]
                     :M [0 (double (/ height 2))]
                     :L [width (double (/ height 2))]
                     :Z []])
          (svg/style :stroke-width 1 :stroke "black")))))

(defrecord MinusDecorator
    []
    Decorator

  (decorate
    [this view context]
    (let [width (:width view)
          height (:height view)
          margin 3]
      (-> (svg/line 0 (double (/ height 2)) width (double (/ height 2)))
          (svg/style :stroke-width 1 :stroke "black")))))

(defn make-plusdecorator
  []
  (PlusDecorator.))

(defn make-minusdecorator
  []
  (MinusDecorator.))


(def pro-stroke-color "#0e5200")
(def con-stroke-color "#e10005")

(def in-fill-color "#8ee888") ;; light green
(def out-fill-color "#ff7e7e") ;; tomato
(def undecided-fill-color "white")

(def default-params
     (let [stmt-params {:style {:fill undecided-fill-color} :width 275 :height 46}
           arg-params {:style {:fill undecided-fill-color} :shape :circle :r 10}]
       {:stmt-params stmt-params
        :stmt-in-params (merge stmt-params {:style {:stroke-width 1.5
                                                    :fill in-fill-color}})
        :stmt-out-params (merge stmt-params {:style {:stroke-width 1.5
                                                     :fill out-fill-color}})
        :stmtlabel-params {:style {:stroke-width 1.5
                                   :font-size "14px"}}
        :arg-params arg-params

        :arg-pro-in-params
        (merge arg-params {:style {:stroke pro-stroke-color
                                   :fill in-fill-color
                                   :stroke-width 1.5}})

        :arg-con-in-params (merge arg-params {:style {:stroke con-stroke-color
                                                      :fill in-fill-color
                                                      :stroke-width 1.5}})

        :arg-pro-undecided-params (merge arg-params {:style {:stroke pro-stroke-color
                                                             :fill undecided-fill-color
                                                             :stroke-width 1.5}})

        :arg-con-undecided-params (merge arg-params {:style {:stroke con-stroke-color
                                                             :fill undecided-fill-color
                                                             :stroke-width 1.5}})

        :arg-pro-out-params (merge arg-params {:style {:stroke pro-stroke-color
                                                       :fill out-fill-color
                                                       :stroke-width 1.5}})

        :arg-con-out-params (merge arg-params {:style {:stroke con-stroke-color
                                                       :fill out-fill-color
                                                       :stroke-width 1.5}})

        :arglabel-params {}
        :depth Integer/MAX_VALUE
        :treeify true
        :full-duplication false}))

(def default-markers
     [[:dot-marker-green
                [:marker {:refX -5
                          :refY 0
                          :orient "auto"
                          :style (format "overflow:visible; stroke-dasharray: 0,0 ; stroke:%s; fill:%s"
                                         pro-stroke-color
                                         pro-stroke-color)}
                 (-> (svg/path [:M [-2.5,-1.0]
                                :C [-2.5,1.76 -4.74,4.0 -7.5,4.0]
                                :C [-10.26,4.0 -12.5,1.76 -12.5,-1.0]
                                :C [-12.5,-3.76 -10.26,-6.0 -7.5,-6.0]
                                :C [-4.74,-6.0 -2.5,-3.76 -2.5,-1.0]
                                :Z []])
                     (xml/add-attrs :transform "scale (0.8) translate (7.4, 1)"))]]
      [:dot-marker-red
                [:marker {:refX -5
                          :refY 0
                          :orient "auto"
                          :style (format "overflow:visible; stroke-dasharray: 0,0; stroke:%s; fill:%s"
                                         con-stroke-color
                                         con-stroke-color)}
                 (-> (svg/path [:M [-2.5,-1.0]
                                :C [-2.5,1.76 -4.74,4.0 -7.5,4.0]
                                :C [-10.26,4.0 -12.5,1.76 -12.5,-1.0]
                                :C [-12.5,-3.76 -10.26,-6.0 -7.5,-6.0]
                                :C [-4.74,-6.0 -2.5,-3.76 -2.5,-1.0]
                                :Z []])
                     (xml/add-attrs :transform "scale (0.8) translate (7.4, 1)"))]]
      [:end-arrow-green
                [:marker {:refX 5
                          :refY 0
                          :orient "auto"
                          :style "overflow:visible;"}
                 (-> (svg/path [:M [5.77 0.0]
                                :L [-2.88 5.0]
                                :L [-2.88 -5.0]
                                :L [5.77 0.0]
                                :Z []])
                     (xml/add-attrs :style (format "fill-rule:evenodd; fill:%s; stroke: %s"
                                                   pro-stroke-color pro-stroke-color))
                     (xml/add-attrs :transform "scale (0.8)"))]]
      [:end-arrow-red
                [:marker {:refX 5
                          :refY 0
                          :orient "auto"
                          :style  "overflow:visible;"}
                 (-> (svg/path [:M [5.77 0.0]
                                :L [-2.88 5.0]
                                :L [-2.88 -5.0]
                                :L [5.77 0.0]
                                :Z []])
                     (xml/add-attrs :style (format "fill-rule:evenodd; fill: %s; stroke: %s"
                                                   con-stroke-color con-stroke-color))
                     (xml/add-attrs :transform "scale (0.8)"))]]])

(def neg-premise-params
     [:marker-start "url(#dot-marker-red)"
      :marker-end nil
      :style  {:stroke-width 1
               :stroke con-stroke-color}])

(def premise-params
     [:marker-end nil
      :style {:stroke-width 1
              :stroke pro-stroke-color}])

(def pro-argument-params
     [:style {:stroke pro-stroke-color
              :stroke-width 1
              :marker-end "url(#end-arrow-green)"}])

(def con-argument-params
  [:style {:stroke con-stroke-color
           :stroke-width 1
           :marker-end "url(#end-arrow-red)"}])

(def undercutter-params {:style {:fill "white"} :width 275 :height 46 :rx 15 :ry 15})

(def undercutter-edge-params [:marker-end nil
                              :style {:stroke con-stroke-color
                                      :stroke-width 1}])
