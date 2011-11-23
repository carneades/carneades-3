;;; Copyright © 2010-2011 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.mapcomponent.export
  (:use clojure.pprint
        lacij.graph.svg.graph
        lacij.graph.core
        lacij.layouts.layout
        lacij.view.core
        lacij.opt.annealing)
  (:require [carneades.engine.argument :as ag]
            [carneades.engine.statement :as statement]
            [carneades.mapcomponent.map :as oldmap]
            [analemma.xml :as xml]
            [analemma.svg :as svg]
            [clojure.string :as s]
            [tikkba.utils.dom :as dom])
  (:import (java.io InputStreamReader ByteArrayInputStream
                    ByteArrayOutputStream)))

(defrecord PlusDecorator
    []
    Decorator

  (decorate
    [this view context]
    (let [[centerx centery] (node-center view)
          width (node-width view)
          height (node-height view)
          margin 3]
      (-> (svg/path [:M [centerx (double (+ (- centery (/ height 2)) margin))]
                     :L [centerx (double (- (+ centery (/ height 2)) margin))]
                     :M [(double (+ (- centerx (/ width 2)) margin)) centery]
                     :L [(double (- (+ centerx (/ width 2)) margin)) centery]
                     :Z []])
          (svg/style :stroke-width 1 :stroke "black")))))

(defrecord MinusDecorator
    []
    Decorator

  (decorate
    [this view context]
    (let [[centerx centery] (node-center view)
          width (node-width view)
          height (node-height view)
          margin 3]
      (-> (svg/line (double (+ (- centerx (/ width 2)) margin)) centery
                    (double (- (+ centerx (/ width 2)) margin)) centery)
          (svg/style :stroke-width 1 :stroke "black")))))

(defn trunk-line
  [s]
  (let [s (oldmap/trunk s)
        lines (s/split-lines s)]
    (if (= (count lines) 1)
      (first lines)
      lines)))

(defn gen-arg-id
  [arg]
  (keyword (str (gensym "a"))))

(defn gen-stmt-id
  [stmt]
  (let [stmtstr (statement/statement-formatted stmt)
        size 50
        s (if (> (count stmtstr) size)
            (subs stmtstr 0 size)
            stmtstr)
        s (s/replace s " " "_")]
    (keyword (str "s-" s "-" (str (Math/abs (hash stmt)))))))

(defn pick-stmt-params
  [ag stmt params]
  (let [comp (statement/statement-complement stmt)]
    (cond (and (ag/in? ag stmt) (ag/out? ag comp))
                (:stmt-inout-params params)
          
                (and (ag/in? ag stmt) (ag/in? ag comp))
                (:stmt-inin-params params)
          
                (and (ag/out? ag stmt) (ag/in? ag comp))
                (:stmt-outin-params params)
          
                :else
                (:stmt-outout-params params))))

(defn pick-arg-params
  [ag arg params]
  (cond (and (ag/applicable? ag arg) (= (:direction arg) :pro))
        (:arg-pro-applicable-params params)
        
        (and (ag/applicable? ag arg) (= (:direction arg) :con))
        (:arg-con-applicable-params params)
        
        (= (:direction arg) :pro)
        (:arg-pro-notapplicable-params params)
        
        :else
        (:arg-con-notapplicable-params params)))

(defn add-node-premise
  [map ag stmt id stmtstr params]
  (let [pm-params (merge (pick-stmt-params ag stmt params)
                         {:label "" :x 0 :y 0 :shape :rect})]
    (-> (add-node-kv map id pm-params)
        (add-label-kv id (trunk-line stmtstr)
                      (:stmtlabel-params params)))))

(defn add-linked-node-premise
  [map ag stmt id stmtstr params]
  (add-node-premise map ag stmt id stmtstr params))

(defn add-premise-edge
  [map p id argid params]
  (let [width 1.5]
   (if (ag/premise-neg? p)
           (add-edge map (geneid) id argid
                     :marker-start "url(#dot-marker-red)"
                     :marker-end nil
                     :style {:stroke-width width
                             :stroke (-> params :arg-con-applicable-params :style :stroke)})
           (add-edge map (geneid) id argid :marker-end nil
                     :style {:stroke-width width
                             :stroke (-> params :arg-pro-applicable-params :style :stroke)}))))

(defn add-premise
  [map ag arg argid pm mapinfo pms stmt-str params]
  (let [stmt (ag/premise-atom pm)
        stmtstr (oldmap/stmt-to-str ag stmt stmt-str)
        already-added (get-in mapinfo [:stmts-ids stmt])
        id (gen-stmt-id stmt)
        [map mapinfo pms] (cond
                   (and already-added (:treeify params) (not (:full-duplication params)))
                   [(-> map
                        (add-linked-node-premise ag stmt id stmtstr params)
                        (add-premise-edge pm id argid params))
                    (assoc-in mapinfo [:stmts-ids stmt] id)
                    pms]

                   (or (and already-added (:treeify params) (:full-duplication params))
                       (not already-added))
                   [(-> map
                        (add-node-premise ag stmt id stmtstr params)
                        (add-premise-edge pm id argid params))
                    (assoc-in mapinfo [:stmts-ids stmt] id)
                    (conj pms stmt)]

                   (and already-added (not (:treeify params)))
                   [(add-premise-edge map pm already-added argid params)
                    mapinfo
                    pms]
                   
                   :else (throw (Exception. "invalid choice")))]
    [map mapinfo pms]))

(defn add-premises
  [map ag arg argid mapinfo stmt-str params]
  (let [pms (ag/argument-premises arg)]
    (reduce (fn [[map mapinfo pms] pm]
              (add-premise map ag arg argid pm mapinfo pms stmt-str params))
            [map mapinfo ()]
            pms)))

(defn link-argument
  [map arg argid conclusionid params]
  (let [id (geneid)]
    (if (= (ag/argument-direction arg) :pro)
      (add-edge map id argid conclusionid
                :style {:stroke (-> params :arg-pro-applicable-params :style :stroke)
                        :stroke-width 1.5
                        :marker-end "url(#end-arrow-green)"}) 
      (add-edge map id argid conclusionid
                :style {:stroke (-> params :arg-con-applicable-params :style :stroke)
                        :stroke-width 1.5
                        :marker-end "url(#end-arrow-red)"}))))

(defn add-arg-decorator
  [map arg id]
  (if (= (ag/argument-direction arg) :pro)
    (add-decorator map id (PlusDecorator.))
    (add-decorator map id (MinusDecorator.))))

(defn add-argument-helper
  [map ag arg id conclusion mapinfo params]
  (let [arg-params (merge (pick-arg-params ag arg params)
                          {:label "" :x 0 :y 0})]
    (-> map
        (add-node-kv id arg-params)
        (add-arg-decorator arg id)
        (add-label-kv id "" (:arglabel-params params))
        (link-argument arg id (get-in mapinfo [:stmts-ids conclusion]) params))))

(defn add-linked-argument-helper
  [map ag arg id conclusion mapinfo params]
  (add-argument-helper map ag arg id conclusion mapinfo params))

(defn add-argument
  [map ag conclusion arg mapinfo pms stmt-str params]
  (let [already-added (get (:args-ids mapinfo) arg)
        id (gen-arg-id arg)
        [map mapinfo]
        (cond
         (and already-added (:treeify params) (not (:full-duplication params)))
         [(add-linked-argument-helper map ag arg id conclusion mapinfo params) mapinfo]

         (or (and already-added (:treeify params) (:full-duplication params))
             (not already-added))
         [(add-argument-helper map ag arg id conclusion mapinfo params)
          (assoc-in mapinfo [:args-ids arg] id)]
             
         (and already-added (not (:treeify params)))
         [(link-argument map already-added (get-in mapinfo [:stmts-ids conclusion] params))
          mapinfo]

         :else (throw (Exception. "invalid choice")))
        [map mapinfo newpms] (if (or (not already-added)
                                 (and already-added (:treeify params) (:full-duplication params)))
                           (add-premises map ag arg id mapinfo stmt-str params)
                           [map mapinfo ()])]
    [map mapinfo (concat pms newpms)]))

(defn add-arguments
  [map ag conclusion mapinfo stmt-str params]
  (let [args (ag/arguments ag conclusion)]
    (reduce (fn [[map mapinfo pms] arg]
              (add-argument map ag conclusion arg mapinfo pms stmt-str params))
      [map mapinfo ()]
      args)))

(defn add-statement
  [map ag stmt stmt-ids stmt-str params]
  (let [stmtstr (oldmap/stmt-to-str ag stmt stmt-str)
        id (gen-stmt-id stmt)
        stmt-params (merge (pick-stmt-params ag stmt params)
                           {:label "" :x 0 :y 0 :shape :rect})
        map (add-node-kv map id stmt-params)
        map (add-label-kv map id
                          (trunk-line stmtstr)
                          (:stmtlabel-params params))
        stmt-ids (assoc stmt-ids stmt id)]
    [map stmt-ids]))

(defn add-entities-helper
  [map ag current-stmt mapinfo stmt-str params depth]
  (if (zero? depth) 
    [map mapinfo]
    (let [[map mapinfo pms]
          (add-arguments map ag current-stmt mapinfo stmt-str params)]
      (reduce (fn [[map mapinfo] pm]
                (add-entities-helper map ag pm mapinfo stmt-str params (dec depth)))
              [map mapinfo]
              pms))))

(defn add-entities
  [map ag stmt-str params]
  {:pre [(not (nil? (:main-issue ag)))]}
  (let [main (:main-issue ag)
        [map stmt-ids] (add-statement map ag main {} stmt-str params)]
    (first (add-entities-helper map ag main {:stmts-ids stmt-ids :args-ids {}}
             stmt-str params (:depth params)))))

(defn add-markers
  [map pro-arg-color con-arg-color]
  (-> map
      (add-def [:dot-marker-green
                [:marker {:refX -5
                          :refY 0
                          :orient "auto"
                          :style (format "overflow:visible; stroke-dasharray: 0,0 ; stroke:%s; fill:%s"
                                         pro-arg-color
                                         pro-arg-color)}
                 (-> (svg/path [:M [-2.5,-1.0]
                                :C [-2.5,1.76 -4.74,4.0 -7.5,4.0]
                                :C [-10.26,4.0 -12.5,1.76 -12.5,-1.0]
                                :C [-12.5,-3.76 -10.26,-6.0 -7.5,-6.0]
                                :C [-4.74,-6.0 -2.5,-3.76 -2.5,-1.0]
                                :Z []])
                     (xml/add-attrs :transform "scale (0.8) translate (7.4, 1)"))]])
      (add-def [:dot-marker-red
                [:marker {:refX -5
                          :refY 0
                          :orient "auto"
                          :style (format "overflow:visible; stroke-dasharray: 0,0; stroke:%s; fill:%s"
                                         con-arg-color
                                         con-arg-color)}
                 (-> (svg/path [:M [-2.5,-1.0]
                                :C [-2.5,1.76 -4.74,4.0 -7.5,4.0]
                                :C [-10.26,4.0 -12.5,1.76 -12.5,-1.0]
                                :C [-12.5,-3.76 -10.26,-6.0 -7.5,-6.0]
                                :C [-4.74,-6.0 -2.5,-3.76 -2.5,-1.0]
                                :Z []])
                     (xml/add-attrs :transform "scale (0.8) translate (7.4, 1)"))]])
      (add-def [:end-arrow-green
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
                                                   pro-arg-color pro-arg-color))
                     (xml/add-attrs :transform "scale (0.8)"))]])
      (add-def [:end-arrow-red
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
                                                   con-arg-color con-arg-color))
                     (xml/add-attrs :transform "scale (0.8)"))]])))

(defn export-ag-helper
  [ag stmt-str & options]
  (let [pro-arg-color "#0e5200"
        con-arg-color "#e10005"
        options-kv (apply hash-map options)
        width (get options-kv :width 1280)
        height (get options-kv :height 1024)
        map (create-graph :width width :height height)
        map (add-markers map pro-arg-color con-arg-color)
        ;; stmt-params {:style {:fill "white"} :width 260 :height 70}
        ;; arg-params {:style {:fill "white"} :shape :circle :r 16}
        stmt-params {:style {:fill "white"} :width 255 :height 46}
        arg-params {:style {:fill "white"} :shape :circle :r 10}
        tomato "#ff7e7e"
        lightgreen "#8ee888"
        params (merge {:stmt-params stmt-params

                       :stmt-inin-params
                       (merge stmt-params {:style {:stroke-width 1.5
                                                   :fill "#ffe955"}})

                       :stmt-inout-params
                       (merge stmt-params {:style {:stroke-width 1.5
                                                   :fill lightgreen}})

                       :stmt-outin-params
                       (merge stmt-params {:style {:stroke-width 1.5
                                                   :fill tomato}})

                       :stmt-outout-params
                       stmt-params
                       
                       :stmtlabel-params {:style {:stroke-width 1.5
                                                  :font-size "14px"}}

                       :arg-params arg-params

                       :arg-pro-applicable-params
                       (merge arg-params {:style {:stroke pro-arg-color
                                                  :fill lightgreen
                                                  :stroke-width 1.5}})

                       :arg-con-applicable-params
                       (merge arg-params {:style {:stroke con-arg-color
                                                  :fill tomato
                                                  :stroke-width 1.5}})

                       :arg-pro-notapplicable-params
                       (merge arg-params {:style {:stroke pro-arg-color
                                                  :fill "white"
                                                  :stroke-width 1.5}})

                       :arg-con-notapplicable-params
                       (merge arg-params {:style {:stroke con-arg-color
                                                  :fill "white"
                                                  :stroke-width 1.5}})

                       :arglabel-params {}

                       :depth Integer/MAX_VALUE

                       :treeify false

                       :full-duplication false} options-kv)
        map (add-entities map ag stmt-str params)
        map (time (apply layout map (:layout options-kv) options))
        map (build map)]
    map
    ))

(defn export-ag
  "Exports an argument graph to a SVG file.

   Options are :treeify, :full-duplication, :depth,
   :layout and all options supported by the layout"
  [ag stmt-str filename & options]
  (let [map (apply export-ag-helper ag stmt-str options)]
    (export map filename :indent "yes")))

(defn export-ag-os
  "Exports an argument graph to an InputStream.

   Options are :treeify, :full-duplication, :depth,
   :layout and all options supported by the layout"
  [ag stmt-str & options]
  (let [map (apply export-ag-helper ag stmt-str options)
        os (ByteArrayOutputStream.)
        v (view map)]
    (dom/spit-xml os v :indent "yes")
    (InputStreamReader.
     (ByteArrayInputStream. (.toByteArray os))
     "UTF8")))
