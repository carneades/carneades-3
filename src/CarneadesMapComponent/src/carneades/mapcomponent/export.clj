;;; Copyright © 2010-2011 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.mapcomponent.export
  (:use clojure.pprint
        lacij.graph.svg.graph
        lacij.graph.core
        lacij.layouts.layout
        lacij.view.core
        lacij.opt.annealing
        carneades.engine.statement
        carneades.mapcomponent.export-params
        carneades.mapcomponent.format-statement)
  (:require [carneades.engine.argument :as arg]
            [analemma.xml :as xml]
            [analemma.svg :as svg]
            [clojure.string :as s]
            [tikkba.utils.dom :as dom])
  (:import (java.io InputStreamReader ByteArrayInputStream
                    ByteArrayOutputStream)))

(defn gen-arg-id
  [arg]
  (keyword (str "a" (str (Math/abs (hash arg))))))

(defn gen-stmt-id
  ;; Please think twice before making changes to this function!
  ;; some ids with special characters could not be accepted by batik
  ;; the generation of the layout would still works but with
  ;; a strange output
  [stmt]
  ;; {:pre [(do (printf "%s -> " stmt) true)]
  ;;  :post [(do (printf "%s\n" %) true)]}
  (let [stmtstr (statement-formatted stmt)
        size 50
        s (if (> (count stmtstr) size)
            (subs stmtstr 0 size)
            stmtstr)
        s (s/replace s #"\W" "_")]
    (keyword (str "s_" s "_" (str (Math/abs (hash stmt)))))))

(defn pick-stmt-params
  [ag stmt params]
  (let [comp (statement-complement stmt)]
    (cond (and (arg/in? ag stmt) (arg/out? ag comp))
                (:stmt-inout-params params)
          
                (and (arg/in? ag stmt) (arg/in? ag comp))
                (:stmt-inin-params params)
          
                (and (arg/out? ag stmt) (arg/in? ag comp))
                (:stmt-outin-params params)
          
                :else
                (:stmt-outout-params params))))

(defn pick-arg-params
  [ag arg params]
  (cond (and (arg/applicable? ag arg) (= (:direction arg) :pro))
        (:arg-pro-applicable-params params)
        
        (and (arg/applicable? ag arg) (= (:direction arg) :con))
        (:arg-con-applicable-params params)
        
        (= (:direction arg) :pro)
        (:arg-pro-notapplicable-params params)
        
        :else
        (:arg-con-notapplicable-params params)))

(defn add-statement
  [svgmap stmt ag stmt-str params]
  (let [stmtstr (stmt-to-str ag stmt stmt-str)
        id (gen-stmt-id stmt)
        stmt-params (merge (pick-stmt-params ag stmt params)
                           {:label "" :x 0 :y 0 :shape :rect})
        svgmap (add-node-kv svgmap id stmt-params)
        svgmap (add-label-kv svgmap id
                          (trunk-line stmtstr)
                          (:stmtlabel-params params))]
    svgmap))

(defn add-arg-decorator
  [svgmap arg argid]
  (if (= (arg/argument-direction arg) :pro)
    (add-decorator svgmap argid (make-plusdecorator))
    (add-decorator svgmap argid (make-minusdecorator))))

(defn add-argument-node
  [svgmap arg ag params]
  (let [arg-params (pick-arg-params ag arg params)
        argid (gen-arg-id arg)
        svgmap (add-node-kv svgmap argid arg-params)
        svgmap (add-arg-decorator svgmap arg argid)]
    svgmap))

(defn add-conclusion-edge
  [svgmap conclusion arg]
  (let [id (geneid)
        conclusionid (gen-stmt-id conclusion)
        argid (gen-arg-id arg)]
    (if (= (arg/argument-direction arg) :pro)
      (apply add-edge svgmap id argid conclusionid pro-argument-params)
      (apply add-edge svgmap id argid conclusionid con-argument-params))))

(defn add-premise-edge
  [svgmap premise arg]
  (let [edgeid (geneid)
        argid (gen-arg-id arg)
        stmtid (gen-stmt-id (:atom premise))]
   (if (arg/premise-neg? premise)
     (apply add-edge svgmap edgeid stmtid argid neg-premise-params)
     (apply add-edge svgmap edgeid stmtid argid premise-params))))

(defn add-premises-edges
  [svgmap premises arg]
  (reduce (fn [svgmap premise]
            (add-premise-edge svgmap premise arg))
          svgmap premises))

(defn add-argument
  [svgmap arg ag params]
  (let [conclusion (arg/argument-conclusion arg)
        premises (arg/argument-premises arg)
        svgmap (add-argument-node svgmap arg ag params)
        svgmap (add-conclusion-edge svgmap conclusion arg)
        svgmap (add-premises-edges svgmap premises arg)]
    svgmap))

(defn add-entities
  [svgmap ag stmt-str params]
  (let [statements (map arg/node-statement (arg/get-nodes ag))
        arguments (arg/arguments ag)]
    (let [svgmap (reduce (fn [svgmap stmt]
                           (add-statement svgmap stmt ag stmt-str params))
                         svgmap statements)
          svgmap (reduce (fn [svgmap arg]
                           (add-argument svgmap arg ag params))
                         svgmap arguments)]
      svgmap)))

(defn add-markers
  [map]
  (reduce (fn [map marker]
            (add-def map marker))
          map
          default-markers))

(defn export-ag-helper
  [ag stmt-str & options]
  (let [options-kv (apply hash-map options)
        width (get options-kv :width 1280)
        height (get options-kv :height 1024)
        map (create-graph :width width :height height)
        map (add-markers map)
        params (merge default-params options-kv)
        map (add-entities map ag stmt-str params)
        map (time (apply layout map (:layout options-kv) options))
        map (build map)]
    map))

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
