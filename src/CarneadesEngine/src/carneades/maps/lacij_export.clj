;;; Copyright (c) 2010-2011 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.maps.lacij-export
  (:use clojure.pprint
        carneades.engine.argument-graph
        carneades.engine.argument-evaluation
        carneades.engine.caes
        lacij.graph.svg.graph
        lacij.graph.core
        lacij.layouts.layout
        lacij.view.core
        lacij.opt.annealing
        carneades.engine.statement
        [carneades.maps lacij-params format-statement subset-ag])
  (:require [analemma.xml :as xml]
            [analemma.svg :as svg]
            [clojure.string :as s]
            [tikkba.utils.dom :as dom])
  (:import (java.io InputStreamReader ByteArrayInputStream
                    ByteArrayOutputStream)))

(defn gen-arg-id
  [arg]
  (keyword (str "a-" (:id arg))))

(defn gen-stmt-id
  [stmt]
  (keyword (str "s-" (:id stmt))))

(defn pick-stmt-params
  [ag stmt]
  (cond  (in-node? stmt) (:stmt-in-params default-params)
         (out-node? stmt) (:stmt-out-params default-params)
         :else (:stmt-params default-params)))

(defn pick-arg-params
  [ag arg]
  (cond (and (in-node? arg) (:pro arg))
        (:arg-pro-applicable-params default-params)
        
        (and (in-node? arg) (:con arg))
        (:arg-con-applicable-params default-params)
        
        (:pro arg)
        (:arg-pro-notapplicable-params default-params)
        
        :else
        (:arg-con-notapplicable-params default-params)))

(defn add-statement
  [svgmap stmt ag stmt-str]
  ;; TODO take the lang as a parameter
  (let [stmtstr (stmt-to-str stmt stmt-str)
        id (gen-stmt-id stmt)
        stmt-params (merge (pick-stmt-params ag stmt)
                           {:label "" :x 0 :y 0 :shape :rect})
        svgmap (add-node-kv svgmap id stmt-params)
        svgmap (add-label-kv svgmap id
                          (trunk-line stmtstr)
                          (:stmtlabel-params default-params))]
    svgmap))

(defn add-arg-decorator
  [svgmap arg argid]
  {:pre [(argument-node? arg)]}
  (if (:pro arg)
    (add-decorator svgmap argid (make-plusdecorator))
    (add-decorator svgmap argid (make-minusdecorator))))

(defn undercutter?
  [ag arg]
  (let [stmtconclusion (map->statement ((:statement-nodes ag) (:conclusion arg)))]
   (= 'undercut (literal-predicate stmtconclusion))))

(defn add-undercutter-argument-node
  [svgmap arg ag]
  {:pre [(not (string? (:scheme arg)))]}
  (let [scheme (:scheme arg)
        label (if scheme
                (str "<" (term-functor scheme) ">")
                "")]
   (add-node-kv svgmap (gen-arg-id arg) (merge {:label label} undercutter-params))))

(defn add-normal-argument-node
  [svgmap arg ag]
  (let [arg-params (pick-arg-params ag arg)
        argid (gen-arg-id arg)
        svgmap (add-node-kv svgmap argid arg-params)
        svgmap (add-arg-decorator svgmap arg argid)]
    svgmap))

(defn add-argument-node
  [svgmap arg ag]
  (if (undercutter? ag arg)
    (add-undercutter-argument-node svgmap arg ag)
    (add-normal-argument-node svgmap arg ag)))

(defn add-conclusion-edge
  [svgmap conclusion arg]
  (let [id (geneid)
        conclusionid (gen-stmt-id conclusion)
        argid (gen-arg-id arg)]
    (if (:pro arg)
      (apply add-edge svgmap id argid conclusionid pro-argument-params)
      (apply add-edge svgmap id argid conclusionid con-argument-params))))

(defn add-premise-edge
  [svgmap ag premise arg]
  (let [edgeid (geneid)
        argid (gen-arg-id arg)
        stmtid (gen-stmt-id (get (:statement-nodes ag) (:statement premise)))]
    (if (:positive premise)
      (apply add-edge svgmap edgeid stmtid argid premise-params)
      (apply add-edge svgmap edgeid stmtid argid neg-premise-params))))

(defn add-premises-edges
  [svgmap ag premises arg]
  (reduce (fn [svgmap premise]
            (add-premise-edge svgmap ag premise arg))
          svgmap premises))

(defn add-argument
  [svgmap arg ag]
  (add-argument-node svgmap arg ag))

(defn filter-out-undercutters-conclusions
  [statements]
  (filter #(not= 'undercut (literal-predicate (map->statement %))) statements))

(defn add-entities
  [svgmap ag stmt-str]
  (let [statements (filter-out-undercutters-conclusions
                    (vals (:statement-nodes ag)))
        arguments (vals (:argument-nodes ag))]
    (let [svgmap (reduce (fn [svgmap stmt]
                           (add-statement svgmap stmt ag stmt-str))
                         svgmap statements)
          svgmap (reduce (fn [svgmap arg]
                           (add-argument svgmap arg ag))
                         svgmap arguments)]
      svgmap)))

(defn add-undercutter-edge
  [svgmap ag undercutter arg]
  (let [edgeid (geneid)
        argid (gen-arg-id arg)
        undercutterid (gen-arg-id undercutter)]
    (apply add-edge svgmap edgeid undercutterid argid undercutter-edge-params)))

(defn add-undercutters-edges
  [svgmap ag undercutters arg]
  (reduce (fn [svgmap undercutter]
            (add-undercutter-edge svgmap ag undercutter arg))
          svgmap
          undercutters))

(defn link-argument
  [svgmap arg ag]
  (let [conclusion (get (:statement-nodes ag) (literal-atom (:conclusion arg)))
        svgmap (if (undercutter? ag arg)
                 svgmap
                 (add-conclusion-edge svgmap conclusion arg))
        svgmap (add-premises-edges svgmap ag (:premises arg) arg)
        svgmap (add-undercutters-edges svgmap ag (undercutters ag arg) arg)]
    svgmap))

(defn link-entities
  [svgmap ag]
  (reduce (fn [svgmap arg]
            (link-argument svgmap arg ag))
          svgmap (vals (:argument-nodes ag))))

(defn add-markers
  [map]
  (reduce (fn [map marker]
            (add-def map marker))
          map
          default-markers))

(defn export-ag-helper
  [ag stmt-str options]
  (let [width (get options :width 1280)
        height (get options :height 1024)
        layouttype (get options :layout :hierarchical)
        svgmap (create-graph :width width :height height)
        svgmap (add-markers svgmap)
        ag (apply subset-ag ag options)
        svgmap (add-entities svgmap ag stmt-str)
        svgmap (link-entities svgmap ag)
        optionsseq (flatten (seq options))
        svgmap (apply layout svgmap layouttype optionsseq)
        svgmap (build svgmap)]
    svgmap))

(defn export-ag
  "Exports an argument graph to a SVG file.

   Options are :treeify, :full-duplication, :depth,
   :layout and all options supported by the layout"
  [ag stmt-str filename options]
  (let [map (export-ag-helper ag stmt-str options)]
    (export map filename :indent "yes")))

(defn export-ag-str
  "Exports an argument graph to a string.

   Options are :treeify, :full-duplication, :depth,
   :layout and all options supported by the layout"
  [ag stmt-str options]
  (let [map (export-ag-helper ag stmt-str options)
        os (ByteArrayOutputStream.)
        v (view map)]
    (dom/spit-xml os v :indent "yes")
    (slurp
     (InputStreamReader.
      (ByteArrayInputStream. (.toByteArray os))
      "UTF8"))))
