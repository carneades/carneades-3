;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.examples.maptest
  (:use carneades.engine.argument
        carneades.engine.statement
        carneades.engine.lkif
        carneades.mapcomponent.export)
  (:require [carneades.mapcomponent.viewer :as jgraphx]
            [carneades.ui.diagram.viewer :as graphviz]))

(def a1 (make-argument 
          :id 'a1
          :conclusion "P"
          :premises ["Q", "R", (¬ "S")]))

(def a2 (make-argument
          :id 'a2
          :conclusion (¬ "P")
          :premises [(¬ "T"), (¬ "U"), "V"]))

(def a3 (make-argument 
          :id 'a3
          :conclusion "R"
          :premises ["W"]))

(def a4 (make-argument
          :id 'a4
          :conclusion "V"
          :premises ["X"]))

(def a5 (make-argument 
          :id 'a5
          :conclusion "P"
          :premises ["Y"]))

(def ag1 
  (-> (make-argument-graph :id 'ag1 :title "test" :main-issue "P")
      (assert-arguments [a1 a2 a3 a4 a5]))
      (accept ["Q" (¬ "S") "W" "X"])
      (reject ["T"]) ; to check that reject is same as accepting the complement
      (question ["R" "Y"]))

; (define ld (make-lkif-data (lkif-data-sources kb1) (lkif-data-rulebase kb1) (list ag1)))
;; (def ld (make-lkif-data '() (rulebase) (list ag1)))
;; (lkif-export '() ld "argument-map-tests2.xml")

(defn main []
  ;; (jgraphx/view ag1)
  ;; (graphviz/view ag1)
  (export-ag ag1 statement-formatted "/tmp/maptest.svg" :layout :radial)
  (export-lkif {:ags [ag1]} "/tmp/maptest.lkif"))