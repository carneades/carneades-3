;; Copyright (c) 2010 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Export from argument graphs to XML using the Carneades Argument Format (CAF)."}
  carneades.xml.caf.export
  (:require [clojure.string :as s]
            [clojure.data.xml :refer :all]
            [carneades.engine.utils :refer [serialize-atom]]
            [taoensso.timbre :as timbre :refer [debug info spy]]))

; TO DO: 
; - exporting namespaces. Should namespaces be encoded using XML entities, as we did with LKIF? Alternatively,
;   CAF could be extended with elements for the namespaces.  Perhaps another name should be used, to 
;   avoid confusion with XML namespaces, which are at another level.

(defn- remove-nils-map
  [m]
  (into {} (remove (comp nil? second) m)))

(defn- remove-blank-values
  [m]
  (into {} (remove (comp #(and (string? %) (s/blank? %)) second) m)))

(defn- clean-header
  [header]
  (reduce-kv (fn [m k v]
               (if (and (not= k :description)
                        (s/blank? v))
                 m
                 (assoc m k v)))
             {}
             header))

(defn- descriptions
  [descs]
  (apply element
         :descriptions
         {}
         (reduce-kv (fn [elts k v]
                      (conj elts
                            (element :description {:lang (name k)}
                                     v)))
                    []
                    (remove-blank-values descs))))

(defn- metadata
  "Build an element from the header. Return nil if the header has no values."
  [header]
  (let [header (clean-header header)]
    (when-not (empty? header)
      (if (:description header)
        (let [desc (:description header)]
          (element :metadata (dissoc header :description)
                   (descriptions (:description header))))
        (element :metadata header)))))

(defn- pack-statement
  [{:keys [standard atom] :as stmt}]
  (let [stmt (-> stmt
                 remove-nils-map
                 remove-blank-values
                 (select-keys [:id :weight :value :standard :atom :main])
                 (assoc :standard (.toUpperCase (name standard))))]
    (if (:atom stmt)
      (assoc stmt :atom (serialize-atom atom))
      stmt)))

(defn- statement
  [stmt]
  (element :statement (pack-statement stmt)
           (metadata (:header stmt))
           (descriptions (:text stmt))))

(defn- statements
  [stmts]
  (element :statements
           {}
           (map statement stmts)))

(defn- conclusion
  [concl]
  (element :conclusion
           {:statement concl}))

(defn- premise
  [prem]
  (let [prem' (-> prem
                  remove-nils-map
                  (select-keys [:positive :role :implicit :statement]))]
    (element :premise prem')))

(defn- premises
  [prems]
  (element :premises
           {}
           (map premise prems)))

(defn- argument
  [arg]
  (let [arg' (-> arg
                 remove-nils-map
                 (select-keys [:id :strict :pro :scheme :weight :value]))]
    (element :argument
             arg'
             (metadata (:header arg))
             (conclusion (:conclusion arg))
             (premises (:premises arg)))))

(defn- arguments
  [args]
  (element :arguments
           {}
           (map argument args)))

(defn references
  [g]
  (element :references
           {}))

(defn export
  [g]
  (indent-str (element :caf {:version "1.3"}
                       (metadata (:header g))
                       (statements (vals (:statement-nodes g)))
                       (arguments (vals (:argument-nodes g)))
                       (references (:references g))
                       )))
  

;; tests in carneades.xml.caf.export-test
