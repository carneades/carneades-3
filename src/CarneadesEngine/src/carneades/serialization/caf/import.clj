;; Copyright (c) 2014 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Import from XML to argument graphs using the Carneades
  Argument Format (CAF)."}
  carneades.serialization.caf.import
  (:require [clojure.data.xml :as x]
            [clojure.java.io :as io]
            [clojure.data.zip.xml :refer [attr text xml->]]
            [clojure.zip :as z]
            [carneades.serialization.xml.validation :refer [create-validation-fn]]
            [taoensso.timbre :as timbre :refer [debug info spy]]
            [carneades.engine.utils :refer [unserialize-atom]]
            [carneades.engine.statement :as st]
            [carneades.engine.argument :as a]
            [carneades.engine.argument-graph :as ag])
  (:refer-clojure :exclude [import]))

(defn- import-description
  [desc]
  (let [n (z/node desc)]
    {(keyword (-> n :attrs :lang))
     (first (:content n))}))

(defn- import-metadata
  [md]
  (let [descs (map import-description (xml-> md :descriptions :description))
        attrs (:attrs (z/node md))]
    (if (empty? descs)
      attrs
      (merge attrs {:description (apply merge descs)}))))

(defn- import-boolean
  [s]
  (when s
   (Boolean/parseBoolean s)))

(defn- import-atom
  [s]
  (when s
    (unserialize-atom s)))

(defn- import-float
  [s]
  (when s
   (Float/parseFloat s)))

(defn- import-standard
  [s]
  (if s
    (keyword (.toLowerCase s))
    :pe))

(defn- import-statement
  [stmt]
  (let [metadata (first (map import-metadata (xml-> stmt :metadata)))
        descs (map import-description (xml-> stmt :descriptions :description))
        text (apply merge descs)
        attrs (:attrs (z/node stmt))]
    (-> attrs
        (update-in [:id] symbol)
        (update-in [:atom] unserialize-atom)
        (assoc :header metadata)
        (update-in [:weight] import-float)
        (update-in [:value] import-float)
        (update-in [:main] import-boolean)
        (update-in [:standard] import-standard)
        (assoc :text text)
        (st/map->statement))))

(defn- import-conclusion
  [conclusion]
  (symbol (-> (z/node conclusion) :attrs :statement)))

(defn- find-statement
  [id stmts]
  (first (filter #(= (:id %) id) stmts)))

(defn- import-premise
  [stmts prem]
  (let [attrs (:attrs (z/node prem))
        stmtid (symbol (:statement attrs))
        stmt (find-statement stmtid stmts)]
    (-> attrs
        (update-in [:positive] (fn [s] (if (nil? s) true (import-boolean s))))
        (update-in [:implicit] import-boolean)
        (assoc :statement stmt)
        (a/map->premise))))

(defn- import-argument
  [stmts arg]
  (let [metadata (first (map import-metadata (xml-> arg :metadata)))
        conclusionid (import-conclusion (first (xml-> arg :conclusion)))
        conclusion (find-statement conclusionid stmts)
        premises (map (partial import-premise stmts) (xml-> arg :premises :premise))
        exceptions (map (partial import-premise stmts) (xml-> arg :exceptions :exception))
        attrs (:attrs (z/node arg))]
    (-> attrs
        (update-in [:id] symbol)
        (update-in [:strict] import-boolean)
        (update-in [:weight] import-float)
        (update-in [:value] import-float)
        (assoc :conclusion conclusion)
        (update-in [:pro] (fn [s] (if (nil? s) true (import-boolean s))))
        (assoc :premises premises)
        (assoc :exceptions exceptions)
        (assoc :header metadata)
        (a/map->argument))))

(defn import-caf
  [xml]
  (let [zipper (z/xml-zip xml)
        metadata (first (map import-metadata (xml-> zipper :metadata)))
        statements (map import-statement (xml-> zipper :statements :statement))
        arguments (map (partial import-argument statements)
                       (xml-> zipper :arguments :argument))
        references (map import-metadata (xml-> zipper :references :metadata))
        g (ag/make-argument-graph :header metadata)
        g (ag/enter-statements g statements)
        g (ag/enter-arguments g arguments)
        g (ag/enter-references g references)]
    g))

(defn import
  ([x]
     (let [xml (x/parse (io/reader x))]
       (import-caf xml)))
  ([x validate]
     (if validate
       (let [validator (create-validation-fn (.getPath (io/resource "test/schemas/CAF.xsd")))
             input (slurp x)
             result (validator input)]
         (if (:left result)
           (throw (ex-info "Invalid XML document." {:error (:left result)}))
           (import-caf (x/parse-str input))))
       (import x))))

;; tests in carneades.serialization.caf.import-test
