;; Copyright (c) 2010 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Functions for exporting argument graphs to XML using the Carneades Argument Format (CAF)."}
    carneades.xml.caf.export
  (:use carneades.engine.dublin-core
        carneades.engine.statement
        carneades.engine.argument-graph)
  (:require [clojure.prxml :as prx]
            [clojure.string :as str]))

; TO DO: 
; - exporting namespaces. Should namespaces be encoded using XML entities, as we did with LKIF? Alternatively,
;   CAF could be extended with elements for the namespaces.  Perhaps another name should be used, to 
;   avoid confusion with XML namespaces, which are at another level.

(defn- metadata->xml
  [md]
  ; {:pre [(metadata? md)]}
  [:metadata 
   (dissoc md 
           :description 
           (when (str/blank? (:key md)) :key)
           (when (str/blank? (:contributor md)) :contributor)
           (when (str/blank? (:coverage md)) :coverage)
           (when (str/blank? (:creator md)) :creator)
           (when (str/blank? (:date md)) :date)
           (when (str/blank? (:format md)) :format)
           (when (str/blank? (:identifier md)) :identifier)
           (when (str/blank? (:language md)) :language)
           (when (str/blank? (:publisher md)) :publisher)
           (when (str/blank? (:relation md)) :relation)
           (when (str/blank? (:rights md)) :rights)
           (when (str/blank? (:source md)) :source)
           (when (str/blank? (:subject md)) :subject)
           (when (str/blank? (:title md)) :title)
           (when (str/blank? (:type md)) :type))
   (reduce (fn [v description]
             (if (str/blank? (second description)) 
               v
               (conj v [:description 
                        {:lang (name (first description))} 
                        (second description)])))
           [:descriptions]
           (:description md))])

(defn- pack-atom
  [atom]
  (cond (sliteral? atom) (str atom),
        (statement? atom) (str (literal-atom atom)),
        :else ""))

(defn- standard->string
  [ps]
  {:pre [(contains? #{:pe, :cce, :brd, :dv} ps)]}
  (condp = ps
    :pe "PE",
    :cce "CCE",
    :brd "BRD",
    :dv "DV"))
 
(defn- statement-nodes->xml
  [stmt-nodes]
  (reduce (fn [v stmt] 
            (let [stmt1 (-> stmt 
                            (dissoc :text :header :premise-of :pro :con
                                    (when (nil? (:value stmt)) :value)
                                    (when (nil? (:weight stmt)) :weight))
                            (assoc :id (str (:id stmt)))
                            (assoc :standard (standard->string (:standard stmt)))
                            (assoc :atom (pack-atom (statement-node-atom stmt))))]
              (conj v [:statement (dissoc stmt1 (when (empty? (:atom stmt1)) :atom))
                       (if (nil? (:header stmt)) "" (metadata->xml (:header stmt)))
                       (reduce (fn [v description]
                                 (if (str/blank? (second description)) 
                                   v
                                   (conj v [:description 
                                            {:lang (name (first description))} 
                                            (second description)])))
                               [:descriptions]
                               (dissoc (:text stmt) :id))])))
          [:statements]
          stmt-nodes))

(defn- argument-nodes->xml
  [arg-nodes]
  (reduce (fn [v arg]
            (conj v [:argument 
                     (-> arg
                         (assoc :id (str (:id arg)))
                         (dissoc :header :conclusion :premises :rebuttals :undercutters
                                 (when (nil? (:weight arg)) :weight)
                                 (when (nil? (:value arg)) :value)))
                     (if (nil? (:header arg)) "" (metadata->xml (:header arg)))
                     [:conclusion {:statement (literal-atom (:conclusion arg))}]
                     (reduce (fn [v p] (conj v [:premise (dissoc p :id :argument :pro :con)]))
                             [:premises]
                             (:premises arg))]))
          [:arguments]
          arg-nodes))

(defn- references->xml
  [refs]
  (reduce (fn [v r] (conj v (metadata->xml r)))
          [:references]
          refs))
   
(defn argument-graph->xml
  [ag]
  (prx/prxml 
    [:caf {:version "1.3"}
     (metadata->xml (:header ag))
     (statement-nodes->xml (vals (:statement-nodes ag)))
     (argument-nodes->xml (vals (:argument-nodes ag)))
     (references->xml (vals (:references ag)))]))


  
