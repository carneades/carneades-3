;;; Copyright (c) 2010 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Functions for exporting argument graphs to XML using the Carneades Argument Format (CAF)."}
    carneades.xml.caf.export
  (:use clojure.string
        carneades.engine.statement
        carneades.engine.argument-graph
        clojure.contrib.prxml))

; TO DO: 
; - exporting namespaces. Should namespaces be encoded using XML entities, as we did with LKIF? Alternatively,
;   CAF could be extended with elements for the namespaces.  Perhaps another name should be used, to 
;   avoid confusion with XML namespaces, which are at another level.

(defn- metadata->xml
  [md]
  [:metadata 
   (dissoc md 
           :description
           (when (blank? (:contributor md)) :contributor)
           (when (blank? (:coverage md)) :coverage)
           (when (blank? (:creator md)) :creator)
           (when (blank? (:date md)) :date)
           (when (blank? (:format md)) :format)
           (when (blank? (:identifier md)) :identifier)
           (when (blank? (:language md)) :language)
           (when (blank? (:publisher md)) :publisher)
           (when (blank? (:relation md)) :relation)
           (when (blank? (:rights md)) :rights)
           (when (blank? (:source md)) :source)
           (when (blank? (:subject md)) :subject)
           (when (blank? (:title md)) :title)
           (when (blank? (:type md)) :type))
   (reduce (fn [v description]
             (if (blank? (second description)) 
               v
               (conj v [:description 
                        {:lang (name (first description))} 
                        (second description)])))
           [:descriptions]
           (:description md))])

(defn- pack-atom
  [atom]
  (cond (sliteral? atom) (str atom),
        (statement? atom) (str (:atom atom)),
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
            (conj v [:statement (-> stmt 
                                    (dissoc :text :header :premise-of :pro :con
                                            (when (nil? (:value stmt)) :value)
                                            (when (nil? (:weight stmt)) :weight))
                                    (assoc :standard (standard->string (:standard stmt)))
                                    (assoc :atom (pack-atom (:atom stmt))))
                     (if (nil? (:header stmt)) "" (metadata->xml (:header stmt)))
                     (reduce (fn [v description]
                               (if (blank? (second description)) 
                                 v
                                 (conj v [:description 
                                          {:lang (name (first description))} 
                                          (second description)])))
                             [:descriptions]
                             (dissoc (:text stmt) :id))]))
          [:statements]
          stmt-nodes))

(defn- argument-nodes->xml
  [arg-nodes]
  (reduce (fn [v arg]
            (conj v [:argument 
                     (dissoc arg :header :conclusion :premises 
                             (when (nil? (:weight arg)) :weight)
                             (when (nil? (:value arg)) :value))
                     (if (nil? (:header arg)) "" (metadata->xml (:header arg)))
                     [:conclusion {:statement (literal-atom (:conclusion arg))}]
                     (reduce (fn [v p] (conj v [:premise (dissoc p :id :argument)]))
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
  (prxml 
    [:caf {:version "1.1"}
     (metadata->xml (:header ag))
     (statement-nodes->xml (vals (:statement-nodes ag)))
     (argument-nodes->xml (vals (:argument-nodes ag)))
     (references->xml (:references ag))]))


  