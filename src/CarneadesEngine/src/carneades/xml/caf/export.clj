;;; Copyright (c) 2010 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Functions for exporting argument graphs to XML using the Carneades Argument Format (CAF)."}
    carneades.xml.caf.export
  (:use carneades.engine.statement
        carneades.engine.argument-graph
        clojure.contrib.prxml))

; TO DO: 
; - exporting namespaces. Should namespaces be encoded using XML entities, as we did with LKIF? Alternatively,
;   CAF could be extended with elements for the namespaces.  Perhaps another name should be used, to 
;   avoid confusion with XML namespaces, which are at another level.

(defrecord Metadata
  [contributor      ; string or nil 
   coverage         ; string or nil 
   creator          ; string or nil 
   date             ; string or nil 
   description      ; (language -> string) map or nil
   format           ; string or nil 
   identifier       ; string or nil 
   language         ; string or nil 
   publisher        ; string or nil 
   relation         ; string or nil 
   rights           ; string or nil 
   source           ; string or nil 
   subject          ; string or nil 
   title            ; string or nil 
   type])           ; string or nil 

(defn- metadata->xml
  [md]
  [:metadata 
   (dissoc md 
           :description
           (if (empty? (:contributor md)) :contributor)
           (if (empty? (:coverage md)) :coverage)
           (if (empty? (:creator md)) :creator)
           (if (empty? (:date md)) :date)
           (if (empty? (:format md)) :format)
           (if (empty? (:identifier md)) :identifier)
           (if (empty? (:language md)) :language)
           (if (empty? (:publisher md)) :publisher)
           (if (empty? (:relation md)) :relation)
           (if (empty? (:rights md)) :rights)
           (if (empty? (:source md)) :source)
           (if (empty? (:subject md)) :subject)
           (if (empty? (:title md)) :title)
           (if (empty? (:type md)) :type))
   (reduce (fn [v description]
             (if (empty? (second description)) 
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
                                            (if (empty? (:value stmt)) :value)
                                            (if (empty? (:weight stmt)) :weight))
                                    (assoc :standard (standard->string (:standard stmt)))
                                    (assoc :atom (pack-atom (:atom stmt))))
                     (metadata->xml (:header stmt))
                     (reduce (fn [v description]
                               (if (empty? (second description)) 
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
                             (if (empty? (:weight arg)) :weight)
                             (if (empty? (:value arg)) :value))
                     (metadata->xml (:header arg))
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
     (references->xml (vals (:references ag)))]))


  