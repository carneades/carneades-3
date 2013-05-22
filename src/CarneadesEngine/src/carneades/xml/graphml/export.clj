;;; Copyright (c) 2010 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Functions for exporting argument graphs to GraphML."}
  carneades.xml.graphml.export
  (:use carneades.engine.dublin-core
        carneades.engine.statement
        carneades.engine.argument-graph)
  (:require [clojure.prxml :as prx]
            [clojure.string :as str]))

;; TO DO: - exporting namespaces. Should namespaces be encoded using
;; XML entities, as we did with LKIF? Alternatively, CAF could be
;; extended with elements for the namespaces.  Perhaps another name
;; should be used, to avoid confusion with XML namespaces, which are at
;; another level.

(defn- metadata->xml
  [md]
  ;; {:pre [(metadata? md)]}
  [:c:metadata 
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
               (conj v [:c:description 
                        {:lang (name (first description))} 
                        (second description)])))
           [:c:descriptions]
           (:c:description md))])

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

(defn- clean [s]
  (str/trim (str/replace s #"( |\t|\r)( |\t|\r)+" " ")))

(defn- statement-nodes->graphml
  [stmt-nodes lang]
  (reduce (fn [v stmt] 
	    (conj v [:node {:id (str (:id stmt)) }
                     ;; (if (nil? (:header stmt)) ""
                     ;;     [:data {:key "metadata"} (metadata->xml (:header stmt))])
                     ;; (if (nil? (:atom stmt)) ""
                     ;;     [:data {:key "atom"} (pack-atom (statement-node-atom stmt))])
                     ;; [:data {:key "standard"} (standard->string (:standard stmt))]
                     [:data {:key "d6"}
                      [:y:ShapeNode
                       [:y:Geometry {:width "178.0" :height "51.0"}]
                       [:y:BorderStyle {:color "#000000" :type "line" :width "1.0"}]
                       [:y:NodeLabel {:alignment "left" :autoSizePolicy "node_size"
                                      :configuration "CroppingLabel"
                                      :width "178.0" :height "51.0"
                                      :modelName "internal" :modelPosition "c"
                                      :fontFamily "Dialog" :fontSize "12"
                                      :fontStyle "plain" :hasBackgroundColor "false" 
                                      :hasLineColor "false" :textColor "#000000" 
                                      :visible "true"} 
                        (clean (lang (:text stmt))) 
                        [:y:Shape {:type "rectangle"}]]]]
                     ;; [:data {:key "descriptions"}
                     ;;  (reduce (fn [v description]
                     ;;    	(if (str/blank? (second description)) 
                     ;;    	  v
                     ;;    	  (conj v [:c:description 
                     ;;    		   {:lang (name (first description))} 
                     ;;    		   (second description)])))
                     ;;          [:c:descriptions]
                     ;;          (dissoc (:text stmt) :id))]
                     ]))
          []
          stmt-nodes))

(defn- argument-nodes->graphml
  [arg-nodes]
  (reduce (fn [v arg]
            (-> (conj v [:node {:id (str (:id arg)) }
                         ;; (if (nil? (:header arg)) ""
                         ;;     [:data {:key "metadata"} (metadata->xml (:header arg))])
                         [:data {:key "d6"}
                          [:y:ShapeNode
                           [:y:BorderStyle {:color "#000000" :type "line" :width "1.0"}]
                           [:y:NodeLabel {:alignment "center" :autoSizePolicy "content" 
                                          :fontFamily "Dialog" :fontSize "12"
                                          :fontStyle "plain" :hasBackgroundColor "false" 
                                          :hasLineColor "false" :height "18.1328125" 
                                          :modelName "custom" :textColor "#000000" 
                                          :visible "true" :width "28.87890625"} 
                            (if (:pro arg) "+" "-")] 
                           [:y:Shape {:type "ellipse"}]]]]
                      [:edge {:id (str (gensym "e")) 
                              :directed "true"
                              :source (str (:id arg))
                              :target (str (literal-atom (:conclusion arg)))}
                       [:data {:key "d10"}
                        [:y:PolyLineEdge 
                         [:y:LineStyle {:color "#000000" :type "line" :width "1.0"}]
                         [:y:Arrows {:source (if (:pro arg) "none" "dash")
                                     :target "standard"}]
                         [:y:BendStyle {:smoothed "false"}]]]])
                ((fn [v s] (apply conj v s))
                 (reduce (fn [v p] 
                           (conj v [:edge {:id (str (gensym "e")) 
                                           :source (str (literal-atom (:statement p)))
                                           :target (str (:id arg))}
                                    [:data {:key "d10"}
                                     [:y:LineStyle {:color "#000000" :type "line" :width "1.0"}]
                                     [:y:BendStyle {:smoothed "false"}]
                                     [:y:Arrows {:source (if (:positive p) "none" "dash") 
                                                 :target "none"}]]]))
                         []
                         (:premises arg)))))
          []
          arg-nodes))

(defn- references->xml
  [refs]
  (reduce (fn [v r] (conj v (metadata->xml r)))
          [:references]
          refs))

(defn argument-graph->graphml
  [ag lang] ; where lang is in #{:en, :de, ...}
  (prx/prxml 
   [:graphml {:xmlns "http://graphml.graphdrawing.org/xmlns"
              :xmlns:xsi "http://www.w3.org/2001/XMLSchema-instance"
              :xmlns:y "http://www.yworks.com/xml/graphml"
              :xmlns:yed "http://www.yworks.com/xml/yed/3"
              :xsi:schemaLocation "http://graphml.graphdrawing.org/xmlns http://www.yworks.com/xml/schema/graphml/1.1/ygraphml.xsd"
              ;; :xmlns:c "http://carneades.github.com"
              }
    [:key {:id "d0" :for "graphml" :yfiles.type "resources"}]
    [:key {:id "d1" :for "port" :yfiles.type "portgraphics"}]
    [:key {:id "d2" :for "port" :yfiles.type "portgeometry"}]
    [:key {:id "d3" :for "port" :yfiles.type "portuserdata"}]
    [:key {:id "d4" :attr.name "url" :for "node" :attr.type "string"}]
    [:key {:id "d5" :attr.name "description" :for "node" :attr.type "string"}]
    [:key {:id "d6" :for "node" :yfiles.type "nodegraphics"}]
    [:key {:id "d7" :attr.name "Description" :for "graph"  :attr.type "string"}]
    [:key {:id "d8" :attr.name "url" :for "edge" :attr.type "string"}]
    [:key {:id "d9" :attr.name "description" :for "edge" :attr.type "string"}]
    [:key {:id "d10" :for "edge" :yfiles.type "edgegraphics"}]
    ;; [:key {:id "metadata" :attr.name "metadata" :for "all" :attr.type "string"}]
    ;; [:key {:id "references" :attr.name "references" :for "graph" :attr.type "string"}]
    ;; [:key {:id "atom" :attr.name "atom" :for "node" :attr.type "string"}]
    ;; [:key {:id "standard" :attr.name "standard" :for "node" :attr.type "string"}]
    ;; [:key {:id "descriptions" :attr.name "descriptions" :for "node" :attr.type "string"}]
    (-> [:graph {:edgedefault "undirected" :id "G"}
         ;; (if (nil? (:header ag)) "" [:data {:key "metadata"} (metadata->xml (:header ag))])
         ]
        ((fn [v s] (reduce conj v s)) 
         (statement-nodes->graphml 
          (vals (:statement-nodes ag)) 
          lang))
        ((fn [v s] (reduce conj v s))
         (argument-nodes->graphml (vals (:argument-nodes ag))))
        ;; (conj [:data {:key "references"} 
        ;;        (references->xml (vals (:references ag)))])
        )]))

