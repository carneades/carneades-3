;; Copyright (c) 2011 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns 
  ^{:doc "An implementation of the argument evaluation protocol using the 
          semantics of Carneades Argument Evaluation Structures (CAES)."}
  carneades.engine.caes
  (:use clojure.pprint
        carneades.engine.statement
        carneades.engine.argument
        carneades.engine.argument-graph
        carneades.engine.argument-evaluation))

(defrecord CEState      ; Carneades Evaluation State     
  [closed-statements    ; set of statement-node ids
   closed-arguments     ; set of argument-node ids
   graph])              ; argument-graph 

(defn- make-cestate [& key-values] 
  (merge (CEState. #{} #{} (make-argument-graph))
         (apply hash-map key-values)))

(defn- cestate? [x] (instance? CEState x))

(declare all-premises-hold? satisfies-proof-standard? eval-argument-node)

(defn boolean->number 
  "boolean or nil -> number"
  [x]
  (cond
    (= x true) 1.0,
    (= x false) 0.0,
    :else nil))

(defn- applicable?
  "argument-graph argument-node -> boolean or nil
   looks up the applicability status of the argument node.  
   Precondition: the value has been previously computed and
   stored in the :value field of the node."
  [ag an]
  {:pre [(argument-graph? ag) (argument-node? an)]}
  ;; (println "applicable? " (statement-node-atom (get (:statement-nodes ag) (:conclusion an))))
  ;; (swank.core/break)
  (cond
    (= (:value an) 1.0) true,
    (= (:value an) 0.0) false,
    :else nil))

(defn- compute-argument-value
  "argument-graph argument-node -> 0.0-1.0
   An argument is applicable if all of its premises hold
   and it hasn't been undercut by an applicable argument."
  [ag an]
  {:pre [(argument-graph? ag) (argument-node? an)]
   :post [(not (nil? %))]}
  ;; (println "compute-argument-value: " (statement-node-atom (get (:statement-nodes ag) (:conclusion an))))
  ;; (swank.core/break)
  (let [pv (all-premises-hold? ag an)
        uv (set (map #(applicable? ag %) (undercutters ag an)))]
    (cond (:value an) (:value an)                     ; the value has already been computed
          ;; (or (= pv nil) (contains? uv nil)) 0.5,     ; unknown
          (= pv nil) 0.5,                             ; unknown
          (and pv (not (contains? uv true))) 1.0,
          :else 0.0)))         

(defn- acceptable?
  "argument-graph statement-node -> boolean or nil
   looks up the acceptability status of the argument node.  
   Precondition: the value has been previously computed and
   stored in the :value field of the node."
  [ag sn]
  {:pre [(argument-graph? ag) (statement-node? sn)]}
  ;; (println "acceptable? " (statement-node-atom sn))
  ;; (swank.core/break)
  (cond 
    (= (:value sn) 1.0) true,
    (= (:value sn) 0.0) false,
    :else nil))
       
(defn- compute-statement-value
  "argument-graph statement-node -> 0.0-1.0"
  [ag sn]
  {:pre [(argument-graph? ag) (statement-node? sn)]
   :post [(not (nil? %))]}
  ;; (println "compute-statement-value: " (statement-node-atom sn))
  ;; (swank.core/break)
  (cond  (:value sn) (:value sn)           ; the value has already been computed
        (and (:weight sn)
             (= (:weight sn) 1.0)) 1.0, ; P accepted
        (and (:weight sn)
             (= (:weight sn) 0.0)) 0.0, ; P rejected 
        :else (let [v (satisfies-proof-standard? ag sn)]
                (cond 
                  (= v true) 1.0,
                  (= v false) 0.0,
                  (and (:weight sn)
                       (>= (:weight sn) 0.75)) 1.0, ; P assumed true
                  (and (:weight sn)
                       (<= (:weight sn) 0.0)) 0.25, ; P asssumed false 
                  :else 0.5))))
        
(defn- eval-statement-node 
  "cestate statement-node -> cestate
   Returns a state in which the statement node has been
   evaluated in the argument graph of the state. If the value
   of the statement node in the resulting graph is nil, then
   the argument graph contained a cycle which prevents
   the statement node from being assigned a value."
  [ces1 sn] 
  {:pre [(cestate? ces1) (statement-node? sn)]}
  ;; (swank.core/break)
  (cond (contains? (:closed-statements ces1) (:id sn)) ces1,       ; cycle!
        (:value sn) ces1,                                     ; value already assigned
        :else (let [ces2 (reduce (fn [s an] (eval-argument-node s an))
                                 (assoc ces1 
                                        :closed-statements (conj (:closed-statements ces1) 
                                                                 (:id sn)))
                                 (statement-node-arguments (:graph ces1) sn))]
                (assoc ces2 
                       :graph (update-statement-node 
                                (:graph ces2) 
                                sn 
                                :value (compute-statement-value  (:graph ces2) sn))))))

(defn- eval-argument-node 
  "cestate argument-node -> cestate
   Returns a state in which the argument node has been
   evaluated in the argument graph of the state. If the value
   of the argument node in the resulting graph is nil, then
   the argument graph contained a cycle which prevent
   the argument node from being assigned a value."
  [ces1 an] 
  {:pre [(cestate? ces1) (argument-node? an)]}
  ;; (swank.core/break)
  (cond (contains? (:closed-arguments ces1) (:id an)) ces1, ; cycle!  
        (:value an) ces1,                                     ; value already assigned
        :else (let [ces2 (reduce (fn [s sn] (eval-statement-node s sn))
                                 (assoc ces1 
                                        :closed-arguments (conj (:closed-arguments ces1) 
                                                                (:id an)))
                                 (map (fn [id] (get (:statement-nodes (:graph ces1))
                                                    id))
                                      (map :statement (:premises an))))
                    ces3 (reduce (fn [s an2] (eval-argument-node s an2))
                                 ces2
                                 (undercutters (:graph ces2) an))]
                (assoc ces3 
                       :graph (update-argument-node 
                                (:graph ces3)
                                an
                                :value (compute-argument-value (:graph ces3) an))))))

(defn holds?
  "argument-graph premise -> boolean or nil
   Whether or not a premise holds depends on the weight and value of its
   statement node.  The value is not computed here, but only read
   from the argument graph."
  [ag p]
  {:pre [(argument-graph? ag) (premise? p)]}
  (let [sn (get (:statement-nodes ag) (:statement p))
        v (acceptable? ag sn)]
    (if (nil? v)
      nil ; unknown
      (if (:positive p) 
        (= v true)       
        (= v false))))) 
      
(defn- all-premises-hold?
  "argument-graph argument-node -> boolean or nil"
  [ag an]
  {:pre [(argument-graph? ag) (argument-node? an)]}
  (let [pv (set (map #(holds? ag %) (:premises an)))]
    (cond (contains? pv nil) nil,
          (every? #(= true %) pv) true,
          :else false)))

;; dispatch on the proof standard
(defmulti satisfies-proof-standard? (fn [_ sn] (:standard sn)))
  
(defn- max-weight [args]
  (if (empty? args) 0.0 (apply max (map weight args))))

(defmethod satisfies-proof-standard? :dv [ag sn]
  (let [pro (set (map (fn [arg] (if (applicable? ag arg) arg nil))
                      (pro-argument-nodes ag sn)))
        app-pro (filter #(not (nil? %)) pro),
        con (set (map (fn [arg] (if (applicable? ag arg) arg nil))
                      (con-argument-nodes ag sn))),
        app-con (filter #(not (nil? %)) con)]
    (cond (or (contains? pro nil) (contains? con nil)) nil,
          (and (not (empty? app-pro) (empty? app-con))) true,
          (and (not (empty? app-con) (empty? app-pro))) false, 
          :else nil)))


(defmethod satisfies-proof-standard? :pe [ag sn]
  (let [pro (set (map (fn [arg] (if (applicable? ag arg) arg nil))
                      (pro-argument-nodes ag sn)))
        app-pro (filter #(not (nil? %)) pro),
        con (set (map (fn [arg] (if (applicable? ag arg) arg nil))
                      (con-argument-nodes ag sn))),
        app-con (filter #(not (nil? %)) con)]
    (cond 
      (or (contains? pro nil) (contains? con nil)) nil,
      (> (max-weight app-pro) (max-weight app-con)) true,
      (> (max-weight app-con) (max-weight app-pro)) false,
      :else nil)))

;; clear-and-convincing-evidence?
(defmethod satisfies-proof-standard? :cce [ag sn]
  (let [pro (set (map (fn [arg] (if (applicable? ag arg) arg nil))
                      (pro-argument-nodes ag sn)))
        app-pro (filter #(not (nil? %)) pro),
        con (set (map (fn [arg] (if (applicable? ag arg) arg nil))
                      (con-argument-nodes ag sn))),
        app-con (filter #(not (nil? %)) con),
        max-app-pro (max-weight app-pro),
        max-app-con (max-weight app-con),
        alpha 0.5
        beta 0.3]
    (cond (or (contains? pro nil) (contains? con nil)) nil,
          (and (> max-app-pro max-app-con)
               (> max-app-pro alpha)
               (> (- max-app-pro max-app-con) beta)) true,
          (and (> max-app-con max-app-pro)
               (> max-app-con alpha)
               (> (- max-app-con max-app-pro) beta)) false,
          :else nil)))

;; beyond-reasonable-doubt?
(defmethod satisfies-proof-standard? :brd [ag sn]
  (let [pro (set (map (fn [arg] (if (applicable? ag arg) arg nil))
                      (pro-argument-nodes ag sn)))
        app-pro (filter #(not (nil? %)) pro),
        con (set (map (fn [arg] (if (applicable? ag arg) arg nil))
                      (con-argument-nodes ag sn))),
        app-con (filter #(not (nil? %)) con),,
        max-app-pro (max-weight app-pro),
        max-app-con (max-weight app-con),
        alpha 0.5
        beta 0.3
        gamma 0.2]
    (cond (or (contains? pro nil) (contains? con nil)) nil,
          (and (> max-app-pro max-app-con)
               (> max-app-pro alpha)
               (> (- max-app-pro max-app-con) beta)
               (< max-app-con gamma)) true,
          (and (> max-app-con max-app-pro)
               (> max-app-con alpha)
               (> (- max-app-con max-app-pro) beta)
               (< max-app-pro gamma)) false,
          :else nil)))

(defn- evaluate-argument-graph
  [ag]
  (:graph (reduce (fn [ces sn] (eval-statement-node ces sn))
                  (make-cestate :graph ag)
                  (vals (:statement-nodes ag)))))

(def caes 
  (reify ArgumentEvaluator
    (evaluate [this ag] (evaluate-argument-graph (reset-node-values ag)))
    (label [this node] (node-standard-label node))))
         
         
         
         
