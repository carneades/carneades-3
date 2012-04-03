;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns 
    ^{:doc "A version of Carneades Argument Evaluation Structures which can handle cycles, via
            a mapping to Dung Argumentation Frameworks."}
  
  carneades.engine.caes2
  
  (:use clojure.pprint
        clojure.set
        carneades.engine.statement
        carneades.engine.argument
        carneades.engine.argument-graph
        carneades.engine.argument-evaluation))

(defrecord ArgumentationFramework
  [arguments     ; set of strings, where each string is the URN of an argument node
   attacks])     ; set of pairs of strings, [URN1 URN2], where the strings are the URNs of arguments and
                 ; the argument with the id URN1 attacks the argument with the id URN2

(defn- applicable?
  "argument-graph argument-node-id -> boolean
   Returns true if the premises of the argument have all been accepted or assumed true."
  [ag arg]
  (let [an (get (:argument-nodes ag) arg),
        hold (fn [pr]
               (let [sn (get (:statement-nodes ag) (:statement pr))]
                 (or (and (:positive pr) (>= (:weight sn) 0.75))
                     (and (not (:positive pr) (<= (:weight sn) 0.25))))))]
    (all hold (:premises an))))

(defn- undercuts?
  "argument-graph argument-node-id argument-node-id -> boolean
   Returns true if arg1 undercuts arg2."
  [ag arg1 arg2]
  (let [an1 (get (:argument-nodes ag) arg1),
        c (:atom (get (:statement-nodes ag) (:conclusion an1)))]
    (= c `(~'undercut ~arg2))))

(defn- rebuts?
  "argument-graph argument-node-id argument-node-id -> boolean
   Returns true if arg1 rebuts arg2."
  [ag arg1 arg2]
  (let [an1 (get (:argument-nodes ag) arg1),
        an2 (get (:argument-nodes ag) arg2),
        sn  (get (:statement-nodes ag) (:conclusion an1))
        alpha 0.5
        beta 0.3
        gamma 0.2]
    (and (= (:conclusion an1) (:conclusion an2))
         (not (= (:pro an1)
                 (:pro an2))) ; one argument is pro and the other con
         (case (:standard sn)
           :dv true
           :pe (> (:weight an1) (:weight an2))
           :cce (and (> (:weight an1) (:weight an2))
                     (> (:weight an1) alpha)
                     (> (- (:weight an1) (:weight an2))
                        beta))
           :brd (and (> (:weight an1) (:weight an2))
                     (> (:weight an1) alpha)
                     (> (- (:weight an1) (:weight an2)) beta)
                     (< (:weight an2) gamma))))))

(defn- undermines?
  "argument-graph argument-node-id argument-node-id -> boolean
   Returns true if arg1 undermines arg2."
  [ag arg1 arg2]
  (let [an1 (get (:argument-nodes ag) arg1)
        an2 (get (:argument-nodes ag) arg2)]
    (some (fn [pr]
            (let [sn (get (:statement-nodes ag) (:statement pr))]
              ;; an1 is pro a negative assumption or con a positive assumption
              (and (= (:conclusion an1) (:statement pr))
                   (or (and (:pro an1)
                            (not (:positive pr))
                            (> (:weight sn) 0) 
                            (<= (:weight sn) 0.25))
                       (and (not (:pro an1))
                            (:positive pr)
                            (< (:weight sn) 1.0)
                            (>= (:weight sn) 0.75))))))
          (:premises an2))))

(defn- attackers
  "argument-node-id argument-graph -> set of argument ids
   Returns the ids of the applicable arguments which attack the input argument."
  [ag arg1]
  (reduce (fn [s arg2] 
            (if (or (undercuts? ag arg2 arg1)
                    (rebuts? ag arg2 arg1)
                    (undermines? ag arg2 arg1))
              (conj s arg2)
              s))
          #{}
          (filter (fn [arg] (applicable? ag arg))
                  (keys (:argument-nodes ag)))))

(defn- make-argumentation-framework
  "argument-graph -> argumentation-framework"
  [ag]
  (let [args (keys (:argument-nodes ag)),
        attacks (apply union (map (fn [arg1]                                       ] 
                                    (set (map (fn [arg2] [arg2 arg1])
                                         (attackers ag arg1)))) 
                                  args))]
    (ArgumentationFramework. args attacks)))

(defn- evaluate-argument-graph
  "argument-graph -> argument-graph"
  [ag]
  (let [af (make-argumentation-framework ag)]
    
(def caes2-evaluator 
  (reify ArgumentEvaluator
    (evaluate [this ag] (evaluate-argument-graph (reset-node-values ag)))
    (label [this node] (node-standard-label node))))





  