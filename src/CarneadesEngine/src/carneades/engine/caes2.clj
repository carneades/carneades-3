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
        carneades.engine.dung
        carneades.engine.argument-graph
        carneades.engine.argument-evaluation))

(defn- applicable?
  "argument-graph argument-node -> boolean
   Returns true if the premises of the argument have all been accepted or assumed true."
  [ag an]
  (let [hold (fn [pr]
               (let [sn (get (:statement-nodes ag) (:statement pr))]
                 (or (and (:positive pr) (:weight sn) (>= (:weight sn) 0.75))
	           (and (not (:positive pr)) (:weight sn) (<= (:weight sn) 0.25)))))]
    (every? hold (:premises an))))

(defn- undercuts?
  "argument-graph argument-node-id argument-node-id -> boolean
   Returns true if arg1 undercuts arg2.
   Strict arguments cannot be undercut."
  [ag arg1 arg2]
  (let [an1 (get (:argument-nodes ag) arg1),
        an2 (get (:argument-nodes ag) arg2),
        c1 (:atom (get (:statement-nodes ag) (:conclusion an1)))]
    (and (not (:strict an2)) (= c1 `(~'undercut ~arg2)))))

(defn- rebuts?
  "argument-graph argument-node-id argument-node-id -> boolean
   Returns true if arg1 rebuts arg2.
   Strict arguments cannot be rebutted."
  [ag arg1 arg2]
  (let [an1 (get (:argument-nodes ag) arg1),
        an2 (get (:argument-nodes ag) arg2),
        sn  (get (:statement-nodes ag) (:conclusion an1))
        alpha 0.5
        beta 0.3
        gamma 0.2]
    (and (not (:strict an2))
         (= (:conclusion an1) (:conclusion an2))
         (not (= (:pro an1)
                 (:pro an2))) ; one argument is pro and the other con
         (case (:standard sn)
	       :dv true
	       :pe (and (:weight an1) 
			(:weight an2)
			(>= (:weight an1) (:weight an2)))
	       :cce (and (:weight an1)
			 (:weight an2)
			 (>= (:weight an1) (:weight an2))
			 (>= (:weight an1) alpha)
			 (>= (- (:weight an1) (:weight an2))
			    beta))
	       :brd (and (:weight an1)
			 (:weight an2)
			 (>= (:weight an1) (:weight an2))
			 (>= (:weight an1) alpha)
			 (>= (- (:weight an1) (:weight an2)) beta)
			 (<= (:weight an2) gamma))))))

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
                            (:weight sn)
                            (> (:weight sn) 0) 
                            (<= (:weight sn) 0.25))
                       (and (not (:pro an1))
                            (:positive pr)
                            (:weight sn)
                            (< (:weight sn) 1.0)
                            (>= (:weight sn) 0.75))))))
          (:premises an2))))

(defn- attackers
  "argument-graph argument-node-id (seq of argument-id) -> set of argument ids
   Returns the subset of a set of arguments which attack a given argument."
  [ag arg1 args]
  (reduce (fn [s arg2] 
            (if (or (undercuts? ag arg2 arg1)
                    (rebuts? ag arg2 arg1)
                    (undermines? ag arg2 arg1))
              (conj s arg2)
              s))
          #{}
	  args))

(defn argument-graph-to-framework
  "argument-graph -> argumentation-framework
   Constructs a Dung argumentation framework from an argument graph. 
   Only applicable arguments are included in the framework."
  [ag]
  (let [args (map :id (filter (fn [an] (applicable? ag an)) 
                              (vals (:argument-nodes ag)))),
        attacks (reduce (fn [m arg] (assoc m arg (attackers ag arg args)))
			{}
			args)]
    (make-argumentation-framework args attacks)))

(defn- evaluate-facts
  "argument-graph -> argument-graph
   Sets the value of accepted and rejected statements to 1.0 and 0.0 respectively."
  [ag]
  (reduce (fn [ag2 sn]
            (update-statement-node 
              ag2
              sn 
              :value 
              (when (:weight sn) 
                (case (:weight sn)
                  1.0 1.0,
                  0.0 0.0,
                  nil)))) 
          ag
          (vals (:statement-nodes ag))))

(defn- evaluate-argument-graph
  "argument-graph -> argument-graph"
  [ag]
  (let [af (argument-graph-to-framework ag)
         l (grounded-labelling af)]
    (reduce (fn [ag2 arg-id]
	      (let [an (get (:argument-nodes ag2) arg-id)  
		  sn (get (:statement-nodes ag2) (:conclusion an))]
		(-> ag2
		    (update-argument-node an :value 
				         (case (get l arg-id)
						:in 1.0
						:out 0.0
						:else 0.5))
		    (update-statement-node
		     sn
		     :value
		     (if (= :in (get l arg-id))
		       (if (:pro an) 1.0 0.0)
		       (:value sn)))))) ; otherwise no change     
	    (evaluate-facts ag)
	    (keys l))))

(def caes2-evaluator 
     (reify ArgumentEvaluator
    	    (evaluate [this ag] (evaluate-argument-graph (reset-node-values ag)))
    	    (label [this node] (node-standard-label node))))





