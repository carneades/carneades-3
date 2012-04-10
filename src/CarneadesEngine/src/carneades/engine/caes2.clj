;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns 
    ^{:doc "A version of Carneades Argument Evaluation Structures 
            which can handle cycles, via a mapping to Dung Argumentation Frameworks."}
  
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
   Returns true if all of the premises of the argument are assumed true, accepted
   or supported by a non-cyclic applicable argument."
  ([ag an parents]
    (let [supported? 
          (fn [pr sn] 
            (and (not (contains? parents (:id an))) ; break cycle
                 (some (fn [an2] (applicable? ag an2 (conj parents (:id an))))
                       (map (fn [id] (get (:argument-nodes ag) id))
                            (if (:positive pr) (:pro sn) (:con sn))))))
          
          ok? 
          (fn [pr sn] (and (:weight sn)
                           (if (:positive pr) 
                             (>= (:weight sn) 0.75)
                             (<= (:weight sn) 0.25))))
          
          
          hold 
          (fn [pr]
            (let [sn (get (:statement-nodes ag) (:statement pr))]
              (or (ok? pr sn) 
                  (supported? pr sn))))]
      (every? hold (:premises an))))
  ([ag an] (applicable? ag an #{})))

(defn- undercuts?
  "argument-graph argument-node-id argument-node-id -> boolean
   Returns true if arg1 undercuts arg2.
   Strict arguments cannot be undercut."
  [ag arg1 arg2]
  (let [an1 (get (:argument-nodes ag) arg1),
        an2 (get (:argument-nodes ag) arg2),
        c1 (:atom (get (:statement-nodes ag) (:conclusion an1)))]
    (and (not (:strict an2))
         (:pro an1)
         (= c1 `(~'undercut ~arg2)))))

(defn- rebuts?
  "argument-graph argument-node-id argument-node-id -> boolean
   Returns true if arg1 rebuts arg2.
   Strict arguments cannot be rebutted."
  [ag arg1 arg2]
  (let [an1 (get (:argument-nodes ag) arg1),
        an2 (get (:argument-nodes ag) arg2),
        sn  (get (:statement-nodes ag) (:conclusion an1))
        alpha 0.5 ; minimum weight of pro for :cce and :brd
        beta 0.3  ; minimum difference between pro and con for :cce
        gamma 0.2] ; maximum weight for con for :brd
    (and (not (:strict an2))
         (= (:conclusion an1) (:conclusion an2))
         (not (= (:pro an1)
                 (:pro an2))) ; one argument is pro and the other con
         (case (:standard sn)
	       :dv true
               ;; with :pe the con argument need only be >= the pro arg to defeat it.
               ;; the pro argument is also defeated if either arg has no weight
	       :pe (or (nil? (:weight an1)) 
		       (nil? (:weight an2))
		       (>= (:weight an1) (:weight an2)))
               ;; with :cce the con arg defeats the pro arg unless pro's weight
               ;; is >= than alpha the difference between pro and con is >= gamma
	       :cce (or (nil? (:weight an1))
			(nil? (:weight an2))
			(>= (:weight an1) (:weight an2))
			(< (:weight an2) alpha)
			(< (- (:weight an2) (:weight an1)) beta))
	       :brd (or (nil? (:weight an1))
			(nil? (:weight an2))
			(>= (:weight an1) (:weight an2))
			(< (:weight an2) alpha)
			(< (- (:weight an2) (:weight an1)) beta)
			(>= (:weight an1) gamma))))))

(defn- undermines?
  "argument-graph argument-node-id argument-node-id -> boolean
   Returns true if arg1 undermines arg2."
  [ag arg1 arg2]
  (let [an1 (get (:argument-nodes ag) arg1)
        an2 (get (:argument-nodes ag) arg2)
        accepted? (fn [sn] (and (:weight sn) (= (:weight sn) 1.0)))
        rejected? (fn [sn] (and (:weight sn) (= (:weight sn) 0.0)))]
    (some (fn [pr]
            (let [sn (get (:statement-nodes ag) (:statement pr))]
              ;; an1 is pro a negative premise or con a positive premise
              ;; unless the premise has been accepted
              (and (= (:conclusion an1) (:statement pr))
                   (or (and (:pro an1)
                            (not (:positive pr))
                            (not (rejected? sn)))
                       (and (not (:pro an1)) 
                            (:positive pr)
                            (not (accepted? sn)))))))
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

(defn- initialize-statement-values
  "argument-graph -> argument-graph
   Sets the initial value of statements. These values
   can be overridden when the arguments are evaluated."
  [ag]
  (reduce (fn [ag2 sn]
            (update-statement-node 
              	     ag2
              	     sn 
              	     :value 
              	     (when (:weight sn) 
                      	    (cond 
                             	      (>= (:weight sn) 0.75) 1.0
                             	      (<= (:weight sn) 0.25) 0.0
                             	     :else nil)))) 
          ag
          (vals (:statement-nodes ag))))

(defn- evaluate-grounded
  "argument-graph -> argument-graph"
  [ag]
  (let [af (argument-graph-to-framework ag)
        l (grounded-labelling af)]
    (reduce (fn [ag2 arg-id]
	      (let [an (get (:argument-nodes ag2) arg-id)  
		  sn (get (:statement-nodes ag2) (:conclusion an))
                       update-conclusion (fn [ag sn] 
                                           ; don't change value of facts
                                           (cond (= (:weight sn) 0.0)  ag
                                                 (= (:weight sn) 1.0)  ag
                                                 (= :in (get l arg-id)) 
                                                     (update-statement-node ag sn 
                                                         :value (if (:pro an) 1.0 0.0))
                                                 :else ag))]
		(-> ag2
		    (update-argument-node an 
                            :value (case (get l arg-id)
			       :in 1.0
			       :out 0.0
			       :else 0.5))
		    (update-conclusion sn))))	         
	    (initialize-statement-values ag)
	    (keys l))))

;; The caes-grounded evaluator uses grounded semantics
(def caes-grounded 
     (reify ArgumentEvaluator
    	    (evaluate [this ag] (evaluate-grounded (reset-node-values ag)))
    	    (label [this node] (node-standard-label node))))





