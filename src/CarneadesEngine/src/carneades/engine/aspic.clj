;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns 
    ^{:doc "An argument evaluator inspired by Henry Prakken's ASPIC+ system.  It maps
            Carneades argument graphs to Dung argumentation frameworks."}
  
  carneades.engine.aspic
  
  (:use clojure.pprint
        clojure.set
        carneades.engine.statement
        carneades.engine.argument
        carneades.engine.dung
        carneades.engine.argument-graph
        carneades.engine.argument-evaluation
        carneades.engine.search))


(defrecord State
  [goals           ; list of literals, id or '(not id), where the id is a statement node id
   arguments       ; set of argument node ids
   assumptions])   ; set of literals

(defn- goal-state?
  "state -> boolean"
  [s]
  {:pre [(instance? State s)]}
  (empty? (:goals s)))

(defn- transitions
  "argument-graph -> node -> seq of node"
  [ag]
  (fn [n]
    (if (goal-state? (:state n))
      []
      (let [s (:state n)
            goal (first (:goals s))
            sn (get (:statement-nodes ag) (literal-atom goal))
            pop-goal (fn [] (assoc s :goals (rest (:goals s))))
            make-node (fn [s] (struct node (inc (:depth n)) nil n s))]                     

        (cond (and (:weight sn)
                   (if (literal-pos? goal)
                     (= (:weight sn) 1.0)   ; positive premise is accepted
                     (= (:weight sn) 0.0))) ; negative premise is accepted
              [(make-node (pop-goal))],

              (and (:weight sn)
                   (if (literal-pos? goal)
                     (>= (:weight sn) 0.75)    ; positive premise is assumable
                     (<= (:weight sn) 0.25))  ; negative premise is assumable
                   (not (contains? (:assumptions s) (literal-complement goal))))
              [(make-node (assoc (pop-goal) :assumptions (conj (:assumptions s) goal)))]

              :else ; apply the arguments pro the goal
              (reduce (fn [v arg]
                        (if (contains? (:arguments s) arg) ; support cycle
                          v
                          (let [an (get (:argument-nodes ag) arg)]
                            (conj v
                                  (make-node (assoc s
                                               :arguments (conj (:arguments s) arg)
                                               :goals (concat (map premise-literal (:premises an))  ; depth-first
                                                              (rest (:goals s)))))))))
                      []
                      (if (literal-pos? goal) (:pro sn) (:con sn))))))))              

(defrecord Position
  [id              ; symbol
   argument        ; argument node id of the last link
   subargs         ; set of argument node ids, including the last link
   assumptions])   ; map from statement node id to boolean

(defn position-map
  "argument-graph ->  map from argument node ids to a vector of positions"
  [ag]
  (let [argument-node-positions
        (fn [pm an]
          (let [p (struct problem 
                          (make-root (State. (map premise-literal (:premises an)) ; goal literals
                                             #{(:id an)}                          ; argument node ids 
                                             #{}))                                 ; assumptions
                          (transitions ag)
                          goal-state?)]
            (assoc pm
              (:id an)
              (map (fn [s] 
                     (Position. (gensym "position-")
                                (:id an) ; id of the last link argument node
                                (:arguments s)
                                (:assumptions s)))
                   (map :state (search p depth-first))))))]                                        
    (reduce argument-node-positions
            {}
            (vals (:argument-nodes ag)))))

(defn- undercuts?
  "argument-graph position position -> boolean
   Returns true if position p1 undercuts some subargument of position p2.
   Strict arguments cannot be undercut."
  [ag p1 p2]
  (some (fn [arg]
          (let [an1 (get (:argument-nodes ag) (:argument p1))
                c1 (:atom (get (:statement-nodes ag) (:conclusion an1)))]
            (and (not (:strict (get (:argument-nodes ag) arg)))
                 (:pro an1)
                 (= c1 `(~'undercut ~arg)))))
        (:subargs p2)))

(defn- rebuts?
  "argument-graph position position -> boolean
   Returns true if position p1 rebuts position p2.
   Only the last link arguments of the positions are compared.
   Strict arguments cannot be rebutted."
  [ag p1 p2]
  (let [an1 (get (:argument-nodes ag) (:argument p1)),
        an2 (get (:argument-nodes ag) (:argument p2)),
        sn  (get (:statement-nodes ag) (:conclusion an1))
        alpha 0.5 ; minimum weight of pro for :cce and :brd
        beta 0.3  ; minimum difference between pro and con for :cce
        gamma 0.2] ; maximum weight for con for :brd
    (and (not (:strict an2))
         (= (:conclusion an1) (:conclusion an2))
         (not (= (:pro an1) (:pro an2))) ; one argument is pro and the other con
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
  "argument-graph position position -> boolean
   Returns true if position p1 undermines position p2."
  [ag p1 p2]
  (let [an1 (get (:argument-nodes ag) (:argument p1))
        sn1  (get (:statement-nodes ag) (:conclusion an1))
        c1 (if (:pro an1) (:id sn1) (literal-complement (:id sn1)))]
    (some (fn [lit] 
            (= (literal-complement lit) c1))
          (:assumptions p2))))

(defn- attackers
  "argument-graph position (seq-of positions) -> set of positions
   Returns the subset of the positions which attack the given position."
  [ag p1 positions]
  (reduce (fn [s p2] 
            (if (or (undercuts? ag p2 p1)
                    (rebuts? ag p2 p1)
                    (undermines? ag p2 p1))
              (conj s p2)
              s))
          #{}
	  positions))

(defn position-map-to-argumentation-framework
  "position-map argument-graph -> argumentation-framework
   Constructs a Dung argumentation framework from a position map.
   Positions play the role of arguments in the framework."
  [pm ag]
  (let [positions (flatten (vals pm)),
        args (set (map :id positions)),
        attacks (reduce (fn [m p] 
			  (assoc m (:id p)
				 (set (map :id (attackers ag p positions)))))
			{}
			positions)]
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
	     (cond (and (:weight sn) (>= (:weight sn) 0.75)) 1.0
		   (and (:weight sn) (<= (:weight sn) 0.25)) 0.0
		   :else 0.5))) 
          ag
          (vals (:statement-nodes ag))))

(defn evaluate-grounded
  "argument-graph -> argument-graph"
  [ag]
  (let [pm (position-map ag)
        af (position-map-to-argumentation-framework pm ag)
        l (grounded-labelling af)]
    (reduce (fn [ag2 an]
	      (let [c (get (:statement-nodes ag2) (:conclusion an))
                    positions (get pm (:id an))

                    arg-value 
		    (cond (some (fn [p] (= (get l (:id p)) :in)) positions) 1.0
			  (some (fn [p] (= (get l (:id p)) :undecided)) 
				positions) 0.5
                          :else 0.0)
                    
                    conclusion-value  
                    (cond (= (:weight c) 0.0) (:value c) 
			  ;; arguments don't affect value of facts
                          (= (:weight c) 1.0) (:value c)

                          (and (= arg-value 1.0)  (:pro an)) 
                          1.0

                          (and (= arg-value 1.0) (not (:pro an))) 
                          0.0

                          ;; otherwise don't change the value
                          :else (:value c))]
                (-> ag2
                    (update-argument-node an :value arg-value)
                    (update-statement-node c :value conclusion-value))))	         
            (initialize-statement-values ag)
            (vals (:argument-nodes ag)))))

;; The aspic-grounded evaluator uses grounded semantics
(def aspic-grounded 
     (reify ArgumentEvaluator
	    (evaluate [this ag] (evaluate-grounded (reset-node-values ag)))
	    (label [this node] (node-standard-label node))))





