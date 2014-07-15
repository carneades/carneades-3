;; Copyright (c) 2012 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "An argument evaluator inspired by Henry Prakken's ASPIC+ system.  It maps
            Carneades argument graphs to Dung argumentation frameworks."}
  carneades.engine.aspic
  (:use clojure.pprint
        ;; clojure.set
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
                     (>= (:weight sn) 0.75)               ; positive premise is assumable
                     (<= (:weight sn) 0.25))              ; negative premise is assumable
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
                                               ;; :assumptions (conj (:assumptions s) goal)
                                               :goals (concat (map premise-literal (:premises an))  ; depth-first
                                                              (rest (:goals s)))))))))
                      []
                      (if (literal-pos? goal) (:pro sn) (:con sn))))))))

(defrecord Position
  [id              ; symbol
   ag              ; argument graph used to construct the position
   root            ; argument node id of the last link, the root of the proof tree
   subargs         ; set of argument node ids, including the last link
   assumptions])   ; map from statement node id to boolean

(defn strict-position? 
  "position -> boolean
   Returns true iff all the subargs of the position are strict."
  [p]
  (every? (fn [arg] 
            (let [an (get (:argument-nodes (:ag p)) arg)]
              (:strict an)))
          (:subargs p)))

(defn position-conclusion
  "position -> statement-node
   Returns the id of the conclusion statement of the last link of the position"
  [p]
  (get (:statement-nodes (:ag p))
       (:conclusion (get (:argument-nodes (:ag p)) (:root p)))))

(defn position-weight
  "position -> real number
   Returns the minimum weight of the subargs of the position, 
   applying the weakest link principle."
  [p]
  (apply min (map (fn [arg] (:weight (get (:argument-nodes (:ag p)) arg)))
                  (:subargs p))))

(defn pro-position?
  "position -> boolean
   Returns true if the last link argument of the position is a pro argument node."
  [p]
  (:pro (get (:argument-nodes (:ag p)) (:root p))))

(defn position-map
  "argument-graph ->  map from argument node ids to a vector of positions"
  [ag]
  (let [argument-node-positions
        (fn [pm an]
          (let [p (struct problem
                          (make-root (State. (map premise-literal (:premises an)) 
                                             ;; goal literals
                                             #{(:id an)} ; argument node ids
                                             #{})) ; assumptions
                          (transitions ag)
                          goal-state?)]
            (assoc pm
              (:id an)
              (map (fn [s]
                     (Position. (gensym "position-")
                                ag
                                (:id an) ; id of the last link argument node
                                (:arguments s)
                                (:assumptions s)))
                   (map :state (search p depth-first))))))]
    (reduce argument-node-positions
            {}
            (vals (:argument-nodes ag)))))

(defn- undercuts?
  "position position -> boolean
   Returns true if position p1 undercuts some subargument of position p2.
   Strict arguments cannot be undercut."
  [p1 p2]
  (let [ag (:ag p1)]
    (some (fn [arg]
            (let [an1 (get (:argument-nodes ag) (:root p1))
                  c1 (:atom (get (:statement-nodes ag) (:conclusion an1)))]
              (and (not (:strict (get (:argument-nodes ag) arg)))
                   (not (:pro an1))
                   (= c1 `(~'valid ~arg)))))
          (:subargs p2))))

;; (defn- rebuts?
;;   "argument-graph position position -> boolean
;;    Returns true if position p1 rebuts position p2.
;;    Only the last link arguments of the positions are compared.
;;    Strict arguments cannot be rebutted."
;;   [ag p1 p2]
;;   (let [an1 (get (:argument-nodes ag) (:argument p1)),
;;         ;; an2 (get (:argument-nodes ag) (:argument p2)),
;;         sn  (get (:statement-nodes ag) (:conclusion an1))
;;         alpha 0.5 ; minimum weight of pro for :cce and :brd
;;         beta 0.3  ; minimum difference between pro and con for :cce
;;         gamma 0.2] ; maximum weight for con for :brd
;;     (some (fn [id]
;;             (let [an2 (get (:argument-nodes ag) id)]
;;               (and (not (:strict an2))
;;                    (= (:conclusion an1) (:conclusion an2))
;;                    (not (= (:pro an1) (:pro an2))) ; one argument is pro and the other con
;;                    (case (:standard sn)
;;                      :dv true
;;                      ;; with :pe the con argument need only be >= the pro arg to defeat it.
;;                      :pe (and (not (nil? (:weight an1)))
;;                               (not (nil? (:weight an2)))
;;                               (>= (:weight an1) (:weight an2)))
;;                      ;; with :cce the con arg defeats the pro arg unless pro's weight
;;                      ;; is >= than alpha the difference between pro and con is >= gamma
;;                      :cce (and (not (nil? (:weight an1)))
;;                                (not (nil? (:weight an2)))
;;                                (>= (:weight an1) (:weight an2))
;;                                (< (:weight an2) alpha)
;;                                (< (- (:weight an2) (:weight an1)) beta))
;;                      :brd (and (not (nil? (:weight an1)))
;;                                (not (nil? (:weight an2)))
;;                                (>= (:weight an1) (:weight an2))
;;                                (< (:weight an2) alpha)
;;                                (< (- (:weight an2) (:weight an1)) beta)
;;                                (>= (:weight an1) gamma))))))
;;           (:subargs p2))))

(defn- rebuts?
  "position position -> boolean
   Returns true if position p1 rebuts position p2.
   Only the last link arguments of the positions are compared.
   Strict arguments cannot be rebutted."
  [p1 p2]
  (let [alpha 0.5 ; minimum weight of pro for :cce and :brd
        beta 0.3  ; minimum difference between pro and con for :cce
        gamma 0.2] ; maximum weight for con for :brd
    (some (fn [id]
            (and (not (strict-position? p2))
                 (= (:id (position-conclusion p1))
                    (:id (position-conclusion p2)))
                 (not= (pro-position? p1) (pro-position? p2)) 
                 ;; one position is pro and the other con
                 (case (:standard (position-conclusion p1))
                   :dv true
                   ;; with :pe the con argument need only be >= the pro arg to defeat it.
                   :pe (>= (position-weight p1) (position-weight p2))
                   ;; with :cce the con arg defeats the pro arg unless pro's weight
                   ;; is >= than alpha the difference between pro and con is >= gamma
                   :cce (and (>= (position-weight p1) (position-weight p2))
                             (< (position-weight p2) alpha)
                             (< (- (position-weight p2) (position-weight p1)) beta))
                   :brd (and (>= (position-weight p1) (position-weight p2))
                             (< (position-weight p2) alpha)
                             (< (- (position-weight p2) (position-weight p1)) beta)
                             (>= (position-weight p1) gamma)))))
          (:subargs p2))))

(defn- undermines?
  "position position -> boolean
   Returns true if position p1 undermines position p2."
  [p1 p2]
  (let [ag (:ag p1)
        an1 (get (:argument-nodes ag) (:root p1))
        sn1  (get (:statement-nodes ag) (:conclusion an1))
        c1 (if (:pro an1) (:id sn1) (literal-complement (:id sn1)))]
    (some (fn [lit]
            (= (literal-complement lit) c1))
          (:assumptions p2))))

(defn- attackers
  "position (seq-of positions) -> set of positions
   Returns the subset of the positions which attack the given position."
  [p1 positions]
  (reduce (fn [s p2]
            (if (or (undercuts? p2 p1)
                    (rebuts? p2 p1)
                    (undermines? p2 p1))
              (conj s p2)
              s))
          #{}
	  positions))

(defn position-map-to-argumentation-framework
  "position-map -> argumentation-framework
   Constructs a Dung argumentation framework from a position map.
   Positions play the role of arguments in the framework."
  [pm]
  (let [positions (flatten (vals pm)),
        args (set (map :id positions)),
        attacks (reduce (fn [m p]
			  (assoc m (:id p)
				 (set (map :id (attackers p positions)))))
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

(defn label-argument-nodes
  [ag pm l]
  (reduce (fn [ag2 an]
            (let [positions (get pm (:id an))

                  arg-value
                  (cond (some (fn [p] (= (get l (:id p)) :in)) positions) 1.0

                        (some (fn [p] (= (get l (:id p)) :undecided))
                              positions)
                        0.5

                        :else 0.0)]
              (update-argument-node ag2 an :value arg-value)))
          ag
          (vals (:argument-nodes ag))))

(defn label-statement-nodes
  [ag]
  (reduce (fn [ag2 stmt]
            (let [conclusion-value
                  (cond (= (:weight stmt) 0.0) 0.0

                        (= (:weight stmt) 1.0) 1.0

                        (and (some in-node? (pro-argument-nodes ag2 stmt))
                             (not-any? in-node? (con-argument-nodes ag2 stmt)))
                        1.0

                        (and (some in-node? (con-argument-nodes ag2 stmt))
                             (not-any? in-node? (pro-argument-nodes ag2 stmt)))
                        0.0

                        :else 0.5)]
              (update-statement-node ag2 stmt :value conclusion-value)))
          (initialize-statement-values ag)
          (vals (:statement-nodes ag))))

(defn evaluate-grounded
  "argument-graph -> argument-graph"
  [ag]
  (let [pm (position-map ag)
        af (position-map-to-argumentation-framework pm)
        l (grounded-labelling af)
        ag (label-argument-nodes ag pm l)
        ag (label-statement-nodes ag)]
    ag))

;; The aspic-grounded evaluator uses grounded semantics
(def aspic-grounded
     (reify ArgumentEvaluator
	    (evaluate [this ag] (evaluate-grounded (reset-node-values ag)))
	    (label [this node] (node-standard-label node))))
