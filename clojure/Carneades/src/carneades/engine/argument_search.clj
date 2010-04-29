(ns carneades.engine.argument-search
  (:use clojure.contrib.def
        carneades.engine.utils
        carneades.engine.statement
        clojure.contrib.pprint)
  (:require [carneades.engine.argument :as arg]
            [carneades.engine.search :as search]))

(set! *assert* true)

(defstruct candidate
  ;; A candidate argument is an argument which might have uninstantiated
  ;; variables. The candidate argument is asserted into an argument graph
  ;; of a state only when the guard is ground in the substitution environment
  ;; of the state, by substituting all the variables in the argument with
  ;; their values in the substitution environment.  If the guard is correct,
  ;; the argument put forward will contain only ground statements,
  ;; i.e. with no variables.
  
  :guard ;; statement containing all the variables used by the argument
  :argument)

(defstruct- state-struct
  :topic     ;; statement for the main issue or thesis
  :viewpoint ;; pro | con the topic
  :pro-goals ;; (seq-of (seq-of statement)) ; disjunctive normal form
  :con-goals ;; (seq-of (seq-of statement)) ; disjunctive normal form
  :arguments ;; argument-graph 
  :substitutions ;; substitution
  :candidates    ;; (seq-of candidate)
  )

(defn state [topic viewpoint pro-goals con-goals
             arguments substitutions candidates]
  {:pre [(not (nil? pro-goals))
         (not (nil? con-goals))]}
  (struct state-struct topic viewpoint pro-goals con-goals
          arguments substitutions candidates))

(defstruct response
  :substitutions ;; term -> term
  :argument ;; argument | nil
  )

(defn initial-state [topic ag]
  "statement argument-graph -> state"
  (state topic :pro (list (list topic)) '() ag identity '()))

(defn next-goals [state]
  "state -> (seq-of (seq-of statement)) 

   Returns a list representing a disjunction of a conjunction of 
   goal statements for the current viewpoint to try to solve in the state
   If no goals remain for the current viewpoint, the empty list is returned."
  (condp = (:viewpoint state)
      :pro (:pro-goals state)
      :con (:con-goals state)
      (throw (Exception. "Invalid value"))))

(defn questioned-assumptions [assumptions ag]
  "(seq-of statement) argument-graph -> (seq-of statement)"
  (filter (partial arg/questioned? ag) assumptions))

(defn apply-candidates [subs ag candidates]
  {:pre [(not (nil? ag))]}
  "substitutions argument-graph (list-of candidate)
   -> [ag candidate]"
  (doall
   (reduce
    (fn [acc c]
      (let [[ag2 candidates2] acc]
        (if (ground? (subs (:guard c)))
          (let [arg (arg/instantiate-argument (:argument c)  subs)]
            [(arg/assert-argument (arg/question
                                   ag2
                                   (list (arg/argument-conclusion arg)))
                                  arg)
              candidates2])
          [ag2 (cons c candidates2)])))
    [ag '()]
    candidates)))

(defn update-goals [dis-of-con replacements]
  "(seq-of (seq-of statement)) (seq-of statement)  ->
              (seq-of (seq-of statement))

   replaces the first literal of the first clause in a list of clauses
   with the given replacement statements. If the resulting clause is empty
   return the remaining clauses, otherwise return the result of
   replacing the first clause with the updated clause."
  (let [clause (concat replacements (rest (first dis-of-con)))]
    (if (empty? clause)
      (rest dis-of-con)
      (cons clause (rest dis-of-con)))))

(defn- get-pro-goals [state premises assumptions exceptions conclusion]
  {:post [(not (nil? %))]}
  (condp = (:viewpoint state)
      :pro (update-goals (:pro-goals state) premises)
      :con  (concat (:pro-goals state)
                    (map list exceptions) ;; separate clause for each exception
                    ;; rebutalls and rebuttals of assumptions
                    (list (list (statement-complement conclusion)))
                    (map list (map statement-complement assumptions)))))

(defn- get-con-goals [state premises assumptions exceptions conclusion]
  {:post [(not (nil? %))]}
  (condp = (:viewpoint state)
      :pro (concat (:con-goals state)
                   ;; separate clause for each exception
                   (map list exceptions)
                   (list (list (statement-complement conclusion)))
                   (map list (map statement-complement assumptions)))
      :con (update-goals (:con-goals state) premises)))

(defn- make-successor-state-putforward [stat newsubs arg]
  (let [conclusion (:conclusion arg)
        assumptions (questioned-assumptions
                     (map arg/premise-statement
                          (filter arg/assumption?
                                  (arg/argument-premises arg)))
                     (:arguments state))
        premises (concat (map arg/premise-statement
                              (filter arg/ordinary-premise?
                                      (arg/argument-premises arg)))
                         assumptions)
        exceptions (map arg/premise-statement
                        (filter arg/exception? (arg/argument-premises arg)))
        [newag newcandidates]
        (apply-candidates
         newsubs
         (:arguments stat)
         (cons (struct candidate
                       `(~'guard ~@(arg/argument-variables arg)) arg)
               (:candidates stat)))]

    (state (:topic stat)
           (:viewpoint stat)
           (get-pro-goals stat premises assumptions exceptions conclusion)
           (get-con-goals stat premises assumptions exceptions conclusion)
           ;; new argument graph
           newag
           ;; new subs
           newsubs
           ;; new candidates
           newcandidates)))

(defn- make-successor-state-noforward [stat newsubs]
  "create a successor state by modifying the substitutions of the state, 
   but without putting forward a new argument."
  (let [[newag newcandidates]
        (apply-candidates newsubs (:arguments stat) (:candidates stat))]
    (state
     (:topic stat)
     (:viewpoint stat)
     ;; new pro goals
     (condp = (:viewpoint stat)
         :pro (update-goals (:pro-goals stat) '())
         :con (:pro-goals stat))
     ;; new con goals
     (condp = (:viewpoint stat)
         :pro (:con-goals stat)
         :con (update-goals (:con-goals stat) '()))
     ;; new argument graphs
     newag
     ;; new substitutions
     newsubs
     ;; new candidates
     newcandidates)))

(defn make-successor-state [state response]
  {:post [(not (nil? %))]}
  "state response -> state"
  (let [newsubs (:substitutions response)]
    (if-let [arg (:argument response)]
      (make-successor-state-putforward state newsubs arg)
      (make-successor-state-noforward state newsubs))))

(defn apply-generator [generator node]
  (let [state1 (:state node)]
    (map (fn [state2]
           (struct search/node (inc (:depth node))
                   nil  ;; no node label 
                   node ;; parent node
                   state2))
         (map (fn [response]
                (make-successor-state (:state node) response))
              (mapinterleave (fn [clause]
                               (let [goal (first clause)]
                                 (generator goal state1)))
                             (next-goals state1))))))

;; type generator : statement state -> (stream-of response)
;; A generator maps a goal statement to a stream of arguments pro this
;; goal. The conclusion of each argument will be a positive statement equal to
;; the atom of the goal statement.
;; If the goal statement is negative, the arguments generated will be con the
;; complement of the goal statement. If the statement is #f
;; the generator should return an empty stream.
;; 
;; Uses mapinterleave to interleave the application of the argument generators.
(defn make-transitions [l]
  "(seq-of generator) -> (node -> (seq-of node))"
  (fn [node]
    (mapinterleave (fn [g]  (apply-generator g node)) l)))

(defn- goal-state? [state]
  {:pre [(not (nil? state))]}
  (let [in (arg/in? (:arguments state)
                    ((:substitutions state) (:topic state)))]
    (condp = (:viewpoint state)
        :pro in
        :con (not in))))

(defn find-arguments [strategy initial-state generators]
  "strategy state (seq-of generator) -> (seq-of state)"
  (let [root (search/make-root initial-state)
        r (search/search (struct search/problem
                                 root
                                 (make-transitions generators)
                                 goal-state?)
                         strategy)]
    (doall (map :state r))))

(defn switch-viewpoint [s]
  "state -> state"
  (letfn [(opposing-viewpoint
           [vp]
           (condp = vp
               :pro :con
               :con :pro))]
    (update-in s [:viewpoint] opposing-viewpoint)))

(defn searcharg [turns arguments strategy generators]
  (if (<= turns 0)
    arguments
    (mapinterleave (fn [state2]
                     (let [arg2 (find-arguments strategy
                                                (switch-viewpoint state2)
                                                generators)]
                       (if (empty? arg2)
                         (list state2)
                         (searcharg (dec turns) arg2 strategy generators))))
                   arguments)))

(defn find-best-arguments [strategy max-turns state1 generators]
  "strategy int state (seq-of generator) -> (seq-of state)
  
  find the best arguments for *both* viewpoints, starting with the viewpoint of
  the initial state. An argument is \"best\" if it survives all possible attacks
  from arguments which can be constructed using the provided argument
  generators, within the given search limits.  find-best-arguments allows
  negative conclusions to be explained, since it includes successful
  counterarguments in its resulting stream of arguments."
  (if (neg? max-turns)
    '()
    (searcharg (dec max-turns) (find-arguments strategy state1 generators)
               strategy generators)))

