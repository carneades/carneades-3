;;; Copyright ? 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Search arguments in a search space."}
  carneades.engine.argument-search
  (:import (java.io PrintWriter) ; for debugging
    )
  (:use clojure.contrib.def
        clojure.set
        carneades.engine.utils
        carneades.engine.statement
        carneades.engine.unify
        clojure.contrib.pprint
        carneades.ui.diagram.viewer ; for debugging
        ;clojure.contrib.profile ; for testing
    )
  (:require [carneades.engine.argument :as arg]
            [carneades.engine.search :as search]))


(defstruct ^{:doc "Struct.

  A candidate argument is an argument which might have uninstantiated
  variables. The candidate argument is asserted into an argument graph
  of a state only when the guard is ground in the substitution environment
  of the state, by substituting all the variables in the argument with
  their values in the substitution environment.  If the guard is correct,
  the argument put forward will contain only ground statements,
  i.e. with no variables.

  :guard is a statement containing all the variables used by the argument
  :argument is the argument"}
  candidate
  :guard
  :argument)

(defstruct- ^{:doc "Struct.

  :topic      statement for the main issue or thesis
  :viewpoint  pro | con the topic
  :pro-goals  (seq-of (seq-of statement)) ; disjunctive normal form
  :con-goals  (seq-of (seq-of statement)) ; disjunctive normal form
  :arguments  argument-graph 
  :substitutions  substitution
  :candidates     (seq-of candidate)
  :candidate-assumptions (seq-of statement)"}
  state-struct
  :topic     ;; statement for the main issue or thesis
  :viewpoint ;; pro | con the topic
  :pro-goals ;; (seq-of (seq-of statement)) ; disjunctive normal form
  :con-goals ;; (seq-of (seq-of statement)) ; disjunctive normal form
  :arguments ;; argument-graph 
  :substitutions ;; substitution
  :candidates    ;; (seq-of candidate)
  :candidate-assumptions ;; (set-of statement)
  )

(defn state [topic viewpoint pro-goals con-goals
             arguments substitutions candidates]
  {:pre [(not (nil? pro-goals))
         (not (nil? con-goals))]}
  (struct state-struct topic viewpoint pro-goals con-goals
          arguments substitutions candidates #{}))

(defstruct- response-struct
  :substitutions ;; term -> term
  :assumptions ;; set of literals
  :argument ;; argument | nil
  )

(defn response [subs assumptions arg]
  {:pre [(not (nil? subs))
         (set? assumptions)]}
  (struct response-struct subs assumptions arg))

(defn initial-state
  "statement argument-graph -> state"
  [topic ag]
  (state topic :pro (list (list topic)) '() ag *identity* '()))

(defn- next-goals
  "state -> (seq-of (seq-of statement)) 

   Returns a list representing a disjunction of a conjunction of 
   goal statements for the current viewpoint to try to solve in the state
   If no goals remain for the current viewpoint, the empty list is returned."
  [state]
  (condp = (:viewpoint state)
      :pro (:pro-goals state)
      :con (:con-goals state)))

(defn questioned-assumptions
  "(seq-of statement) argument-graph -> (seq-of statement)"
  [assumptions ag]
  (filter (partial arg/questioned? ag) assumptions))

(defn- apply-candidates
  "substitutions argument-graph (list-of candidate)
   -> [ag candidate]"
  [subs ag candidates]
  {:pre [(not (nil? subs))
         (not (nil? ag))]}
   (reduce
    (fn [acc c]
      (let [[ag2 candidates2] acc]
        (if (ground? (apply-substitution subs (:guard c)))
          (let [arg (arg/instantiate-argument (:argument c)  subs)]
            [(arg/assert-argument ag2 arg)
              candidates2])
          [ag2 (cons c candidates2)])))
    [ag nil]
    candidates))

(defn- update-goals
  "(seq-of (seq-of statement)) (seq-of statement)  ->
   (seq-of (seq-of statement)) 
   Replaces the first literal of the first clause in a list of clauses
   with the given replacement statements. If the resulting clause is empty
   return the remaining clauses, otherwise return the result of
   replacing the first clause with the updated clause."
  ([dis-of-con replacements]
    (let [clause (concat replacements (rest (first dis-of-con)))]
      (if (empty? clause)
        (rest dis-of-con)
        (cons clause (rest dis-of-con)))))
  ([dis-of-con]
    (let [clause (rest (first dis-of-con))]
      (if (empty? clause)
        (rest dis-of-con)
        (cons clause (rest dis-of-con))))))

(defn- get-pro-goals  
  [state arg assumptions]
  {:post [(not (nil? %))]}
  (if arg
    (condp = (:viewpoint state)
      :pro (update-goals (:pro-goals state) 
                         (map arg/premise-statement (:premises arg)))
      :con  (concat (:pro-goals state)           
                    (list (list (statement-complement (:conclusion arg))))
                    (map list (map statement-complement assumptions))))
    (condp = (:viewpoint state)
      :pro (update-goals (:pro-goals state))
      :con  (concat (:pro-goals state)           
                    (map list (map statement-complement assumptions))))))
    
(defn- get-con-goals 
  [state arg assumptions]
  {:post [(not (nil? %))]}
  (if arg
    (condp = (:viewpoint state)
      :pro (concat (:con-goals state)
                   (list (list (statement-complement (:conclusion arg))))
                   (map list (map statement-complement assumptions)))
      :con (update-goals (:con-goals state) 
                         (map arg/premise-statement (:premises arg))))
    (condp = (:viewpoint state)
      :pro (concat (:con-goals state)
                   (map list (map statement-complement assumptions)))
      :con (update-goals (:con-goals state)))))

(defn- update-assumption 
  "state statement -> state"
  [state a1]
  ; (println "a1 of update assumption: " a1)
  (let [ag (:arguments state)
        subs (:substitutions state)
        cas (:candidate-assumptions state)
        a2 (apply-substitution subs a1) ]
    ; (println "a2 of update assumption: " a2)
    (if (not (ground? a2))
      state
      (let [ag2 (condp = (arg/status ag a2)
                  :stated (arg/accept ag [a2])
                  :questioned ag
                  :rejected (arg/question ag [a2])
                  :accepted ag)]
        (assoc state :arguments ag2
               :candidate-assumptions (difference cas #{a1}))))))
                 
(defn- update-assumptions
  [state]
  ; (println "update assumptions: " (:candidate-assumptions state))
  (reduce update-assumption state (:candidate-assumptions state)))

(defn- make-successor-state
  "state response -> state"
  [state response]
  {:post [(not (nil? %))]}
  (let [subs2 (:substitutions response)
        assumptions (:assumptions response)
        arg (:argument response)
        premises (and arg (map arg/premise-statement (arg/argument-premises arg)))
        [ag2 candidates2] (apply-candidates
                            subs2
                            (:arguments state)
                            (if (not arg)
                              (:candidates state)
                              (cons (struct candidate
                                            `(~'guard ~@(arg/argument-variables arg)) arg)
                                    (:candidates state))))]
    (update-assumptions 
      (assoc state
             :pro-goals (get-pro-goals state arg assumptions)
             :con-goals (get-con-goals state arg assumptions)
             :arguments ag2
             :substitutions subs2
             :candidates candidates2
             :candidate-assumptions (union assumptions (:candidate-assumptions state))))))

(defn- apply-generator [generator node]
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
 
;; Uses mapinterleave to interleave the application of the argument generators.
(defn- make-transitions
  "(seq-of generator) -> (node -> (seq-of node))"
  [l]
  (fn [node]
    (mapinterleave (fn [g]  (apply-generator g node)) l)))

(defn- goal-state? [state]
  {:pre [(not (nil? state))]}
  (let [in (arg/in? (:arguments state)
                    (apply-substitution (:substitutions state) (:topic state)))]
    (condp = (:viewpoint state)
        :pro in
        :con (not in))))

(defn switch-viewpoint
  "state -> state"
  [s]
  (letfn [(opposing-viewpoint
           [vp]
           (condp = vp
               :pro :con
               :con :pro))]
    (update-in s [:viewpoint] opposing-viewpoint)))

(defn find-arguments
  "strategy state (seq-of generator) -> (seq-of state)"
  [strategy max-nodes initial-state generators]
  (map :state (search/search (struct search/problem
                              (search/make-root initial-state)
                              (make-transitions generators)
                              goal-state?)
                      strategy
                      max-nodes)))

(defn find-best-arguments
  "strategy int int state (seq-of generator) -> (seq-of state)
   Find the best arguments for *both* viewpoints, starting with the viewpoint of
   the initial state. An argument is 'best' if it survives all possible attacks
   from arguments which can be constructed using the provided argument
   generators, within the given search limits.  find-best-arguments allows
   some negative conclusions to be explained, since it includes successful
   counterarguments in its resulting stream of states."
  [strategy max-nodes max-turns state1 generators]
  (if (<= max-turns 0)
    []
    (mapinterleave 
      (fn [state2]
        (let [seq1 (find-arguments
                     strategy
                     max-nodes
                     (switch-viewpoint state2)
                     generators)]
          (if (empty? seq1)
            (list state2)
            (flatten (map (fn [state3]
                            (find-best-arguments
                              strategy
                              max-nodes
                              (dec max-turns)
                              state3
                              generators))
                          seq1)))))
      (find-arguments strategy max-nodes state1 generators))))

(defn construct-arguments
  "state integer integer (seq-of generator) -> state
   Construct arguments for both viewpoints and combine the arguments into
   a single argument graph of a state.  All arguments found within the given
   resource limits are included in the argument graph of the resulting state,
   regardless of whether or not the goal of the input state was achieved."
  [state1 max-nodes max-turns generators]
  ; (println "construct-arguments: " (arg/arguments (:arguments state1)) "\n\n")
  (if (<= max-turns 0)
    state1 
    (let [seq1 (map :state 
                    (search/traverse 
                      (struct search/problem
                              (search/make-root state1)
                              (make-transitions generators)
                              (fn [s] true))
                      search/depth-first
                      max-nodes))
          ag2 (arg/unite-argument-graphs (map :arguments (cons state1 seq1)))]
      (construct-arguments (switch-viewpoint (assoc state1 :arguments ag2))
                           max-nodes (dec max-turns) generators))))

