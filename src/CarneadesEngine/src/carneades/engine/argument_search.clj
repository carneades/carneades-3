;;; Copyright © 2010 Fraunhofer Gesellschaft 
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
          arguments substitutions candidates {}))

(defstruct- response-struct
  :substitutions ;; term -> term
  :assumptions ;; set of statements
  :argument ;; argument | nil
  )

(defn response [subs assumptions arg]
  {:pre [(not (nil? subs))]}
  (struct response-struct subs assumptions arg))

(defn initial-state
  "statement argument-graph -> state"
  [topic ag]
  (state topic :pro (list (list topic)) '() ag *identity* '()))

(defn next-goals
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

(defn apply-candidates
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
            [(arg/assert-argument (arg/question
                                   ag2
                                   (list (arg/argument-conclusion arg)))
                                  arg)
              candidates2])
          [ag2 (cons c candidates2)])))
    [ag '()]
    candidates))

(defn update-goals
  "(seq-of (seq-of statement)) (seq-of statement)  ->
              (seq-of (seq-of statement))

   replaces the first literal of the first clause in a list of clauses
   with the given replacement statements. If the resulting clause is empty
   return the remaining clauses, otherwise return the result of
   replacing the first clause with the updated clause."
  [dis-of-con replacements]
  (let [clause (concat replacements (rest (first dis-of-con)))]
    (if (empty? clause)
      (rest dis-of-con)
      (cons clause (rest dis-of-con)))))

(defn- get-pro-goals [state premises conclusion goal-proc]
  {:post [(not (nil? %))]}
  (condp = (:viewpoint state)
    :pro (if (= goal-proc :update)
           (update-goals (:pro-goals state) premises)
           (:pro-goals state)),
    :con  (concat (:pro-goals state)
                  (list (list (statement-complement conclusion))))))

(defn- get-con-goals [state premises conclusion goal-proc]
  {:post [(not (nil? %))]}
  (condp = (:viewpoint state)
    :pro (concat (:con-goals state)
                 (list (list (statement-complement conclusion))))
    :con (if (= goal-proc :update)
           (update-goals (:con-goals state) premises)
           (:con-goals state))))

(defn- update-assumption 
  "state statement -> state"
  [state assumption]
  (let [ag (:argument state)
        subs (:substitutions state)
        cas (:candidate-assumptions)]
    (if (ground? (apply-substitution subs assumption))
      (let [ag2 (condp = (arg/status ag assumption)
                  :stated (arg/accept ag [assumption])
                  :questioned ag
                  :rejected (arg/question ag [assumption])
                  :accepted ag)]
         (assoc state :arguments ag2
                      :candidate-assumptions (difference cas {assumption})))
      (assoc state :candidate-assumptions (union cas {assumption})))))
                
(defn- update-assumptions
  [state assumptions]
  (reduce update-assumption state (union assumptions (:candidate-assumptions state))))

(defn- make-successor-state-putforward 
  ([stat newsubs arg]
    (make-successor-state-putforward stat newsubs arg :update))
  ([stat newsubs arg goal-proc]
    (let [conclusion (:conclusion arg) ; maybe use complement regarding argument-dirction?
          premises (map arg/premise-statement (arg/argument-premises arg))
          [newag newcandidates]
          (apply-candidates
            newsubs
            (:arguments stat)
            (cons (struct candidate
                          `(~'guard ~@(arg/argument-variables arg)) arg)
                  (:candidates stat)))
          pro-goals (get-pro-goals stat premises conclusion goal-proc)
          con-goals (get-con-goals stat premises conclusion goal-proc)
          new-state (state 
                      (:topic stat)
                      (:viewpoint stat)
                      pro-goals
                      con-goals
                      ;; new argument graph
                      newag
                      ;; new subs
                      newsubs
                      ;; new candidates
                      newcandidates)]
      ;            (println "-------------")
      ;            ;(println "state" stat)
      ;            (println "argument" (:id arg))
      ;            (println "conclusion" conclusion)
      ;            (println "gesubt" (newsubs conclusion))
      ;            (println "premises" premises)
      ;            (println "old pro-goals" (:pro-goals stat))
      ;            (println "new pro-goals" pro-goals)
      ;            (println "old con-goals" (:con-goals stat))
      ;            (println "new con-goals" con-goals)
      ;            (println "goal process" goal-proc)
      ;        (view (let [cand-args (map :argument (:candidates new-state)),
      ;                    inst-cands (map (fn [a] (arg/instantiate-argument a newsubs)) cand-args),
      ;                    agwc (arg/assert-arguments newag inst-cands)]
      ;                agwc))
      ;        (println "-------------")
      new-state
      )))


(defn- make-successor-state-noforward
  "Creates a successor state by modifying the substitutions of the state, 
   but without putting forward a new argument."
  [stat newsubs]
  {:pre [(not (nil? newsubs))]}
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

(defn make-successor-state
  "state response -> state"
  [state response]
  {:post [(not (nil? %))]}
  (let [newsubs (:substitutions response)
        state2  (if-let [arg (:argument response)]
                  (if (seq? arg)
                    (let []
                      ;(println "reducing" (count arg) "states")
                      (reduce (fn [s a] (make-successor-state-putforward s newsubs a :none)) 
                              (make-successor-state-putforward state newsubs (first arg) :update) (rest arg)))
                    (make-successor-state-putforward state newsubs arg))
                  (make-successor-state-noforward state newsubs))]
    (update-assumptions state2 (:assumptions response))))

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
(defn make-transitions
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

(defn find-arguments
  "strategy state (seq-of generator) -> (seq-of state)"
  [strategy initial-state generators]
  (let [root (search/make-root initial-state)
        r (search/search (struct search/problem
                                 root
                                 (make-transitions generators)
                                 goal-state?)
                         strategy)]
    (doall (map :state r))))

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
  [type strategy max-nodes initial-state generators]
  (let [root (search/make-root initial-state)
        r (type (struct search/problem
                                 root
                                 (make-transitions generators)
                                 goal-state?)
                         strategy
                         max-nodes)]
    (map :state r)))


(defn searcharg [type strategy max-nodes turns arguments generators]
  (if (<= turns 0)
    arguments
    (mapinterleave (fn [state2]
                     (let [arg2 (find-arguments
                                 type
                                 strategy
                                 max-nodes
                                 (switch-viewpoint state2)
                                 generators)]
                       (if (empty? arg2)
                         (list state2)
                         (searcharg type
                                    strategy
                                    max-nodes
                                    (dec turns)
                                    arg2
                                    generators))))
                   arguments)))

(defn find-best-arguments
  "strategy int state (seq-of generator) -> (seq-of state)
  
  find the best arguments for *both* viewpoints, starting with the viewpoint of
  the initial state. An argument is \"best\" if it survives all possible attacks
  from arguments which can be constructed using the provided argument
  generators, within the given search limits.  find-best-arguments allows
  negative conclusions to be explained, since it includes successful
  counterarguments in its resulting stream of arguments."
  [type strategy max-nodes max-turns state1 generators]
    (if (neg? max-turns)
    '()
    (searcharg type strategy max-nodes (dec max-turns)
               (find-arguments type strategy max-nodes state1 generators)
               generators)))

