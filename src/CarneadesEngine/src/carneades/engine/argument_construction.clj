;;; Copyright (c) 2011 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Argument construction using generators."}
  carneades.engine.argument-construction
  (:use clojure.set
        clojure.pprint
        carneades.engine.uuid
        carneades.engine.statement
        carneades.engine.unify
        carneades.engine.argument-graph
        carneades.engine.argument
        carneades.engine.argument-generator
        carneades.engine.argument-builtins)
  (:require [clojure.tools.logging :refer [info debug]]))

(defrecord ArgumentTemplate
  [guard       ; term with all unbound variables of the argument
   instances   ; set of ground terms matching the guard
   argument])  ; partially instantiated argument

(defn make-argument-template
  [& values]
  (let [m (apply hash-map values)]
    (merge (ArgumentTemplate.
            nil    ; guard
            #{}    ; instances
            nil)   ; argument
           m)))

(defn- argument-template? [x] (instance? ArgumentTemplate x))

(defrecord Goal
  [issues         ; (seq-of literals)  ; open issues
   closed-issues  ; set of literals
   substitutions  ; (term -> term) map
   depth])        ; int

(defn make-goal
   [& values]
   (let [m (apply hash-map values)]
   (merge (Goal.
             ()     ; issues
            #{}     ; closed issues
             {}     ; substitutions
             0)     ; depth
          m)))

(defn- goal? [x] (instance? Goal x))

(defrecord ACState        ; argument construction state
  [goals                  ; (symbol -> goal) map, where the symbols are goal ids
   open-goals             ; set of goal ids (todo: change to a priority queue)
   graph                  ; argument-graph
   arg-templates          ; (symbol -> argument template) map; symbols are template ids
   asm-templates])        ; vector of non-ground literals

(defn make-acstate
  [& values]
  (let [m (apply hash-map values)]
    (merge (ACState.
             {}     ; goals
             '()    ; open goals
             (make-argument-graph)
             {}     ; argument templates
             [])    ; assumption templates
           m)))

(defn- acstate? [x] (instance? ACState x))

(defn- initial-acstate
  "literal argument-graph -> acstate"
  [issue ag]
  (let [goal-id (gensym "g")]
    (make-acstate
      :goals {goal-id (make-goal :issues (list issue))}
      :open-goals (list goal-id)  ; #{goal-id}
      :graph ag)))


(defn- add-goal
  "acstate goal -> acstate"
  [state1 goal]
  {:pre [(acstate? state1)]
   :post [(acstate? %)]}
  (let [id (gensym "g")]
    (assoc state1
           :goals (assoc (:goals state1) id goal)
           :open-goals (concat (:open-goals state1) (list id))))) ; breadth-first

(defn- remove-goal
  "acstate symbol -> acstate"
  [state1 goal-id]
  (assoc state1
         :open-goals (rest (:open-goals state1))
         :goals (dissoc (:goals state1) goal-id)))

(defn- update-issues
  "acstate goal response -> acstate
   Add a goal to the state by replacing the first issue of the parent goal
   with the issues of the response. The depth of the parent goal is incremented in this new goal."
  [state1 g1 response]
  (let [arg (:argument response)
        subs (:substitutions response)
        closed-issues (conj (:closed-issues g1)
                            (apply-substitutions subs (first (:issues g1))))]
    (if (nil? arg)
      (add-goal state1
                (make-goal
                  :issues (rest (:issues g1))
                  :closed-issues closed-issues
                  :substitutions subs
                  :depth (inc (:depth g1))))
      (let [conclusion (literal->sliteral (conclusion-literal arg))]
        ;; undercutter `(~'undercut ~(:id arg))]
        (add-goal state1
                  (make-goal
                    ; pop the first issue and add issues for the
                    ; premises and exceptions of the argument to the beginning for
                    ; depth-first search
                    :issues (concat (map (fn [p] (literal->sliteral (premise-literal p)))
                                         (concat (:premises (:argument response))
                                                 (:exceptions (:argument response))))
                                    ; (list undercutter)
                                    (rest (:issues g1)))
                    :closed-issues closed-issues
                    :substitutions subs
                    :depth (inc (:depth g1))))))))


(defn- add-instance
  "argument-template-map symbol term -> argument-template-map"
  [arg-template-map key term]
  {:pre [(map? arg-template-map)
         (symbol? key)
         (argument-template? (get arg-template-map key))
         ;;(list? term)
         ]
   :post [(map? %)]}
  (let [arg-template (get arg-template-map key)
        result (assoc arg-template-map key
                      (assoc arg-template
                        :instances (conj (:instances arg-template) term)))]
    result))

(defn- add-argument-to-graph
  "acstate argument -> acstate
   Enters a ground argument, and undercutters of the argument, into the
   argument graph of the acstate, unless an equivalent argument is already
   in the graph."
  [s arg]
  {:pre [(acstate? s) (argument? arg)]
   :post [(acstate? %)]}
  (let [schemes (schemes-applied (:graph s) (:conclusion arg))]
    (printf "schemes: %s\n:" schemes)
    (if (contains? schemes (:scheme arg))
      ;; the scheme has already been applied to this issue
      ;; Todo: This seems too restrictive.  Consider two
      ;; arguments from expert opinion with the same conclusion,
      ;; but from two different experts.
      s
      (assoc s
        :graph (enter-arguments (:graph s)
                                (cons arg (make-undercutters arg)))))))

(defn- add-argument-instance-to-templates
  "acstate symbol term -> acstate"
  [s k trm]
  {:pre [(acstate? s) (symbol? k)]
   :post [(acstate? %)]}
  (assoc s :arg-templates (add-instance (:arg-templates s) k trm)))

(defn- apply-arg-templates
  "acstate goal response -> acstate
   Apply the argument templates to the substitutions of the response, adding
   arguments to the argument graph of the ac-state for all templates with ground
   guards, if the instance is new.  Add the new instance to the set of instances
   of the template. Also add an undercutter for each exception of the argument to the graph."
  [state1 goal response]
  {:pre [(acstate? state1) (response? response)]
   :post [(acstate? %)]}
  ;; (prn "[apply-arg-templates] =" goal)
  (let [subs (:substitutions response)]
    (reduce (fn [s k]
              (let [template (get (:arg-templates s) k)
                    trm (apply-substitutions subs (:guard template))
                    add-undercutters (fn [s arg]
                                       (reduce (fn [s e]
                                                 (add-argument-to-graph
                                                  s
                                                  (make-argument
                                                   :id (make-urn)
                                                   :conclusion `(~'undercut ~(:id arg))
                                                   :pro true
                                                   :strict false
                                                   :weight 0.5
                                                   :premises [e]
                                                   :exceptions []
                                                   :scheme (:scheme arg))))
                                               s
                                               (:exceptions arg)))]
                (if (or (not (ground? trm))
                        (contains? (:instances template) trm))
                  s
                  (let [arg (instantiate-argument (:argument template) subs)]
                    (-> s
                        (add-argument-to-graph arg)
                        (add-argument-instance-to-templates k trm)
                        (add-undercutters arg))))))
            state1
            (keys (:arg-templates state1)))))

(defn- apply-asm-templates
  "acstate goal substitutions -> acstate"
  [state1 g1 subs]
  (reduce (fn [state2 template]
            (let [ag (:graph state2)
                  asm (apply-substitutions subs template)]
              (if (not (ground? asm))
                state2
                (let [ag2 (assume ag [asm])]
                  (add-goal (assoc state2 :graph ag2)
                            (make-goal
                              :issues (list (literal-complement (literal->sliteral asm)))
                              :substitutions subs
                              :depth (inc (:depth g1))))))))
          state1
          (:asm-templates state1)))

(defn- process-assumptions
  "acstate response -> acstate
   add the assumptions of the response to the assumption templates"
  [state response]
  (let [subs (:substitutions response)
        asms (:assumptions response)
        asms (map (fn [asm] (apply-substitutions subs asm)) asms)
        state (assoc state :graph (assume (:graph state) (filter ground? asms)))
        ;; state (update-in state [:graph] assume (map ground? asms))
        ]
    (if (empty? asms)
      state
      (assoc state :asm-templates
             (concat (:asm-templates state)
                     (filter (complement ground?) asms))))))

(defn- process-argument
  "acstate response -> acstate"
  [state1 response]
  (let [arg (:argument response)]
    (cond (nil? arg) state1,
          (ground-argument? arg) (add-argument-to-graph state1 arg),
          :else (assoc state1
                  :arg-templates
                  (assoc (:arg-templates state1)
                    (gensym "at")
                    (make-argument-template
                     :guard `(~'guard ~@(argument-variables arg))
                     :instances #{}
                     :argument arg))))))

(defn- apply-response
  "acstate goal response -> acstate"
  [state1 goal response]
  (-> state1
      (update-issues goal response)
      (process-argument response)
      (process-assumptions response)
      (apply-arg-templates goal response)
      (apply-asm-templates goal (:substitutions response))))

(defn select-random-member
  "set -> any
   Select and return a random member of a set"
  [set]
  (let [sq (seq set)]
    (nth sq (rand-int (count sq)))))

(defn- generate-subs-from-basis
  "argument-graph -> argument-generator"
  [ag1]
  (reify ArgumentGenerator
    (generate [this goal subs]
      (reduce (fn [l wff]
                (let [subs2 (unify goal (literal-atom wff) subs)]
                  (if (empty? subs2)
                    l
                    (conj l (make-response subs2 () nil)))))
              []
              (basis ag1)))))

(defn- reduce-goal
  "acstate symbol generators -> acstate
   reduce the goal with the given id"
  [state1 id generators1]
  ;; Remove the goal from the state. Every goal is reduced at most once. The remaining issues of the goal
  ;; are passed down to the children of the goal, so they are not lost by removing the goal.
  (let [goal (get (:goals state1) id),
        state2 (remove-goal state1 id)]
    ;; (prn "[reduce-goal]")
    ;; (prn "goal = ")
    ;; (pprint goal)
    (if (empty? (:issues goal))
      state2 ; no issues left in the goal
      (let [issue (apply-substitutions (:substitutions goal) (first (:issues goal)))]
        (if (contains? (:closed-issues goal) issue)
          ;; the issue has already been handled for this goal and closed
          ;; Add a goal for the remaining issues and return
          (add-goal state2 (assoc goal :issues (rest (:issues goal))))
          ;; close the selected issue in state3 and apply the generators to the issue and its complement
          ;; Rebuttals are constructed even if no pro arguments can be found
          ;; This has the advantage that the same argument graph is constructed for the
          ;; issue P as the issue (not P). The positive or negative form of the issue or query is no longer
          ;; relevant for the purpose of argument construction. But it is still important
          ;; for argument evaluation, where burden of proof continues to play a role.
          (let [generators2 (concat (list (generate-subs-from-basis (:graph state2)))
                                    generators1)]
            (let [responses (apply concat (map (fn [g]
                                                 (concat (generate g issue
                                                                   (:substitutions goal))
                                                         (generate g (literal-complement issue)
                                                                   (:substitutions goal))))
                                               generators2))]
              ;; (prn "responses=" )
              ;; (pprint responses)
              (reduce (fn [s r] (apply-response s goal r))
                      state2
                      responses))))))))

(defn- reduce-goals
  "acstate integer (seq-of generator) -> acstate
   Construct arguments for both viewpoints and combine the arguments into
   a single argument graph of a acstate.  All arguments found within the given
   resource limits are included in the argument graph of the resulting acstate."
  [state1 max-goals generators]
  (if (or (empty? (:open-goals state1))
          (<= max-goals 0))
    (do
      (if (empty? (:open-goals state1))
        (prn "EMPTY GOALS")
        (prn "EXHAUSTED"))
      state1)
    (let [id (first (:open-goals state1))]
      (if (not id)
        state1
        (recur (reduce-goal state1 id generators)
               (dec max-goals)
               generators)))))

(defn- notify-observers
  "Informs generators satisfying the ArgumentConstructionObserver protocol that
   the construction is finished."
  [generators]
  (doseq [generator generators]
    (when (satisfies? ArgumentConstructionObserver generator)
      (finish generator))))

(defn construct-arguments
  "argument-graph literal int (coll-of literal) (seq-of generator) -> argument-graph
   Construct an argument graph for both sides of an issue."
  ([ag1 issue max-goals facts generators1]
    (let [ag2 (accept ag1 facts)
          generators2 (concat (list (builtins)) generators1)
          graph (:graph (reduce-goals (initial-acstate issue ag2)
                                      max-goals
                                      generators2))]
    (notify-observers generators2)
    graph))
  ([issue max-goals facts generators]
      (construct-arguments (make-argument-graph) issue max-goals facts generators)))
