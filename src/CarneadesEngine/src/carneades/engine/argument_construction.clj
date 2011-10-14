(ns ^{:doc "Argument construction using generators."}
  carneades.engine.argument-construction
  (:use clojure.set
        clojure.pprint
        carneades.engine.statement
        carneades.engine.unify
        carneades.engine.argument
        carneades.engine.argument-builtins
        carneades.engine.argument-from-arguments))

(defstruct argument-template 
  :guard      ; term with all unbound variables of the argument
  :instances  ; set of ground terms matching the guard
  :argument)  ; argument

(defstruct goal
  :issue          ; statement
  :substitutions  ; (term -> term) map
  :depth)         ; int
  
(defstruct ac-state       
  :goals                  ; (symbol -> goal) map, where the symbols are goal ids
  :open-goals             ; set of goal ids (todo: change to a priority queue)
  :closed-issues          ; set of statements for goals already processed 
  :graph                  ; argument-graph 
  :arg-templates          ; (symbol -> argument template) map; symbols are template ids
  :asm-templates)         ; vector of non-ground statements

(defn initial-ac-state
  "statement argument-graph -> ac-state"
  [issue ag]
  (let [goal-id (gensym "g")]
    (struct-map ac-state
               :goals {goal-id (struct-map goal :issue issue :substitutions {} :depth 0)}
               :open-goals #{goal-id}
               :closed-issues #{}
               :graph ag
               :arg-templates {}
               :asm-templates [])))

(defn- closed-issue?
  [ac-state subs stmt1]
  (some (fn [stmt2] (unify stmt1 stmt2 subs)) 
        (:closed-issues ac-state)))

(defn- add-subgoal
  "ac-state goal substitutions statement -> ac-state"
  [s g1 subs stmt]
  (if (closed-issue? s subs stmt)
    (do (println "closed-issue: " stmt) s)
    (let [id (gensym "g")]
      (assoc s
             :goals (assoc (:goals s) 
                           id 
                           (struct-map goal 
                                       :issue stmt
                                       :substitutions subs
                                       :depth (inc (:depth g1))))
             :open-goals (conj (:open-goals s) id)))))
  
(defn- process-premises
  "ac-state goal substitutions (seq-of premise) -> ac-state
   Add goals for the premises to the open goals, incrementing the depth."
  [state1 g1 subs premises]
  ; (pprint "process-premises")
  (reduce (fn [s p] (add-subgoal s g1 subs (premise-statement p)))
          state1
          premises))  

(defn- process-conclusion
  "ac-state goal substitutions statement -> ac-state
   To enable the construction of rebuttals, add the complement of the conclusion 
   to the goals of the ac-state, unless it is a closed issue. Note: The depth of 
   the goal for the rebuttal is the same as the depth of the goal being rebutted, 
   not incremented, so that if goals are prioritized by depth complementary goals
   will get the same priority."
  [state1 g1 subs statement] 
  ; (pprint "process-conclusion")
  (let [stmt (statement-complement statement)]
    (if (closed-issue? state1 subs stmt)
      state1
      (let [id (gensym "g")
            g2 (struct-map goal 
                           :issue stmt
                           :substitutions subs
                           :depth (:depth g1))]
        (assoc state1
               :goals (assoc (:goals state1) id g2)
               :open-goals (conj (:open-goals state1) id))))))
  
(defn- process-argument
  "ac-state goal substitutions argument -> ac-state"
  [state1 goal subs arg]
  ; (println "process-argument:") (pprint arg)
  ; (println "subs:") (pprint subs)
  (if (not arg)
    state1
    (-> state1
        (process-premises goal subs (:premises arg))
        (process-conclusion goal subs (:conclusion arg))
         ; to do: add a goal for excluding the rule/scheme, for undercutters
        (assoc :arg-templates (assoc (:arg-templates state1) 
                                     (gensym "at") 
                                     (struct-map argument-template
                                         :guard `(~'guard ~@(argument-variables arg))
                                         :instances #{}
                                         :argument   arg))))))

(defn- add-instance
  "map symbol term -> map"
  [arg-template-map key term]
  ; (pprint "add instance")
  (let [arg-template (get arg-template-map key)]
    (assoc arg-template-map key 
           (assoc arg-template 
                  :instances (conj (:instances arg-template) term)))))
  
(defn- apply-arg-templates
  "ac-state substitutions -> ac-state
   Apply the argument templates to the substitutions of the response, adding
   arguments to the argument graph of the ac-state for all templates with ground
   guards, if the instance is new.  Add the new instance to the set of instances 
   of the template."
  [state1 subs]
  ; (pprint "apply-arg-templates state: ")
  (reduce (fn [s k]
            (let [template (get (:arg-templates s) k)
                  trm (apply-substitution subs (:guard template))]
              ; (println "template:")
              ; (pprint template)
              ; (println "template instances: " (:instances template))
              (if (or (not (ground? trm))
                      (contains? (:instances template) trm))
                s
                (assoc s 
                       :graph (assert-argument 
                                (:graph s) 
                                (instantiate-argument (:argument template) subs))
                       :arg-templates (add-instance (:arg-templates s) k trm)))))
          state1
          (keys (:arg-templates state1))))

(defn- apply-asm-templates
  "ac-state goal substitutions -> ac-state"
  [state1 g1 subs]
  ; (println "asm-templates: ")
  ; (pprint (:asm-templates state1))
  (reduce (fn [state2 template] 
            (let [ag (:graph state2)
                  asm (apply-substitution subs template)]
              ; (println "asm: " asm)
              (if (not (ground? asm))
                state2
                (let [ag2 (condp = (status ag asm)
                            :stated (accept ag [asm])
                            :questioned ag
                            :rejected (question ag [asm])
                            :accepted ag)]
                  (add-subgoal (assoc state2 :graph ag2)
                               g1 
                               subs 
                               (statement-complement asm))))))
          state1
          (:asm-templates state1)))          

(defn- process-assumptions
  "ac-state substitions (seq-of statement) -> ac-state
   add the assumptions of the response to the assumption templates"
  [state subs asms]
  (assoc state :asm-templates 
         (concat (:asm-templates state) 
               (map (fn [asm] (apply-substitution subs asm))
                    asms))))
         
(defn- apply-response
  "ac-state goal response -> ac-state"
  [state1 goal response]
  ; (pprint "apply-response")
  (-> state1
    (process-argument goal (:substitutions response) (:argument response))
    (process-assumptions (:substitutions response)  (:assumptions response)) 
    (apply-arg-templates (:substitutions response))
    (apply-asm-templates goal (:substitutions response))))

; type generator : statement substitutions -> (seq-of response)

(defn select-random-member
  "set -> any
   Select and return a random member of a set"
  [set] 
  (let [sq (seq set)] 
    (nth sq (rand-int (count sq)))))
       
(defn- reduce-goal
  "ac-state symbol generators -> ac-state
   reduce the goal with the given id remove it from from the goal lists"
  [state1 id generators1]
  ; (pprint "reduce-goal")
  (let [goal (get (:goals state1) id)
        generators2 (concat (list (generate-responses-from-in-statements (:graph state1)))
                            generators1) 
        state2  (assoc state1    
                       :goals (dissoc (:goals state1) id)
                       :open-goals (disj (:open-goals state1) id)
                       :closed-issues (conj (:closed-issues state1) 
                                            (:issue goal)))]
    (println "goal: " goal)
    ; apply the generators to the selected goal
    (let [responses (apply concat 
                           (map (fn [g] 
                                  (g (:issue goal) (:substitutions goal))) 
                                generators2))]   
      (println "responses: " (count responses))
      (pprint responses)
      (reduce (fn [s r] (apply-response s goal r))
              state2
              responses))))
    
(defn- reduce-goals
  "ac-state integer (seq-of generator) -> ac-state
   Construct arguments for both viewpoints and combine the arguments into
   a single argument graph of a ac-state.  All arguments found within the given
   resource limits are included in the argument graph of the resulting ac-state."
  [state1 max-goals generators]
  ; (pprint "reduce-goals")
  (let [id (select-random-member (:open-goals state1))]     
    (if (or (not id) (<= max-goals 0))
      state1 
      (recur (reduce-goal state1 id generators) 
             (dec max-goals) 
             generators))))

(defn construct-arguments
  "argument-graph statement int (set-of statement) (seq-of generator) -> argument-graph
   Construct an argument graph for both sides of an issue."
  ([ag1 issue max-goals assumptions generators1]
    ; (pprint "argue")
    (let [ag2 (accept ag1 assumptions)
          generators2 (concat (list (builtins))  generators1)
          final-state (reduce-goals (initial-ac-state issue ag2) 
                            max-goals 
                            generators2)]
      (pprint (:arg-templates final-state))
      (println "closed issues: " (:closed-issues final-state))
      (:graph final-state)))
  ([issue max-goals assumptions generators]
    (let [ag (assoc (argument-graph) :main-issue issue)]
      (construct-arguments ag max-goals assumptions generators))))

