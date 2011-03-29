;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.engine.shell
  (:use clojure.set
        ;clojure.contrib.pprint
        clojure.contrib.profile
        clojure.contrib.monads
        carneades.engine.utils
        [carneades.engine.abduction :as abd]
        carneades.engine.argument-search
        carneades.engine.ask
        carneades.engine.unify
        [carneades.engine.argument :only (node-statement get-nodes arguments)]
        [carneades.engine.search :only (depth-first resource search traverse)]
        carneades.ui.diagram.viewer)
  (:require [carneades.engine.argument :as arg]))

(defn solutions [states]
  "(seq-of state) -> (seq-of state)

  A state is a \"solution\" if the instantiated topic of the state is in."
  (filter (fn [s]
            (let [sub (apply-substitution (:substitutions s) (:topic s))]
              (arg/in? (:arguments s) sub)))
          states))

(defn succeed? [query engine]
  "engine -> boolean

    True if at least one goal state was found by the engine
  "
  (not (empty? (solutions (engine query)))))

(defn fail? [query engine]
  "engine -> boolean

    True if no state found by the engine is a goal state
  "
  (empty? (solutions (engine query))))

(defn unite-solutions
  [sols]
  (arg/unite-argument-graphs (map :arguments sols)))

(defn add-candidates
  [ag candidates subs]
  (arg/assert-arguments ag (map
                         (fn [c]
                           (arg/instantiate-argument
                             (:argument c)
                             subs))
                         candidates)))

(defn unite-solutions-with-candidates
  [sols]
  (arg/unite-argument-graphs
    (map (fn [s] (add-candidates
                   (:arguments s)
                   (:candidates s)
                   (:substitutions s)))
      sols)))

(defn monadic-construction
    [goal max-nodes ag generators askable? get-answer-cont]
    (println "monadic construction started:" get-answer-cont (type get-answer-cont))
    (run-cont
      (domonad cont-m
        [ga (call-cc (fn [c] 
                       ;(def continuation c)
                       (get-answer-cont c)))]
        (do (println "domonad" ga " - " (type ga))
            (find-best-arguments traverse depth-first max-nodes 1
                          (initial-state goal ag) (cons (ask-user askable? ga) generators))))))

(defn future-construction
    [to-engine from-engine]    
    (println "future construction started:")
    (future-call
        (fn []
            (println "future-construction waiting for initial request")
            (let [to (atom to-engine),
                     from (atom from-engine),
                     [goal max-nodes ag generators askable?] @@to, ; read
                      sol (doall (find-best-arguments traverse depth-first max-nodes 1
                                       (initial-state goal ag) (cons (ask-user askable? to from) generators)))]
                   (println "engine has found solution" sol)
                   (deliver @from (list 'solution sol))))))

(defn continue-construction
  [goal max-nodes state generators]
     (find-best-arguments traverse depth-first max-nodes 1
                          state generators))

(defn construct-arguments
  "integer integer argument-graph (seq-of generator) -> statement -> (seq-of state)"
  ([goal max-nodes ag generators]
     (find-best-arguments traverse depth-first max-nodes 1
                          (initial-state goal ag) generators))
  ([goal max-nodes max-turns strategy ag generators]
     (find-best-arguments traverse strategy max-nodes max-turns
                          (initial-state goal ag) generators)))

(defn construct-arguments-abductively
  ([goal max-nodes max-turns ag generators]
    (construct-arguments-abductively goal goal max-nodes max-turns ag generators :pro #{goal}))
  ([main-issue goal max-nodes max-turns ag generators viewpoint applied-goals]
    (println "current goal:" goal)
    (condp = max-turns
      0 ag,
      1 (unite-solutions (construct-arguments goal max-nodes ag generators)),
      (let [ag2 (prof :unite (unite-solutions (construct-arguments goal max-nodes ag generators))),
            asmpts (abd/assume-decided-statements ag2),
            new-goals (prof :abduction
                        (apply union
                    (if (= viewpoint :con)
                        (abd/statement-in-label ag2 asmpts main-issue)
                        (abd/statement-out-label ag2 asmpts main-issue)))),
            goals (difference new-goals applied-goals),
            new-vp (if (= viewpoint :pro)
                     :con
                     :pro)]
        ;(view ag2)
        ;(println "new goals:" new-goals)
        ;(println "goals    :" goals)
        (reduce (fn [ag3 g] (construct-arguments-abductively main-issue g max-nodes (- max-turns 1) ag3 generators new-vp (union applied-goals goals))) ag2 goals)))))

(defn make-engine* [max-nodes max-turns ag generators]
  "integer integer argument-graph (seq-of generator) -> statement -> 
   (seq-of state)"
  (fn [goal]
    (find-best-arguments search depth-first max-nodes max-turns
                         (initial-state goal ag) generators)))

(defn make-engine [max-nodes max-turns generators]
  "integer integer (seq-of generator) -> statement -> (seq-of state)
 
   a simplified version of make-engine*, using the default-context "
  (make-engine* max-nodes max-turns arg/*empty-argument-graph* generators))

(defn ask
  " ask: statement (statement -> (stream-of state)) -> void

    Displays the query with the substitions found in each state
    produced by the given inference engine or prints nothing if the stream is emtpy.
    Always terminates, as only states found given the resource limit of the
    inference engine will be displayed."
  [query engine]
  (map (fn [s] (clojure.contrib.pprint/pprint (apply-substitution (:substitutions s) query))) (solutions (engine query))))

; (defn show-state [state]
;   "view a diagram of the argument graph of a state"
;  (view (sget state :arguments)))

;; (defn show
;;   ([query engine]
;;      (show query engine true))
;;   ([query engine showall]
;;      (let [states (engine query)]
;;        (if showall
;;          (doseq [s states]
;;            (show-state s))
;;          (when-not (empty? states)
;;            (show-state (first states)))))))

;; (defn show1 [query engine]
;;   (show query engine false))

(defn- search-graph [pred objects]
  (let [n-ahead 100]
   (seque n-ahead (keep (fn [obj]
                          (when (pred obj)
                            obj))
                        objects))))

(defn- stmt-pred [stmt stmt-fmt to-search]
  (let [formatted (stmt-fmt stmt)]
    (.contains (.toLowerCase formatted) to-search)))

(defn- arg-pred [arg to-search]
  (let [title (:title arg)]
    (if-not (nil? title)
      (.contains (.toLowerCase title) to-search)
      false)))

(defn search-statements [ag stmt-fmt search-content options]
  "Produces a sequence of statements satisfying the search options.
   The sequence is produced in the background with seque. The 
   reading from the sequence can block if the reader gets ahead of the
   search

   The keys for options are ..."
  (let [to-search (.toLowerCase search-content)
        stmts (map node-statement (get-nodes ag))]
    (search-graph #(stmt-pred % stmt-fmt to-search) stmts)))

(defn search-arguments [ag search-content options]
  "See search-statements"
  (let [to-search (.toLowerCase search-content)]
    (search-graph #(arg-pred % to-search) (arguments ag))))

(defn search-all [ag stmt-fmt search-content options]
  "returns a seq of the form ((:stmt stmt1) (:arg arg1) (:stmt stmt2) ...)"
  (let [to-search (.toLowerCase search-content)
        stmts (search-statements ag stmt-fmt search-content options)
        args (search-arguments ag search-content options)]
    (interleaveall (partition 2 (interleave (repeat :stmt) stmts))
                   (partition 2 (interleave (repeat :arg) args)))))

