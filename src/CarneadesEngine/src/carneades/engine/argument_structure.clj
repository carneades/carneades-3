(ns carneades.engine.argument-structure
 (:use carneades.engine.statement
       carneades.engine.dublin-core
       carneades.engine.unify
       carneades.engine.response))

; type literal = <symbol> | (not <symbol>), where the symbols are keys of 
; the statement table in a argument graph

; ArgumentNodes records are used to pass atomic arguments in and out of argument graphs.  For 
; example, these are returned in responses from argument generators.

(defrecord ArgumentNode
  [id               ; symbol
   title            ; string or hash table (for multiple languages)
   scheme           ; string
   strict           ; boolean
   weight           ; real number between 0.0 and 1.0, default 0.5
   conclusion       ; symbol, a key in the statement table of the argument graph
   premises         ; (string -> symbol) map, where strings are role names and the symbols are statement keys
   sources])        ; vector of source texts

 (defn make-argument-node
   [& values]
   (let [m (apply hash-map values)]
     (merge (ArgumentNode. 
              (gensym "a") ; id
              ""           ; title
              ""           ; 
              false        ; strict
              0.5          ; weight
              nil          ; conclusion
              {}           ; premises
              [])          ; sources 
            m)))
  
; type language = :en | :de | :nl | :fr ...

(defn status?
  [k]
  (contains? #{:stated, :questioned, :accepted, :rejected} k))

(defn proof-standard?
  [k]
  (contains? #{:dv, :pe, :cce, :brd} k))

(defrecord StatementNode
  [id               ; symbol
   wff              ; atomic statement
   status           ; status
   standard         ; proof-standard
   formulations])   ; (language -> string) map, natural language formulations of the statement
   
(defrecord ArgumentGraph 
  [id               ; symbol
   title            ; string or hash table (for multiple languages)
   main-issue       ; symbol, a key into the statement table
   keys             ; (statement -> symbol) map
   statements       ; (symbol -> StatementNode) map, i.e. a "key list" of atomic statements
   arguments])      ; (symbol -> ArgumentNode) map

 (defn make-argument-graph
   [& values]
   (let [m (apply hash-map values)]
   (merge (ArgumentGraph. 
            (gensym "ag")    ; id
             ""              ; title
             nil             ; main-issue
             {}              ; keys
             {}              ; statements
             {})             ; arguments
          m)))

(defn- update-statement-nodes
  "argument-graph (seq-of statement) -> argument-graph"
  [ag1 stmts]
  (reduce (fn [ag2 stmt] 
            (let [node (get (:statements ag2) (statement-atom stmt))]
              (if node 
                ag2 
                (assoc ag2 :statements (assoc (:statements ag2)
                                              (statement-atom stmt)
                                              (StatementNode. 
                                                (gensym "s") 
                                                (statement-atom stmt) 
                                                :stated :pe {}))))))
          ag1
          stmts))

(defn- literal
  "Returns P or (not P), where P is the key of a statement in the key list of the argument graph.
   That is, the keys are used as propositional letters." 
  [ag stmt]
  (if (statement-pos? stmt)
    (get (:keys ag) stmt)
    (list 'not (get (:keys ag) stmt))))
 
(defn- argument-template->argument-node
  "argument-graph argument-template -> argument-node"
  [ag at]
  (make-argument-node 
    :id (:id at)           
    :title (:title at)      
    :scheme (:scheme at) 
    :strict (:strict at)
    :weight (:weight at)  
    :conclusion (literal ag (:conclusion at))
    :premises (zipmap (keys (:premises at)) 
                      (map (fn [p] (literal ag p)) 
                           (vals (:premises at)))) 
    :sources (:sources at)))
   
(defn assert-argument
  "argument-graph argument-template -> argument-graph
   If some argument with the same id exists in the argument graph, then the existing argument is 
   replaced by the new argument. Cycles in the resulting argument graph are now permitted."
  [ag1 arg] 
  {:pre [(ground? (:conclusion arg)) 
         (every? ground? (vals (:premises arg)))]}
  (let [ag2 (update-statement-nodes ag1 (conj (vals (:premises arg)) (:conclusion arg)))]
    (assoc ag2
           :arguments (assoc (:arguments ag)
                             (:id arg)
                             (argument-template->argument-node ag2 arg)))))

(defn assert-arguments
  "argument-graph (collection-of argument) -> argument-graph"
  [ag args]
  {:pre [(not (nil? ag))]}
  (reduce (fn [ag arg] (assert-argument ag arg)) ag args))

; START HERE - make changes required by the change of representation of argument-nodes to 
; use "literals" in the conclusion and premises, instead of statements.

(defn argument-variables
  "argument -> (seq-of symbol)
   Returns a seq containing the variables of the argument arg"
  [arg]
  (distinct (concat (mapcat #(variables (:atom %)) (vals (:premises arg)))
                    (variables (:conclusion arg)))))
 
(defn instantiate-argument
  "argument substitutions -> argument
   Instantiate the variables of an argument by applying substitions"
  [arg subs]
  (assoc arg
         :id (gensym "a")
         :premises (zipmap (keys (:premises arg)) 
                           (map (fn [a] (apply-substitutions subs a)) 
                                (vals (:premises arg))))
         :conclusion (apply-substitutions subs (:conclusion arg))))

(defn- get-statement-node
  "argument-graph statement -> statement node
   Returns a statement node for the statement, creating one if one does not exist
   in the argument-graph."
  [ag stmt]
  (or (get (:statements ag) (statement-atom stmt))
      (StatementNode. (gensym "s") (statement-atom stmt) :stated :pe {})))
 
(defn- set-status
  "argument-graph statement status -> argument-graph"
  ([ag stmt]
    (set-status ag stmt nil))
  ([ag stmt new-status]
    {:pre [(status? new-status)]}
    (let [n (get-statement-node stmt)]
      (assoc ag :statements (assoc (:statements ag)
                                   (statement-atom stmt) 
                                   (assoc n :status (or new-status :stated)))))))
           
(defn state
  "argument-graph (seq-of statement) -> argument-graph
   Changes, non-destructively, the status of each statement in the list to 
   stated in the argument graph.  Statement nodes are created for any statements
   without nodes in the argument graph."
  [ag statements]
  (reduce #(set-status %1 %2 :stated) ag statements))

(defn question
  "argument-graph (seq-of statement) -> argument-graph"
  [ag statements]
  (reduce #(set-status %1 %2 :questioned) ag statements))

(defn accept
  "argument-graph (collection-of statement) -> argument-graph"
  [ag statements]
  (reduce #(set-status %1 %2 (if (statement-pos? %2)
                                     :accepted
                                     :rejected)) ag statements))

(defn reject [ag statements]
  (reduce #(set-status %1 %2 (if (statement-pos? %2)
                                     :rejected
                                     :accepted)) ag statements))

(defn assoc-standard
  "argument-graph  proof-standard (list-of statement) -> argument-graph
   Assigns the given proof standard to each statement in the list, creating
   statment nodes for statements without nodes in the argument graph."
  [ag ps statements]
  (reduce (fn [ag stmt]
            (let [n (get-statement-node ag stmt)]
              (assoc ag :statements (assoc (:statements ag)
                                           (statement-atom stmt) 
                                           (assoc n :standard ps)))))
          ag statements))

(defn get-argument
  "argument-graph symbol -> argument | nil"
  [ag id]
  ((:arguments ag) id))

(defn get-arguments
  "argument-graph (seq-of symbol) -> (seq-of argument)"
  [ag ids]
  (filter identity (map #(get-argument ag %) ids)))

(defn arguments 
  " Returns all arguments in an argument graph pro and con some statement,
     or all arguments in the argument graph, if no statement is provided.
     argument-graph [statement] -> (seq-of argument)"
   ([ag stmt]
      (filter (fn [arg] (= (statement-atom (:conclusion arg)) (statement-atom stmt)))
              (arguments ag)))                
   ([ag]
      (if-let [args (vals (:arguments ag))]
        args
        ())))

(defn pro-arguments
  "argument-graph statement -> (seq-of argument)"
  [ag s]
  (filter (fn [arg]
            (= (statement-pos? s) 
               (statement-pos? (:conclusion arg))))
          (arguments ag s)))

(defn con-arguments
  "argument-graph statement -> (seq-of argument)"
  [ag s]
  (pro-arguments ag (statement-complement s)))

(defn schemes-applied
  "argument-graph statement -> (seq-of string)"
  [ag stmt]
  (map :scheme (arguments ag stmt)))

(defn accepted?
  "argument-graph statement -> boolean"
  [ag s]
  (let [n (get-statement-node ag s)]
    (if (statement-pos? s)
      (= (:status n) :accepted)
      (= (:status n) :rejected))))

(defn rejected?
  "argument-graph statement -> boolean"
  [ag s]
  (let [n (get-statement-node ag s)]
    (if (statement-pos? s)
      (= (:status n) :rejected)
      (= (:status n) :accepted))))

(defn assumed?
  "argument-graph statement -> boolean"
  [ag s]
  (contains? #{:accepted :rejected} 
            (:status (get-statement-node ag s))))

(defn questioned?
  "argument-graph statement -> boolean"
  [ag s]
  (= :questioned (:status (get-statement-node ag s))))

(defn stated? 
  [ag s]
  (= :stated (:status (get-statement-node ag s))))

(defn issue?
  "argument-graph statement -> boolean
   An statement is an issue if it has not been accepted (assumed true) or rejected 
   (assumed false) in the argument graph."
  [ag s]
  (not (assumed? ag s)))

(defn assumptions 
  "argument-graph -> (seq-of statement)
   Returns a sequence of positive and negative statements for the 
   statements which have been accepted or rejected in the argument graph."
  [ag]
  (map (fn [n]
         (if (= (:status n) :accepted)
           (:statement n)
           (statement-complement (:statement n))))
       (filter (fn [n] (contains? #{:accepted :rejected} (:status n)))
               (vals (:statements ag)))))





