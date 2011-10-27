(ns carneades.engine.argument-graph
 (:use carneades.engine.statement
       carneades.engine.dublin-core
       carneades.engine.argument))


; A literal is a propositional letter, represented by a symbol, 
; or the negation of a propositional letter.

(defrecord Literal
  [positive         ; boolean
   letter])         ; symbol

(defn make-literal
  [positive letter]
  (Literal. positive letter))

(defrecord ArgumentNode
  [id               ; symbol
   title            ; string or hash table (for multiple languages)
   scheme           ; string
   strict           ; boolean
   weight           ; 0.0-1.0, default 0.5
   conclusion       ; literal
   premises         ; (string -> literal) map, where the keys are role names
   sources])        ; vector of sources

(defn make-argument-node
   "key value ... -> argument-node"
   [& key-values]
   (merge (ArgumentNode. 
            (gensym "a") ; id
            ""           ; title
            ""           ; scheme
            false        ; strict
            0.5          ; weight
            nil          ; conclusion
            {}           ; premises
            [])          ; sources 
          (apply hash-map key-values)))
  
(defn proof-standard?
  [k]
  (contains? #{:dv, :pe, :cce, :brd} k))

; type language = :en | :de | :nl | :fr ...

(defrecord StatementNode
  [id               ; symbol, same as the propositional letter in the key list
   wff              ; ground atomic formula or nil
   weight           ; nil or 0.0-1.0, default nil
   standard         ; proof-standard
   text             ; (language -> string) map, natural language formulations of the statement
   premise-of       ; (set-of symbol), argument node ids
   conclusion-of])  ; (set-of symbol), argument node ids

(defn- make-statement-node
  [stmt]
  (StatementNode. (gensym "s") (statement-atom stmt) nil :pe {} #{} #{}))
   
(defrecord ArgumentGraph 
  [id               ; symbol
   title            ; string or hash table (for multiple languages)
   main-issue       ; symbol, a key into the statement node map
   language         ; (statement -> symbol) map, i.e. a "key list"
   statement-nodes  ; (symbol -> StatementNode) map, 
   argument-nodes   ; (symbol -> ArgumentNode) map
   references])     ; (symbol -> Source) map

(defn make-argument-graph
   "key value ... -> argument-graph"
   [& key-values]  
   (merge (ArgumentGraph. 
            (gensym "ag")   ; id
            ""              ; title
            nil             ; main-issue
            {}              ; keys
            {}              ; statement nodes
            {}              ; argument nodes
            {})             ; references to sources
          (apply hash-map key-values)))

(defn get-statement-node
  "argument-graph statement -> [argument-graph statement-node]
   Returns a [argument-node statement node] pair for the statement, 
   creating one if one does not exist in the initial argument-graph."
  [ag stmt]
  (let [n (get (:statement-nodes ag) 
               (get (:language ag) (statement-atom stmt)))]
    (if n 
      [ag n]
      (let [n2 (make-statement-node stmt)
            ag2 (assoc ag 
                       :language 
                       (assoc (:language ag)
                              (:wff n2)
                              (:id n2))
                       :statement-nodes 
                       (assoc (:statement-nodes ag)
                              (:id n2)
                              n2))]
        [ag2 n2]))))

(defn- update-statement-node
  "argument-graph symbol key value ... -> argument-graph
   Updates the statement node with the given id with the values of the 
   properties with the given keys, but retaining the values of other properties."
  [ag id & key-values]
  (assoc ag 
         :statement-nodes (assoc (:statement-nodes ag)
                                 id
                                 (merge (get (:statement-nodes ag) id) 
                                        (apply hash-map key-values)))))

(defn- statement->literal
  "argument-graph statement -> literal" 
  [ag stmt]
  (make-literal (statement-pos? stmt)
                (get (:language ag) stmt)))

(defn- literal->statement
  "argument-graph literal -> statement"
  [ag literal]
  (if (:positive literal)
    (statement-complement (:wff (get (:statement-nodes ag) (:letter literal))))
    (:wff (get (:statements ag) (:letter literal)))))

(defn- link-conclusion
  [ag literal arg-id]
  (let [n (get (:statement-nodes ag) (:letter literal))]
    (update-statement-node 
      ag 
      (:letter literal)
      (assoc n :conclusion-of (conj (:conclusion-of n) arg-id)))))

(defn- link-premises
  [ag1 literals arg-id]
  (reduce (fn [ag2 literal]
            (let [n (get (:statement-nodes ag2) (:letter literal))]
              (update-statement-node 
                ag2 
                (:letter literal)
                (assoc n :premise-of (conj (:conclusion-of n) arg-id)))))
          ag1
          literals))

(defn- find-sources
  "argument-graph (seq-of string) -> (seq-of source)
   Returns the sources with the given ids in the
   reference list of the argument graph."
  [ag ids]
  (concat (map (fn [id] (get (:references ag) id)) ids)))
                          
(defn- update-references
  "argument-graph source -> argument-graph
   Checks whether a source is in the list of references of the
   argument graph and updates the list to add the source if it
   is new. Does not overwrite or modify existing references."
  [ag source]
  {:pre (not (nil? (first (:identifier source))))}
  (let [source2 (get (:references ag) 
                     (first (find-sources (:identifier source))))]
    (if source2
      ; the source was already in the reference list
      ag 
      ; else add the new source to the reference list
      (assoc ag :references
             (assoc (:references ag)
                    (first (:identifier source))
                    source)))))

(defn- source-ids
  "argument-graph (seq-of source) -> (seq-of string)
   Returns the identifiers used to identifty each source in the list 
   of references of the argument map."
  [ag sources]
  (filter (fn [x] (not (nil? x)))
          (map (fn [src] 
                 (first (map (fn [id] 
                               (get (:references ag) id))
                             (:identifier src))))
               sources)))
        
(defn assert-argument
  "argument-graph argument -> argument-graph
   Converts a one-step argument to an argument node and adds
   it to the argument graph. Precondition: statement nodes 
   have already been created in the argument graph for all 
   the statements in the argument."
  [ag1 arg]
  {:pre [(ground? (:conclusion arg)) 
         (every? ground? (vals (:premises arg)))]}
  (let [ag2 (reduce (fn [ag stmt] (first (get-statement-node ag stmt)))
                    ag1
                    (conj (vals (:premises arg)) (:conclusion arg)))
        ag3 (reduce (fn [ag src] (update-references ag src))
                    ag2
                    (:sources arg))
        node (make-argument-node 
               :id (:id arg)           
               :title (:title arg)      
               :scheme (:scheme arg) 
               :strict (:strict arg)
               :weight (:weight arg)  
               :conclusion (statement->literal ag3 (:conclusion arg))
               :premises (zipmap (keys (:premises arg)) 
                                 (map (fn [p] (statement->literal ag3 p)) 
                                      (vals (:premises arg)))) 
               :sources (source-ids ag3 (:sources arg)))]
    (-> ag3 
        (assoc :argument-nodes (assoc (:argument-nodes ag3) (:id arg) node))
        (link-conclusion (:conclusion node) (:id arg))
        (link-premises (:vals (:premises arg)) (:id arg)))))

(defn assert-arguments
  "argument-graph (collection-of argument) -> argument-graph"
  [ag args]
  {:pre [(not (nil? ag))]}
  (reduce (fn [ag arg] (assert-argument ag arg)) ag args))

(defn- update-argument-node
  "argument-graph symbol key value ... -> argument-graph
   Updates the argument node with the given id, replacing the properties with
   the given keys values but retaining the values of other properties."
  [ag id & key-values]
  (assoc ag 
         :argument-nodes (assoc (:argument-nodes ag)
                                 id
                                 (merge (get (:argument-nodes ag) id) 
                                        (map hash-map key-values)))))


(defn assoc-standard
  "argument-graph  proof-standard (list-of statement) -> argument-graph
   Assigns the given proof standard to each statement in the list, creating
   statement nodes for statements without nodes in the argument graph."
  [ag ps statements]
  (reduce (fn [ag stmt]
            (let [[ag2 n] (get-statement-node ag stmt)]
              (assoc ag2 :statement-nodes (assoc (:statement-nodes ag2)
                                                 (:id n) 
                                                 (assoc n :standard ps)))))
          ag statements))

(defn get-argument
  "argument-graph symbol -> argument-node | nil"
  [ag id]
  ((:argument-nodes ag) id))

(defn arguments 
  "argument-graph [statement] -> (seq-of argument-node)
   Returns all argument nodes in an argument graph pro and con some statement,
   or all argument nodes in the argument graph, if no statement is provided."
  ([ag stmt]
    (map (fn [arg-id] (get (:argument-nodes ag) arg-id))
         (:conclusion-of (get (:statement-nodes ag)
                              (get (:language ag)
                                   (statement-atom stmt))))))           
  ([ag]
    (if-let [args (vals (:argument-nodes ag))]
      args
      ())))

(defn pro-arguments
  "argument-graph statement -> (seq-of argument-node)"
  [ag s]
  (filter (fn [node]
            (= (statement-pos? s) 
               (:positive (:conclusion node))))
          (arguments ag s)))

(defn con-arguments
  "argument-graph statement -> (seq-of argument)"
  [ag s]
  (pro-arguments ag (statement-complement s)))

(defn schemes-applied
  "argument-graph statement -> (seq-of string)"
  [ag stmt]
  (map :scheme (arguments ag stmt)))

(defn stated?
  [ag s]
  (nil? (:weight (get-statement-node ag s))))

; TO DO: accept, reject, etc. should take a collection of statements, not single statements.

(defn accept 
  "argument-graph statement -> argument-graph"
  [ag s]
  (update-statement-node 
    ag 
    (get-statement-node ag s)
    :weight (if (statement-pos? s) 1.0 0.0)))
   
(defn accepted?
  "argument-graph statement -> boolean"
  [ag s]
  (let [n (get-statement-node ag s)]
    (if (statement-pos? s)
      (= (:weight n) 1.0)
      (= (:weight n) 0.0))))

(defn accepted-statements
  "argument-graph -> (seq-of statement)
   Returns a sequence of the accepted statements in the argument
   graph. If a statement P is rejected in the graph, its complement
   (not P) is accepted and included in the resulting sequence."
  [ag]
  (reduce (fn [s n] (cond (= (:weight n) 1.0) (conj s (:wff n))
                          (= (:weight n) 0.0) (conj s (statement-complement (:wff n)))
                          :else s))
          ()
          (:statement-nodes ag)))

(defn facts
  "argument-graph -> (seq-of statement)
   Returns the accepted statements of the argument graph. A
   synonym for the accepted-statements function."
  [ag]
  (accepted-statements ag))
  
(defn reject 
  "argument-graph statement -> argument-graph"
  [ag s]
  (update-statement-node 
    ag 
    (get-statement-node ag s)
    :weight (if (statement-pos? s) 0.0 1.0)))

(defn rejected?
  "argument-graph statement -> boolean"
  [ag s]
  (let [n (get-statement-node ag s)]
    (if (statement-pos? s)
      (= (:status n) 0.0)
      (= (:status n) 1.0))))

(defn rejected-statements
  "argument-graph -> (seq-of statement)
   Returns a sequence of the rejected statements in the argument
   graph. If a statement P is accepted in the graph, its complement
   (not P) is rejected and included in the resulting sequence."
  [ag]
  (reduce (fn [s n] (cond (= (:weight n) 0.0) (conj s (:wff n))
                          (= (:weight n) 1.0) (conj s (statement-complement (:wff n)))
                          :else s))
          ()
          (:statement-nodes ag)))

(defn assume 
  "argument-graph statement -> argument-graph"
  [ag s]
  (update-statement-node 
    ag 
    (get-statement-node ag s)
    :weight (if (statement-pos? s) 0.75 0.25)))                             
                              
(defn assumed?
  "argument-graph statement -> boolean"
  [ag s]
  (let [x (:weight (get-statement-node ag s))]
    (if (nil? x)
      false
      (if (statement-pos? s)
        (< 0.5 x 1.0)
        (< 0.0 x 0.5)))))

(defn assumptions
  "argument-graph -> (seq-of statement)
   Returns a sequence of the assumptions in the argument
   graph."
  [ag]
  (reduce (fn [s n] (cond (< 0.5 (:weight n) 1.0) (conj s (:wff n))
                          (< 0.0 (:weight n) 0.5) (conj s (statement-complement (:wff n)))
                          :else s))
          ()
          (:statement-nodes ag)))

(defn question
  "argument-graph statement -> argument-graph
   Questions a statement, making it an issue."
  [ag s]
  (update-statement-node 
    ag 
    (get-statement-node ag s)
    :weight 0.5))
  
(defn issue?
  "argument-graph statement -> boolean"
  [ag s]
  (let [x (:weight (get-statement-node ag s))]
    (and (<= x 0.75)
         (>= x 0.25))))

(defn issues
  "argument-graph -> (seq-of statement)
   Returns a sequence of the issues in the argument
   graph. Only positive statements are returned. 
   ¬P is an issue iff P is an issue."
  [ag]
  (reduce (fn [s n] (if (= (:weight n) 0.5) (conj s (:wff n)) s))
          ()
          (:statement-nodes ag)))

(defn atomic-statements 
  "argument-graph -> (seq-of statement)
   Returns a sequence of the atomic statements in the argument graph."
  [ag]
  (map (fn [n] (:statement n))
       (vals (:statement-nodes ag))))


  
  


