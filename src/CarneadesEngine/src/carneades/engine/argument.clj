;;; Copyright ? 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1


(ns ^{:doc "Function to create an argument graph."}
  carneades.engine.argument
  (:use clojure.test
        clojure.set
        clojure.contrib.def
        clojure.contrib.pprint
        ;clojure.contrib.profile ; for testing
        carneades.engine.utils
        carneades.engine.statement
        carneades.engine.unify
        carneades.engine.proofstandard))

(declare update-statement assert-arguments) ; forward declaration

(defstruct premise
  :atom ; an atomic statement
  :polarity      ; boolean true => positive premise | false => negative premise
  :role ; string, the role of the premise in the argumentation schemed applied
        ; nil if none
  )

;; abbreviations for constructing premises with empty roles

(defn pm
  "statement -> premise
  Constructs a  premise "
  [s]
  (struct premise (statement-atom s) (statement-pos? s) nil))

(defn premise-pos? [p]
  (:polarity p))

(defn premise-neg? [p]
  (not (:polarity p)))

(defn premise-statement [p]
  (if (premise-pos? p)
    (:atom p)
    (statement-complement (:atom p))))

(defn premise-atom [p]
  (:atom p))

(defn premise= [p1 p2]
  (and (= (:polarity p1) (:polarity p2))
       (statement= (:atom p1) (:atom p2))))

(defstruct- argument-struct
  :id ; symbol
  :applicable ; boolean
  :weight ; 0.0..1.0
  :direction ; :pro | :con
  :conclusion ; statement
  :premises ; seq-of premise
  :scheme ; a string describing or naming the scheme applied | nil
  :title ; a string for the title | nil
)

(def *default-weight* 0.5)

(defn argument
  ([id direction conclusion premises]
     (struct argument-struct id false *default-weight* direction conclusion
             premises nil nil))
  ([id direction conclusion premises scheme]
     (struct argument-struct id false *default-weight* direction conclusion
             premises scheme nil))
  ([id applicable weight direction conclusion premises scheme]
     (struct argument-struct id applicable weight direction
             conclusion premises scheme nil))
  ([id applicable weight direction conclusion premises scheme title]
     (struct argument-struct id applicable weight direction
             conclusion premises scheme title)))

(defn argument-id [a] (:id a))

(defn argument-scheme [a] (:scheme a))

(defn argument-direction [a]
  {:post [(not (nil? %))]}
  (:direction a))

(defn argument-conclusion [a] (:conclusion a))

(defn argument-head [a]
   (if (= (:direction a) :pro)
       (:conclusion a)
       (statement-complement (:conclusion a))))

(defn argument-premises [a] (:premises a))

(defn get-premise
  "Returns the premise of arg which has the :atom equals to atom"
  [arg atom]
  (let [pms (:premises arg)
        pms (group-by (fn [pm]
                        (= (:atom pm) atom)) pms)]
    (first (get pms true))))

;; Implicit premises, i.e. enthymemes, may be revealed using the  add-premise
;; function.

(defn pro [id conclusion premises]
  (argument id false *default-weight* :pro conclusion premises nil))

(defn con [id conclusion premises]
  (argument id false *default-weight* :con conclusion premises nil))

(defn assoc-applicability
  "argument boolean -> argument"
  [arg applicability]
  (assoc arg :applicable applicability))

(defmacro defargument
  "Defines an argument with an given id and 
   assigns it to the variable named id with the def method

   Example: (defargument b1 (pro not-property 
                              (pm possession-required)
                              (pm no-possession)
                              (pm foxes-are-wild)))"
  [id definition]
  
  `(def ~id (make-arg ~id ~definition)))

(defmacro make-arg
  "Like defargument but does not assign the created argument to anything,
   just returns it."
  ([id definition]
     (letfn [(prefixns
           [x sq]
           (let [f (first sq)
                 r (rest sq)]
             (cons (symbol (str x "/" f)) r)))]
    (let [cns (namespace ::here)
          dir (first definition) ;; direction : pro or con function
          nsdir (symbol (str cns "/" dir))
          conclusion (second definition)
          premises (map #(prefixns cns %) (drop 2 definition))]
      `(~nsdir (quote ~id) ~conclusion (list ~@premises)))))
  ([definition]
     `(arg ~(gensym "a") ~definition)))

;; use (prn-str object) and (read-string s) to read/write argument
;; structures

(defn argument-variables
  "argument -> (seq-of symbol)

   Returns a seq containing the variables of the argument arg"
  [arg]
  (distinct (concat (mapcat #(variables (:atom %)) (:premises arg))
                    (variables (:conclusion arg)))))

(defn instantiate-argument
  "argument substitutions -> argument
   Instantiate the variables of an argument by applying substitions"
  [arg subs]
  (assoc arg
    :id (gensym "a")
    :premises (map #(update-in % [:atom] (fn [a] (apply-substitution subs a))) (:premises arg))
    :conclusion (apply-substitution subs (:conclusion arg))))

;; (defn add-premise [arg p]
;;   (assoc arg :applicable false :premises (cons p (:premises arg))))

;;                  dv        ; dialectical validity
;;                  pe        ; preponderance of the evidence
;;                  cce       ; clear and convincing evidence
;;                  brd       ; beyond a reasonable doubt
(defvar *default-proof-standard* :pe)

(defstruct- node-struct
  :statement ; statement
  :status ; status
  :standard ; proof standard
  :acceptable ; boolean
  :complement-acceptable ; boolean
  :premise-of ; (set-of symbol) where each symbol is an argument id
  :conclusion-of ; (set-of symbol) where each symbol is an argument id
  )

(defn node
  "Builds a new node from a statement with no pro- or con-arguments"
  [s]
  (struct node-struct
          (statement-atom s)
          :stated
          *default-proof-standard*
          false
          false
          #{}
          #{}))

(defn nodes
  "(seq-of statement) -> node table

   Builds a node-table from a list of statements "
  [s]
  ;; this function is not used!
  ; take care here, = is used by the map, not statement=, to
  ; order element
  (reduce (fn [nodes x]
            (let [sym (statement-symbol x)]
              (assoc-in nodes [sym x] (node x))))
          {} s))

(defn node-statement [n]
  (:statement n))

(defstruct- argument-graph-struct
  :id ; symbol
  :title ; string
  :main-issue ; statement | nil
  :nodes ; map: symbol -> statement -> node
  :arguments ; map: argument-id -> argument
  )

(defn argument-graph
  ([]
     (struct argument-graph-struct (gensym "ag") "" nil {} {}))
  ([exprs]
     "converts a collection of arguments into an argument 
      graph"
     (assert-arguments (argument-graph) exprs))
  ([id title main-issue]
     (struct argument-graph-struct id title main-issue {} {}))
  ([id title main-issue nodes]
     (struct argument-graph-struct id title main-issue nodes {}))
  ([id title main-issue nodes arguments]
     (struct argument-graph-struct id title main-issue nodes arguments)))

(defvar *empty-argument-graph* (argument-graph))

(defn statement-node
  "Returns the node of s if it exists, nil otherwise"
  [ag s]
  (get-in ag
          [:nodes (statement-symbol (statement-atom s))
           (statement-atom s)]))

(defn get-node
  "argument-graph statement -> node"
  [ag s]
  (if-let [n (statement-node ag s)]
    n
    (node s)))

(defn status
  "argument-graph statement -> status"
  [ag s]
  (let [n (get-node ag s)
        st (:status n)]
    (if (statement-pos? s)
      st
      (condp = st
        :stated :stated
        :questioned :questioned
        :accepted :rejected
        :rejected :accepted
        :unstated))))

(defn proof-standard [ag s]
  (:standard (get-node ag s)))

(defn- prior [a1 a2]
  ; this function is not used!
  (> (:weight a1)
     (:weight a2)))

(defn add-node
  "argument-graph node -> argument-graph 

   Add a node to the nodes table of an argument graph and replace 
   the nodes table of the argument graph with this new table "
  [ag n]
  {:pre [(not (nil? ag))]}
  (assoc-in ag [:nodes (statement-symbol (:statement n)) (:statement n)] n))

(defn- node-in?
 "argument-graph node boolean -> boolean"
 [n positive]
  (if positive
    (or (= (:status n) :accepted)
        (:acceptable n))
    (or (= (:status n) :rejected)
        (:complement-acceptable n))))

(defn in?
  "argument-graph statement -> boolean 
   looks up the cached 'in' status of the statement in the argument graph"
  [ag s]
  {:pre [(not (nil? ag))]}
  (let [n (get-node ag s)]
    (node-in? n (statement-pos? s))))

(defn out? [ag s]
  (not (in? ag s)))

(defn- holds?
  "argument-graph premise -> boolean"
  [ag p]
  (let [n (get-node ag (:atom p))]
    (condp = (:status n)
      :accepted (premise-pos? p)
      :rejected (premise-neg? p)
      :questioned (in? ag (premise-statement p))
      :stated (in? ag (premise-statement p))
      :unstated false
      false)))

(defn- all-premises-hold?
  "argument-graph argument -> boolean"
  [ag arg]
  (every? #(holds? ag %) (:premises arg)))

(defn state
  "argument-graph (seq-of statement) -> argument-graph

   Changes, non-destructively, the status of each statement in the list to 
   stated in the argument graph.  Statements in the list which do not
   have a node in the argument graph are ignored. "
  [ag statements]
  (reduce #(update-statement %1 %2 :stated) ag statements))

(defn question
  "argument-graph (seq-of statement) -> argument-graph"
  [ag statements]
  (reduce #(update-statement %1 %2 :questioned) ag statements))

(defn accept
  "argument-graph (collection-of statement) -> argument-graph"
  [ag statements]
  (reduce #(update-statement %1 %2 (if (statement-pos? %2)
                                     :accepted
                                     :rejected)) ag statements))

(defn reject [ag statements]
  (reduce #(update-statement %1 %2 (if (statement-pos? %2)
                                     :rejected
                                     :accepted)) ag statements))

(defn assoc-standard
  "argument-graph  proof-standard (list-of statement) -> argument-graph"
  [ag ps statements]
  (reduce (fn [ag s]
            (let [n (get-node ag s)]
              (update-statement (add-node ag (assoc n :standard ps))
                                s
                                (:status n))))
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
  " Returns all arguments pro and con of some statement in an argument graph,
     or all arguments in the argument graph, if no statement is provided
     argument-graph [statement] -> (seq-of argument)"
   ([ag s]
      (if-let [n (get-node ag s)]
        (get-arguments ag (seq (:conclusion-of n)))
        ()))
   ([ag]
      (if-let [args (vals (:arguments ag))]
        args
        ())))

(defn pro-arguments
  "argument-graph statement  -> (seq-of argument)"
  [ag s]
  (let [args (arguments ag s)]
    (if (statement-pos? s)
      (filter #(= (:direction %) :pro) args)
      (filter #(= (:direction %) :con) args))))

(defn con-arguments
  "argument-graph statement -> (seq-of argument)"
  [ag s]
  (pro-arguments ag (statement-complement s)))

(defn schemes-applied
  "argument-graph statement -> (seq-of symbol)"
  [ag s]
  (let [n (get-node ag s)]
    (map (fn [x]
           (:scheme x)) (get-arguments ag (:conclusion-of n)) )))

(defn accepted?
  "argument-graph statement -> boolean"
  [ag s]
  (let [n (get-node ag s)]
    (if (statement-pos? s)
      (= (:status n) :accepted)
      (= (:status n) :rejected))))

(defn rejected?
  "argument-graph statement -> boolean"
  [ag s]
  (let [n (get-node ag s)]
    (if (statement-pos? s)
      (= (:status n) :rejected)
      (= (:status n) :accepted))))

(defn assumed?  ; was called "decided?"
  "argument-graph statement -> boolean"
  [ag s]
  (or (accepted? ag s) (rejected? ag s)))

(defn questioned?
  "argument-graph statement -> boolean"
  [ag s]
  (let [n (get-node ag s)]
    (= (:status n) :questioned)))

(defn stated? [ag s]
  (let [n (get-node ag s)]
    (= (:status n) :stated)))

(defn issue?
  "argument-graph statement -> boolean
   
   An statement is an issue if it has not been accepted (assumed true) or rejected 
   (assumed false) in the argument graph. An acceptable statement is still an issue, 
   due to nonmonotonicity: Additional arguments may make the statement unacceptable again."
  [ag s]
   (not (assumed? ag s)))

(defn all-premises
  "all-premises: argument-graph statement -> (list-of premise)

  Returns the set of all the premises of all arguments pro or con the 
  statement s in the argument graph ag. The set of premises is represented
  as a list."
  [ag s]
 (reduce #(union-if premise= (:premises %2) %1)  '() (arguments ag s)))

(defn check-acceptability
  "argument-graph statement -> boolean"
  [ag s]
  (let [ps (proof-standard ag s)]
    (satisfies-proof-standard? ag ps (pro-arguments ag s) (con-arguments ag s)
                all-premises-hold?)))

(defn acceptable?
  "argument-graph statement -> boolean"
  [ag s]
  (let [n (get-node ag s)]
    (if (statement-pos? s)
      (:acceptable n)
      (:complement-acceptable n))))

(defn applicable?
  "argument-graph argument -> boolean

   Assumes that the applicability of the argument has been 
   previously computed or updated"
  [ag arg]
  {:post [(not (nil? %))]}
  (:applicable (get-argument ag (:id arg))))

(defn depends-on?
  "argument-graph statement statement -> boolean

   s1 depends on s2 in ag if s1 equals s2 or, recursively, some premise 
   of some argument pro or con s1 depends on s2 in ag."
  [ag s1 s2]
  (or (statement= s1 s2)
      (some #(depends-on? ag (:atom %) s2) (all-premises ag s1))))

(defn cycle-free?
  "argument-graph argument -> boolean

  (cycle-free arg ag) checks whether an argument arg will not introduce a cycle
  into the argument graph ag.  An argument will not introduce a cycle if
  none of its premises depend on its conclusion in ag."
  [ag arg]
  (not-any? #(depends-on? ag (:atom %) (:conclusion arg)) (:premises arg)))

(defn relevant?
  "argument-graph statement statement -> boolean

  A sentence s is relevant for proving a 
  goal sentence g in ag if g depends on s in ag"
  [ag s g]
  (depends-on? ag g s))

(defn get-nodes
  "argument-graph [symbol] -> (seq-of node)
  
  returns the list of all statement nodes in the argument graph, or all
  statement nodes with the given predicate, if one is provided."
  ([ag]
     (mapcat #(vals %) (vals (:nodes ag))))
  ([ag predicate]
     ;; predicate is the symbol used as a key for statements
     (vals (get-in ag [:nodes predicate]))))

(defn in-node-statements
  "argument-graph node -> (seq-of statement)

   For a node whose statement is P, returns a list of the members of
   '(P (not P)) which are in."
  [ag n]
  (filter identity (list (if (node-in? n true)
                           (:statement n))
                         (if (node-in? n false)
                           (statement-complement (statement-atom (:statement n)))))))

(defn in-statements
  "argument-graph [symbol] -> (list-of statement)
 
  returns the list of all the in statements (literals) in the argument graph,
  or all in statements with the given predicate, if one is provided.
  Notice that for some atomic statement P, both P and (not P) may be members
  of the list of statements returned, since if a weak proof standard is assigned
  to P, both P and (not P) may be in."
 ([ag]
    (mapcat #(in-node-statements ag %) (get-nodes ag)))
 ([ag predicate]
    (mapcat #(in-node-statements ag %) (get-nodes ag predicate))))

(defn matching-in-statements 
  "argument-graph statement -> [statement]
   Returns a sequence of all the *in* statements in the argument graph matching the query."
  [ag query]
  (filter (fn [s] (unify s query)) (in-statements ag)))
 
(defn relevant-statements
  "argument-graph statement -> (seq-of statement)

  Finds the statements in ag which are 
  relevant for proving g. Since only nodes in the graph are 
  considered, g itself will be a member of the resulting list
  only if it has a node in the argument graph."
  [ag g]
 (filter #(relevant? ag % g) (map #(:statement %) (get-nodes ag))))

(defn update-argument
  "argument-graph argument -> argument-graph 

   updates the applicability of an argument in an argument graph and propogates 
   this change by updating the argument graph for the conclusion
   of the argument. "
  [ag arg]
  {:pre [(not (nil? (:id arg)))]}
  (let [old-applicability (:applicable arg)
        new-applicability (all-premises-hold? ag arg)
        ag2 (assoc-in ag [:arguments (:id arg)]
                      (assoc-applicability arg new-applicability))]
    (if (= old-applicability new-applicability)
      ag2
      ;; update the applicability of the new argument and propogate the change
      ;; by updating the conclusion of the argument
      (update-statement ag2 (:conclusion arg)))))

(defn- update-or-create-premise-nodes
  "update or create nodes for each of the premises of the argument"
  [ag1 arg]
  (reduce (fn [ag p]
            (let [n (get-node ag (:atom p))]
              ; (println "premise: " (:atom p))
              (add-node ag
                        (assoc n :premise-of
                               (union #{(:id arg)} 
                                      (:premise-of n))))))
          ag1 
          (:premises arg)))

(defn assert-argument
  "argument-graph argument -> argument-graph

   Add the argument, arg, to the argument graph, ag,
   if doing so would not introduce a cycle. If some argument with
   the same id exists in the argument graph, then the existing argument is 
   replaced by the new argument. A node for the conclusion of the argument is 
   added to the node table of the argument graph if one does not yet exist. 
   It is the responsiblity of the protocol to question the conclusion of the 
   argument, if this is wanted. The \"applicable\" field of the argument is 
   changed to false before it is updated toassure the the acceptabiity of its 
   conclusion is checked if the argument is in fact applicable."
  [ag arg]
  {:pre [(not (nil? ag))]}
  
  (if (not (cycle-free? ag arg))
    ag
    (let [n (get-node ag (:conclusion arg))]
          (-> ag
          ;; update the node for the conclusion of the argument
          ;; :acceptable and :complement-acceptable are updated below
            (add-node (assoc n :conclusion-of (union #{(:id arg)} (:conclusion-of n))))
            ; (question (list (:conclusion arg)))
            (update-or-create-premise-nodes arg)
            (update-argument (assoc arg :applicable false))))))

(defn assert-arguments
  "argument-graph (collection-of argument) -> argument-graph

   asserts a list of arguments"
  [ag args]
  {:pre [(not (nil? ag))]}
  (reduce (fn [ag arg] (assert-argument ag arg)) ag args))

(defn update-statement
  "argument-graph statement (or symbol nil) -> argument-graph

   updates the nodes for the given statement by changing the status of the 
   statment to new-status and recomputing the acceptability of the statement
   and its complement. If the \"in\" status of the statement
   of the node, or the complement of the statement, is affected, the change
   is propogated forwards by updating the arguments in which the statement 
   is used as a premise, which, recursively, updates the conclusions of these 
   arguments. "
  ([ag s]
     (update-statement ag s nil))
  ([ag s new-status]
     (let [n1 (get-node ag s)
           old-status (:status n1)
           n2 (assoc n1
                :status (or new-status old-status)
                :acceptable (check-acceptability ag (statement-atom s))
                :complement-acceptable
                (check-acceptability ag
                                     (statement-complement (statement-atom s))))
           ag2 (add-node ag n2)]
       (if (and (= (node-in? n1 true) (node-in? n2 true))
                (= (node-in? n1 false) (node-in? n2 false))
                (= new-status old-status))
         ;; then the "in" status of the statement hasn't changed and there's no
         ;; need to propogate further
         ag2
         ;; else propogate the update to the arguments in which the statement
         ;; is a premise
         (reduce (fn [ag id]
                   (if-let [arg (get-argument ag id)]
                     (update-argument ag arg)
                     ag))
                 ag2 (:premise-of n1))))))

(defn- has-premise?
  [p prs]
  (and
    (not (empty? prs))
    (or
      (statement= (:atom p) (:atom (first prs)))
      (recur p (rest prs)))))

(defn- premises=?
  [pr1 pr2]
  (every? (fn [p] (has-premise? p pr2)) pr1))

(defn- unite-args
  [ag arg]
  (if (some (fn [arg2]
              (and
                 (= (:scheme arg) (:scheme arg2))
                 (= (:direction arg) (:direction arg2))
                 (statement= (:conclusion arg) (:conclusion arg2))
                 (premises=? (:premises arg) (:premises arg2))))
        (vals (:arguments ag)))
    ag
    (assert-argument ag (assoc arg :id (gensym "a")))))

(defn- unite-graphs
  [ag1 ag2]
  ;(print "uniting graphs:" (:id ag1) (:id ag2))
  (let [all-nodes (get-nodes ag2),
        accepted-nodes (filter (fn [n] (= (:status n) :accepted)) all-nodes),
        rejected-nodes (filter (fn [n] (= (:status n) :rejected)) all-nodes),
        questioned-nodes (filter (fn [n] (= (:status n) :questioned)) all-nodes),
        stated-nodes (filter (fn [n] (= (:status n) :stated)) all-nodes),]
    ;(println " - " (count all-nodes))
    (-> (reduce unite-args ag1 (arguments ag2))
        (accept (map :statement accepted-nodes))
        (reject (map :statement rejected-nodes))
        (question (map :statement questioned-nodes))
        (state (map :statement stated-nodes)))))

(defn unite-argument-graphs
  [l]
  ; (println "uniting argument-graphs:" (count l))
  (let [r (assoc (reduce unite-graphs *empty-argument-graph* l) :id (gensym "a"))]
    ; (println "united graph:" r)
    r))

(defn depth-in
  [ag n]
  (let [parent-args (:premise-of n),
        parent-stmts (map (fn [aid] (:conclusion (get-argument ag aid))) parent-args)]
    (if (empty? parent-stmts)
      0
      (+ 1 (apply min (map (fn [s] (depth-in ag (get-node ag s))) parent-stmts))))))

(defn height-in
  [ag n]
  (let [child-args (:conclusion-of n),
        child-stmts (apply concat 
                           (map (fn [aid] 
                                  (map :atom (:premises (get-argument ag aid)))) 
                                child-args))]
    ;(println "child-stmts" (:statement n) child-stmts)
    (if (empty? child-stmts)
      0
      (+ 1 (apply max (map (fn [s] (height-in ag (get-node ag s))) child-stmts))))))


(defn graph-depth
  [ag]
  (let [main-issues (filter (fn [n] (empty? (:premise-of n))) (get-nodes ag))]
    (apply max (map (fn [n] (height-in ag n)) main-issues))))

         

