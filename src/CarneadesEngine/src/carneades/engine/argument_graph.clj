;;; Copyright (c) 2011 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.engine.argument-graph
  (:use clojure.pprint
        carneades.engine.uuid
        carneades.engine.statement
        carneades.engine.dublin-core
        carneades.engine.argument
        [carneades.engine.theory :only (format-statement)] ;; for the enter-language function
        )
  (:require [clojure.string :as s]))

; A literal is a propositional letter, represented by a symbol,
; or the negation of a propositional letter.

(defn- make-literal
  [positive letter]
  (if positive letter (list 'not letter)))

(defrecord ArgumentNode
  [id               ; URN symbol
   header           ; nil or dublin core metadata about the argument
   scheme           ; nil or (symbol term ...) form, where the symbol
   ;; is the URI of the scheme and the terms are variables or constants
   ;; which instantiate the variables of the scheme
   strict           ; boolean
   weight           ; 0.0-1.0, default 0.5; input to argument evaluation
   value            ; nil or 0.0-1.0, default nil; output from argument evaluation
   conclusion       ; URN symbol, id of a statement node
   pro              ; boolean, default true; con argument if false
   premises])       ; sequence of premises, where the :statement of each premise is the
                    ; id (URN symbol) of a statement node.

(defn- make-argument-node
  "key value ... -> argument-node"
  [& key-values]
  (let [m (merge (ArgumentNode.
                   nil          ; id
                   nil          ; header
                   ""           ; scheme
                   false        ; strict
                   0.5          ; weight
                   nil          ; value
                   nil          ; conclusion
                   true         ; pro
                   [])          ; premises
                 (apply hash-map key-values))]
    (assoc m :id (if (:id m) (:id m) (make-urn-symbol)))))

(defn argument-node? [x] (instance? ArgumentNode x))

(defn weight
  "The assigned weight of strict arguments is irrelevant.  Strict
   arguments all have the same weight and weigh more than
   defeasible arguments."
  [arg]
  (if (:strict arg) 2.0 (:weight arg)))

(defn proof-standard?
  [k]
  (contains? #{:dv, :pe, :cce, :brd} k))

; type language = :en | :de | :nl | :fr ...

; Note: the atom of a statement node must be kept in sync with the atom associated
; with the id of the statement node in the language table, i.e. the key list.

(defrecord StatementNode
  [id               ; URN symbol, same as the key the statement-nodes table
   atom             ; ground atomic formula or nil
   header           ; nil or Dublin metadata structure about the statement
   weight           ; nil or 0.0-1.0, default nil; input to argument evaluation
   value            ; nil or 0.0-1.0, default nil; outut from argument evaluation
   standard         ; proof-standard
   main             ; boolean, default false; true if the statement is a main issue
   text             ; (language -> string) map, natural language formulations of the statement
   premise-of       ; (set-of symbol), argument node ids
   pro              ; (set-of symbol), pro argument node ids
   con])            ; (set-of symbol), con argument node ids


(defn- make-statement-node
  [stmt]
  {:pre [(literal? stmt)]}
  (let [sn (StatementNode.
             nil   ; id
             nil   ; atom
             nil   ; header
             nil   ; weight
             nil   ; value
             :pe   ; standard
             false ; main
             {}    ; text
             #{}   ; premise-of
             #{}   ; pro
             #{})]  ; con
    (cond (statement? stmt)
          (-> sn
              (merge stmt)
              (dissoc :positive)
              (assoc :id  (or (:id stmt)
                              (if (urn-symbol? (literal-atom stmt))
                                (literal-atom stmt)
                                (make-urn-symbol))))),
          (sliteral? stmt)
          (assoc sn
                 :atom stmt
                 :id (if (urn-symbol? (literal-atom stmt))
                       (literal-atom stmt)
                       (make-urn-symbol))))))


(defn statement-node? [x] (instance? StatementNode x))

(defn statement-node-atom
  [sn]
  (or (:atom sn) (:id sn)))

(defrecord ArgumentGraph
  [header           ; nil or Dublin meta-data description of the model
   language         ; (sexp -> symbol) map, i.e. a "key list";
                    ; where the sexp represents a ground atomic formula
   statement-nodes  ; (symbol -> StatementNode) map, where the symbol is the node's id (URN symbol)
   argument-nodes   ; (symbol -> ArgumentNode) map, where the symbol is the node's id (URN symbol)
   references       ; (string -> Metadata) map, where the string is a citation key
   namespaces])     ; (symbol -> string) map, from symbol to URI

(defn make-argument-graph
   "key value ... -> argument-graph"
   [& key-values]
   (merge (ArgumentGraph.
            nil             ; header
            {}              ; keys
            {}              ; statement nodes
            {}              ; argument nodes
            {}              ; references
            {})             ; namespaces
          (apply hash-map key-values)))

(defn argument-graph? [x] (instance? ArgumentGraph x))

(defn get-argument-node
  "argument-graph symbol -> argument-node | nil"
  [ag id]
  (get (:argument-nodes ag) id))

(defn pro-argument-nodes
  "argument-graph statement-node -> (seq-of argument-node)"
  [ag sn]
  (map (fn [id] (get-argument-node ag id))
       (:pro sn)))

(defn con-argument-nodes
  "argument-graph statement-node -> (seq-of argument-node)"
  [ag sn]
    (map (fn [id] (get-argument-node ag id))
       (:con sn)))

(defn get-statement-node
  "argument-graph statement -> statement-node or nil
   Returns the statement node for a statement, or nil
   if no statement node for the statement exists. Use
   create-statement-node instead to create a statement
   node if one doesn't yet exist."
  [ag stmt]
  {:pre [(argument-graph? ag)]} ; (literal? stmt)]}
  (get (:statement-nodes ag)
       (get (:language ag) (literal-atom stmt))))

(defn- create-statement-node
  "argument-graph statement -> [argument-graph statement-node]
   Returns a [argument-node statement node] pair for the statement,
   creating one if one does not exist in the initial argument-graph."
  [ag stmt]
  {:pre [(argument-graph? ag) (literal? stmt)]}
  (let [n (get (:statement-nodes ag)
               (get (:language ag) (literal-atom stmt)))]
    (if n
      [ag n]
      (let [n2 (make-statement-node stmt)
            ag2 (assoc ag
                       :language
                       (assoc (:language ag)
                              (literal-atom stmt)
                              (:id n2))
                       :statement-nodes
                       (assoc (:statement-nodes ag)
                              (:id n2)
                              n2))]
        [ag2 n2]))))

(defn enter-statement
  "argument-graph statement -> argument-graph
   Adds a statement to an argument graph."
  [ag stmt]
  {:pre [(not (nil? ag))]}
  (first (create-statement-node ag stmt)))

(defn enter-statements
  "argument-graph (collection-of statement) -> argument-graph"
  [ag stmts]
  {:pre [(not (nil? ag))]}
  (reduce (fn [ag stmt] (enter-statement ag stmt)) ag stmts))

(defn update-statement-node
  "argument-graph statement-node key value ... -> argument-graph
   Updates the statement node with the values of the
   properties with the given keys, but retaining the values of other properties.
   Warning: this is a low level function. It does not (yet) keep the atom of the statement
   in sync with its key in the language table."
  [ag node & key-values]
  {:pre [(argument-graph? ag)
         (statement-node? node)]}
  (update-in ag [:statement-nodes (:id node)] merge (apply hash-map key-values)))

(defn enter-language
  "argument-graph language -> argument-graph
   Replace each of the statement's text in the argument graph
   with the formatted text of the form contained in the language"
  ([ag language]
     (enter-language ag language {}))
  ([ag language namespaces]
     (let [build-text (fn [stmt forms selector]
                        (reduce (fn [text lang]
                                  (assoc text lang (format-statement stmt
                                                                     language
                                                                     lang
                                                                     selector
                                                                     namespaces)))
                                {}
                                (keys forms)))]
       (reduce (fn [ag stmt-node]
                 (let [stmt (map->statement stmt-node)
                       individual (language (term-functor stmt))
                       selector (if (literal-pos? stmt) :positive :negative)
                       text (build-text stmt (-> individual :forms) selector)]
                   (update-statement-node ag stmt-node :text text)))
               ag
               (vals (:statement-nodes ag))))))

(defn- get-statement-sliteral
  "argument-graph statement -> sliteral
   Precondition: the atom of the literal has already been entered into the
   language table (key list) of the argument graph."
  [ag stmt]
  {:pre [(not (nil? (get (:language ag) (literal-atom stmt))))]}
  (make-literal (literal-pos? stmt)
                (get (:language ag) (literal-atom stmt))))


(defn- link-conclusion
  "argument-graph argument-node -> argument-graph"
  [ag an]
  (let [sn (get (:statement-nodes ag) (literal-atom (:conclusion an)))]
    (if (:pro an)  ; then conclusion of a pro argument
      (update-statement-node
        ag
        sn
        :pro (conj (:pro sn) (:id an)))
      (update-statement-node
        ag
        sn
        :con (conj (:con sn) (:id an))))))

(defn- link-premises
  [ag1 an]
  (reduce (fn [ag2 p]
            (let [sn (get (:statement-nodes ag2) (:statement p))]
              (update-statement-node
                ag2
                sn
                :premise-of (conj (:premise-of sn) (:id an)))))
          ag1
          (:premises an)))

(defn get-reference
  "argument-graph string -> metadata
   Returns the references with the given key in the
   reference list of the argument graph."
  [ag key]
  (get (:references ag) key))

(defn enter-reference
  "argument-graph metadata -> argument-graph
   Checks whether the metadata is in the list of references of the
   argument graph and updates the list to add the metadata if it
   is new. Does not overwrite or modify existing references.
   If the key of the metadata is nil, it is not added to the list
   and the argument graph is returned unchanged."
  [ag md]
  (let [md2 (get-reference ag (:key md))]
    (if (or (nil? (:key md)) (not (nil? md2)))
      ; the metadata has no key or metadata with the
      ; same key was already in the reference list, so
      ; return the argument graph unchaneged
      ag
      ; else add the new reference to the list
      (assoc ag :references
             (assoc (:references ag) (:key md) md)))))

(defn enter-references
  "argument-graph (collection-of metadata) -> argument-graph"
  [ag metadata-collection]
  (reduce enter-reference ag metadata-collection))

(defn get-namespace-uri
  "argument-graph string -> string or nil"
  [ag prefix]
  {:pre [(argument-graph? ag) (string? prefix)]}
  (get (:namespaces ag) prefix))

(defn enter-namespace
  "argument-graph map -> argument-graph"
  [ag m]
  {:pre [(map? m)]}
  (assoc ag :namespaces
         (assoc (:namespaces ag)
                (:prefix m)
                (:uri m))))

(defn enter-namespaces
  "argument-graph (collection-of maps) -> argument-graph"
  [ag maps]
  (reduce enter-namespace ag maps))

(defn- add-argument-node
  "argument-graph argument-node -> argument-graph"
  [ag node]
  (assoc ag :argument-nodes (assoc (:argument-nodes ag) (:id node) node)))

(defn update-argument-node
  "argument-graph argument-node key value ... -> argument-graph
   Updates the argument node, replacing the properties with
   the given keys values but retaining the values of other properties.
   Warning: this is a low level function. It does not (yet) keep the premises
   and conclusion of the argument in sync with the premise-of and pro or con
   properties of statements in the statement table of the argument graph."
  [ag node & key-values]
  {:pre [(argument-graph? ag)
         (argument-node? node)]}
  (assoc ag
         :argument-nodes (assoc (:argument-nodes ag)
                                (:id node)
                                (merge node
                                       (apply hash-map key-values)))))

(defn enter-argument
  "argument-graph argument -> argument-graph
   Converts a one-step argument to an argument node and adds
   it to the argument graph."
  [ag1 arg]
  {:pre [(argument-graph? ag1)
         (argument? arg)
         (ground? (:conclusion arg))
         (every? (fn [p] (and (premise? p)
                              (ground? (:statement p))))
                 (:premises arg))]}
  ; (pprint {:arg arg})
  (let [ag2 (reduce (fn [ag stmt] (enter-statement ag stmt))
                    ag1
                    (conj (map :statement (:premises arg))
                          (:conclusion arg)))
        node (make-argument-node
               :id (:id arg)
               :header (:header arg)
               :scheme (:scheme arg)
               :strict (:strict arg)
               :weight (:weight arg)
               :conclusion (get-statement-sliteral ag2 (:conclusion arg))
               :pro (:pro arg)
               :premises (map (fn [p] (assoc p
                                         :statement
                                        (get-statement-sliteral ag2 (:statement p))))
                              (:premises arg)))]
    (-> ag2
        (add-argument-node node)
        (link-conclusion node)
        (link-premises node))))

(defn enter-arguments
  "argument-graph (collection-of argument) -> argument-graph"
  [ag args]
  {:pre [(not (nil? ag))]}
  (reduce (fn [ag arg] (enter-argument ag arg)) ag args))


(defn assoc-standard
  "argument-graph  proof-standard (list-of statement) -> argument-graph
   Assigns the given proof standard to each statement in the list, creating
   statement nodes for statements without nodes in the argument graph."
  [ag ps statements]
  (reduce (fn [ag stmt]
            (let [[ag2 n] (create-statement-node ag stmt)]
              (assoc ag2 :statement-nodes (assoc (:statement-nodes ag2)
                                                 (:id n)
                                                 (assoc n :standard ps)))))
          ag statements))


(defn arguments
  "argument-graph statement -> (seq-of argument-node)
   Returns all argument nodes in an argument graph pro and con some statement,
   or all argument nodes in the argument graph, if no statement is provided."
  ([ag stmt]
    (let [sn (get-statement-node ag stmt)]
      (if (nil? sn)
        ()
        (map (fn [arg-id] (get (:argument-nodes ag) arg-id))
             (concat (:pro sn) (:con sn))))))
  ([ag]
    (if-let [args (vals (:argument-nodes ag))]
      args
      ())))

(defn statement-node-arguments
  "argument-graph statement-node -> (seq-of argument-node)"
  [ag sn]
  (map (fn [arg-id] (get (:argument-nodes ag) arg-id))
       (concat (:pro sn) (:con sn))))

(defn pro-arguments
  "argument-graph statement -> (seq-of argument-node)"
  [ag stmt]
  (let [sn (get-statement-node ag stmt)]
    (if (nil? sn)
      ()
      (if (literal-pos? stmt)
        (:pro sn)
        (:con sn)))))

(defn con-arguments
  "argument-graph statement -> (seq-of argument-node)"
  [ag s]
  (pro-arguments ag (literal-complement s)))


(defn undercutters
  "argument-graph argument-node -> (seq-of argument-node)"
  [ag an]
  (let [atom `(~'undercut ~(:id an))
        sn (get-statement-node ag atom)]
    (if (nil? sn)
      ()
      (map (fn [an-id] (get (:argument-nodes ag) an-id))
           (:pro sn)))))

(defn schemes-applied
  "argument-graph statement -> (set-of string)"
  [ag stmt]
  (set (map :scheme (arguments ag stmt))))

(defn stated?
  [ag s]
  (nil? (:weight (get-statement-node ag s))))

(defn accept
  "argument-graph (seq-of statement) -> argument-graph"
  [ag stmts]
  {:pre [(argument-graph? ag)
         (every? literal? stmts)]}
  (reduce (fn [ag2 stmt]
            (let [[ag3 sn] (create-statement-node ag2 stmt)]
              (update-statement-node
               ag3
               sn
               :weight (if (literal-pos? stmt) 1.0 0.0))))
          ag
          stmts))

(defn accepted?
  "argument-graph statement -> boolean"
  [ag s]
  (let [n (get-statement-node ag s)]
    (if (literal-pos? s)
      (= (:weight n) 1.0)
      (= (:weight n) 0.0))))

(defn accepted-statements
  "argument-graph -> (seq-of statement)
   Returns a sequence of the accepted statements in the argument
   graph. If a statement P is rejected in the graph, its complement
   (not P) is accepted and included in the resulting sequence."
  [ag]
  (reduce (fn [s n] (cond (= (:weight n) 1.0) (conj s (statement-node-atom n))
                          (= (:weight n) 0.0) (conj s (literal-complement (statement-node-atom n)))
                          :else s))
          ()
          (vals (:statement-nodes ag))))

(defn facts
  "argument-graph -> (seq-of statement)
   Returns the accepted statements of the argument graph. A
   synonym for the accepted-statements function."
  [ag]
  (accepted-statements ag))

(defn reject
  "argument-graph (seq-of statement) -> argument-graph"
  [ag stmts]
  (reduce (fn [ag2 stmt]
            (let [[ag3 sn] (create-statement-node ag2 stmt)]
              (update-statement-node
                ag3
                sn
                :weight (if (literal-pos? stmt) 0.0 1.0))))
          ag
          stmts))

(defn rejected?
  "argument-graph statement -> boolean"
  [ag s]
  (let [n (get-statement-node ag s)]
    (if (literal-pos? s)
      (= (:status n) 0.0)
      (= (:status n) 1.0))))

(defn rejected-statements
  "argument-graph -> (seq-of statement)
   Returns a sequence of the rejected statements in the argument
   graph. If a statement P is accepted in the graph, its complement
   (not P) is rejected and included in the resulting sequence."
  [ag]
  (reduce (fn [s n] (cond (= (:weight n) 0.0) (conj s (literal-atom n))
                          (= (:weight n) 1.0) (conj s (literal-complement (literal-atom n)))
                          :else s))
          ()
          (:statement-nodes ag)))

(defn question
  "argument-graph (seq-of statement) -> argument-graph"
  [ag stmts]
  (reduce (fn [ag2 stmt]
            (let [[ag3 sn] (create-statement-node ag2 stmt)]
              (update-statement-node
                ag3
                sn
                :weight 0.5)))
          ag
          stmts))

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
   ?P is an issue iff P is an issue."
  [ag]
  (reduce (fn [s n] (if (= (:weight n) 0.5) (conj s (literal-atom n)) s))
          ()
          (:statement-nodes ag)))

(defn assumed?
  "argument-graph statement -> boolean"
  [ag s]
  (let [x (:weight (get-statement-node ag s))]
    (if (nil? x)
      false
      (if (literal-pos? s)
        (< 0.5 x 1.0)
        (< 0.0 x 0.5)))))

(defn assume
  "argument-graph (seq-of statement) -> argument-graph
   Assumes the given statements to be true, unless they have
   been assumed to be false, in which case they are questioned, or
   rejected or accepted, in which case their status is left unchanged."
  [ag stmts]
  {:pre [(argument-graph? ag) (every? literal? stmts)]}
  (reduce (fn [ag2 stmt]
            (if (stated? ag stmt)
              (let [[ag3 sn] (create-statement-node ag2 stmt)]
                (update-statement-node
                  ag3
                  sn
                  :weight (if (literal-pos? stmt) 0.75 0.25)))
              (if (assumed? ag (literal-complement stmt))
                (question ag [stmt])
                ag2)))
          ag
          stmts))

(defn assumptions
  "argument-graph -> (seq-of statement)
   Returns a sequence of the assumptions in the argument graph."
  [ag]
  (reduce (fn [s n] (cond (nil? (:weight n)) s
                          (< 0.5 (:weight n) 1.0) (conj s (statement-node-atom n))
                          (< 0.0 (:weight n) 0.5) (conj s (literal-complement (statement-node-atom n)))
                          :else s))
          ()
          (vals (:statement-nodes ag))))

(defn basis
  "argument-graph -> (seq-of statement)
   Returns a sequence of the facts and assumptions in an argument graph"
  [ag]
  (concat (facts ag) (assumptions ag)))

(defn atomic-statements
  "argument-graph -> (seq-of statement)
   Returns a sequence of the atomic statements in the argument graph."
  [ag]
  (map :atom (vals (:statement-nodes ag))))


(defn reset-node-values
   "argument-graph -> argument-graph
    Resets the values of argument and statement nodes to nil in
    an argument graph, to assure that they are reevaluated."
   [ag]
   {:pre [(argument-graph? ag)]}
   (letfn [(reset-arguments [ag] (reduce (fn [ag an] (update-argument-node ag an :value nil))
                                         ag
                                         (vals (:argument-nodes ag))))
           (reset-statements [ag] (reduce (fn [ag sn] (update-statement-node ag sn :value nil))
                                          ag
                                          (vals (:statement-nodes ag))))]
     (-> ag
         (reset-arguments)
         (reset-statements))))

(defn set-main-issues
  "Sets the statetement to be the main issue of ag"
  [ag stmt]
  (let [main-nodes (doall (filter
                           (fn [s] (= (literal-predicate s) (literal-predicate stmt)))
                           (atomic-statements ag)))]
    (reduce (fn [ag atom] (update-statement-node ag (get-statement-node ag atom) :main true))
            ag
            main-nodes)))

(defn get-main-issues
  [ag]
  (filter #(:main %) (vals (:statement-nodes ag))))
