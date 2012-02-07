(ns 
  ^{:doc "An implementation of the argument evaluation protocol using the 
          semantics of Carneades Argument Evaluation Structures (CAES)."}
  carneades.engine.aspic
  (:use clojure.pprint
        clojure.set
        carneades.engine.statement
        carneades.engine.argument
        carneades.engine.argument-graph
        carneades.engine.argument-evaluation))

(defrecord ArgumentationFramework
    [arguments     ; symbol -> argument-tree map, where the symbols are argument-tree URNs
     defeats])     ; symbol -> set of symbols, where the symbols are argument-tree URNs

(defrecord ArgumentTree
    [id            ; URN of a statement node, if there are no subtrees, otherwise of an argument node
     conclusion    ; statement node id
     children])    ; sequence of argument tree ids

(defn make-argument-tree
  "key value ... -> argument-tree"
  [& key-values]
  (merge (ArgumentTree. nil []) (apply hash-map key-values)))

(defn argument-tree? [x] (instance? ArgumentTree x))

(defn argument-tree-premises
  "argumentation-framework argument-tree -> set of statement-node ids"
  [af arg]
  {:pre [(argument-tree? arg)]}
  (reduce (fn [s arg2] (union s (argument-tree-premises arg2)))
          #{(:conclusion arg)}
          (:children arg)))

(defn argument-tree-subtrees
  "argumentation-framework argument-tree -> seq of argument-trees"
  [af arg]
  {:pre [(argument-tree? arg)]}
  
  
          



(defn supports
  "argument-graph argument-node -> set of URN symbols
   Returns the set of ids of all the argument nodes supporting an argument node,
   including the id of the argument node itself."
  [ag an]
  