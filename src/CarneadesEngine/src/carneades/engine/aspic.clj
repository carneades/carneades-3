;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns 
  ^{:doc "An implementation of the argument evaluation protocol using the 
          semantics of Henry Prakken's ASPIC+ system.  See:
          
          Prakken, Henry. An abstract framework for argumentation with structured arguments. 
          Argument & Computation 1 (2010): 93-124.
          
          ASPIC uses the term 'premise' to mean statements which are true, or assumed to 
          true, in a knowledge base. To avoid confusion with premises of (single-step) arguments,
          we use the terminology of Carneades argument graphs here:
          
          ASPIC premises -> basis
          ASPIC ordinary premises -> facts
          ASPIC assumptions -> assumptions
          "}
  
  carneades.engine.aspic
  
  (:use clojure.pprint
        clojure.set
        carneades.engine.statement
        carneades.engine.argument
        carneades.engine.argument-graph
        carneades.engine.argument-evaluation))

(defrecord ArgumentationFramework
  [arguments     ; set of strings, where each string is the URN of an argument node
   defeats])     ; set of pairs of strings, [URN1 URN2], where the strings are the URNs of arguments and
                 ; the argument with the id URN1 defeats the argument with the id URN2

(defrecord ArgumentTree
  [subargs       ; set of argument node ids, for the proper subarguments of the argument, 
                 ; not including the argument itself.
   assumptions]) ; set of statement node ids
  
(defn- make-argument-tree
  [& values]
  (let [m (apply hash-map values)]
    (merge (ArgumentTree. 
             #{}    ; subargs
             #{})   ; assumptions
           m)))

; type argument-tree-map = map from argument node id to argument tree

(declare argument-tree) ; the subarguments and argument-tree functions are mutually recursive

(defn- subarguments
  "argument-graph argument-node argument-tree-map -> {:args :map}
   Returns an {:args :map} map, were :args is a set of argument node ids of the 
   proper subarguments of the argument node and :map is the input argument-tree-map
   extended with the argument-trees of these subarguments.
   An argument a2 is a proper subargument of an argument a1 if 
   1. a2 is an argument pro a positive premise of a1, 
   2. a2 is an argument con a negative premise of a1, or, 
   3. recursively, a2 is proper subargument of a proper subargument of a1.
   Arguments pro premises which are facts or assumptions in the knowledge base are ignored. 
   That is, the tree of supporting (sub) arguments is cut off below the facts and assumptions."
  [ag an atm1]
  (reduce (fn [m1 pr] 
            (let [sn (get (:statement-nodes ag) (:statement pr))
                  f (fn [m2 an2] 
                      (let [atm2 (argument-tree ag an2 (:map m2))]
                        {:args (union (:subargs atm2) ; see case 3 above, re subargs of subargs
                                      (conj (:args m2) (:id an2)))
                         :map atm2}))
                  g (fn [id] (get (:argument-nodes ag) id))]
              (cond (or (and (:positive pr)
                             (>= (:weight sn) 0.75))
                        (and (not :positive pr)
                             (<= (:weight sn) 0.25)))
                    ; the premise is a fact or assumed
                    m1, 
                    
                    (:positive pr)
                    (reduce f m1 (map g (:pro sn))),
                    
                    :else  ; negative premise
                    (reduce f m1 (map g (:con sn))))))
          {:args #{} :map atm1}
          (:premises an)))

(defn- direct-assumptions 
  "argument-graph argument-node -> set of literals
   Returns literals for the premises of the argument node which
   are assumptions of the knowledge base."
  [ag an]
  (set (map (fn [pr] (if (:positive pr) 
                       (:statement pr)
                       (literal-complement (:statement pr))))
            (filter (fn [pr] 
                      (let [sn (get (:statement-nodes ag) (:statement pr))]
                        (or (and (:positive pr)
                                 (>= (:weight sn) 0.75)
                                 (< (:weight sn) 1.0))
                            (and (not (:positive pr))
                                 (<= (:weight sn) 0.25)
                                 (> (:weight sn) 0.0)))))
                    (:premises an)))))
                            
(defn- argument-tree
  "argument-graph argument-node argument-tree-map -> argument-tree-map
   Adds the argument tree for the given argument node to the map.
   Returns the map unmodified if an entry for the argument node
   is already in the map."
  [ag an m]
  (if (contains? m an) 
    ; base case to break out of the recursion and avoid infinite loops 
    ; when the argument graph contains support cycles
    m
    (let [{sa :args, m2 :map} (subarguments ag an m),
          assumptions (apply union (conj (map :assumptions (get m2 sa)) 
                                         (direct-assumptions ag an)))]
      (assoc m2 (:id an) (make-argument-tree sa assumptions)))))
   
(defn- argument-trees
  "argument-graph -> (argument node id -> argument tree) map
   Constructs a map of argument trees for all the argument nodes
   in the argument graph."
  [ag]
  (reduce (fn [m an] (argument-tree ag an m))
          {}
          (vals (:argument-nodes ag))))

(defn- undercuts
  "urn urn argument-graph argument-tree-map -> boolean
   Returns true if the argument with id1 undercuts the argument with id2"
  [id1 id2 ag atm]
  ; START HERE
  )
   

(defn- defeaters
  "argument-node-id argument-graph argument-tree map -> set of argument ids
   Returns the ids of the arguments which defeat the input argument."
  [id1 ag atm]
  (reduce (fn [s id2] 
            (if (or (undercuts id2 id1 ag atm)
                    (successfully-rebuts id2 id1 ag atm)
                    (undermines id2 id1 ag atm))
              (conj s id2)
              s))
          #{}
          (keys (:argument-nodes ag))))

(defn make-argumentation-framework
  "argument-graph -> argumentation-framework"
  [ag]
  (let [args (keys (:argument-nodes ag)),
        atm (argument-trees ag)
        defeats (apply union (map (fn [id1] 
                                    (set (map (fn [id2] [id2 id1])
                                         (defeaters id1 ag atm)))) 
                                  args))]
    (ArgumentationFramework. args defeats)))







  