
(ns carneades.engine.atms
  ; (:require )
  (:use clojure.set)
  (:use clojure.contrib.set)
  ;(:import )
  )

;; This is a purely functional implementation of de Kleer's ATMS
;; based on Ulrich Junker's more imperative implementation in Standard ML.


(defstruct atms :nodes) ; datum -> node map

(declare make-formula-node add-formula-node)

(defn make-atms
  "Makes an ATMS, initialized with a node for :bottom."
  []
  (add-formula-node (struct atms {}) (make-formula-node :bottom)))


; type environment = set of datum

(defn nogoods
  "Returns the label of the bottom node in an ATMS."
  [atms]
  (:label (:bottom (:nodes atms))))
 
(defn nogood?
  "Check whether an environment e is a member of the nogoods of an ATMS."
  [atms e]
  (contains? (nogoods atms) e))

(defn inconsistent?
  "An environment e is inconsistent if it is a superset of some nogood set
   of the ATMS."
  [atms e]
  (some (fn [nogood] (superset? e nogood)) (nogoods atms)))

;; Label

; type label = set of environments

; empty-label = #{}
; singleton-label e = #{e}

(defn minimize-label
  "Remove all supersets of the environment e from  the label l."
  [l e]
  (cond
    (empty? l)
    #{}

    (proper-superset? (first l) e)
    (recur (rest l) e) ; then remove the first enviornment

    :else
    (conj (minimize-label (rest l) e) (first l))))

(defn implicit-member?
  "An environment e is an implicit member of a label l if e is a superset
   of some environment in l."
  [e l]
  (some (fn [e2] (superset? e e2)) l))

(defn cross-singleton [e l1 l2]
  "cross-singleton: environment label label -> label"
  (if (empty? l1)
    l2
    (minimize-label  
      (conj #{(union e (first l1))}
         (cross-singleton e (rest l1) l2))
      e)))

(defn cross2
  [l1 l2]
  (if (empty? l1)
    #{}
    (cross-singleton (first l1) l2 (cross2 (rest l1) l2))))

(defn cross
  "cross: set of labels -> label"
  [labels] ; set of labels
  (cond
    (empty? labels) #{}
    (= (count labels) 1) (first labels)
    :else (cross2 (first labels) (cross (rest labels)))))

(defn purge-label
  "Return a label l, removing all environments of the label
   which are inconsistent in the ATMS."
  [atms l]
  (set (for [e l :when (not (inconsistent? atms e))] e)))


;; Node: Formula and Justification

; A formula node is a premise of its successor nodes and
; a conclusion of its predecessor nodes.
(defstruct formula-node
  :datum        ; some representation of the formula
  :label        ; set of environments
  :predecessors ; set of justification nodes
  :successors   ; set of justification nodes
)

(defn make-formula-node
  "Make a formula node for a wff."
  [wff]
  (struct-map formula-node 
    :datum wff
    :label #{}
    :predecessors #{}
    :successors #{}))

(defn add-formula-node
  "Add a formula-node n to an ATMS. If a node already exists for the
   same datum, it is replaced."
  [atms n]
  (merge atms {:nodes (assoc (:nodes atms) (:datum n) n)}))

(defn get-formula-node
  "Get the formula node for the given wff (datum) in the ATMS, creating
   a new node if one does not already exist.  The ATMS is not modified."
  [atms wff]
  (or (get (:nodes atms) wff)
    (make-formula-node wff)))


(defstruct justification-node
  :antecedents ; set of formulas (not nodes)
  :consequent  ; formula (not node)
)

(defn eq-formula-nodes? [n1 n2]
  "Two formula nodes are eq if their data are equal."
  (= (:datum n1) (:datum n2)))

(defn link-consequent
  "Link a justification node j to the predecessors of formula node f in
   an ATMS and return the extended ATMS."
  [atms f j]
  (merge atms
    {:nodes (assoc (:nodes atms)
              (:datum f)
              (merge f {:predecessors (conj (:predecessors f) j)}))}))

(defn link-antecedent
  "Link a justification node j to the successors of a formula node f in
   an ATMS and return the extended ATMS."
  [atms f j]
  (merge atms
    {:nodes (assoc (:nodes atms)
              (:datum f)
              (merge f {:successors (conj (:successors f) j)}))}))

(defn link-antecedents
  "In an ATMS, link a justification node j to the successors of each
   formula node in l and returns the extended ATMS."
  [atms l j]
  (reduce (fn [atms f] (link-antecedent atms f j))
    (cons atms l)))

(defn link-justification
  "Links the antecedent nodes and consequent nodes of a justification j in
   an ATMS, returning the extended ATMS."
  [atms j]
  (let [atms2 (link-consequent atms (get (:nodes atms) (:consequent j)) j)]
    (link-antecedents
      atms2
      (map (fn [wff] (get (:nodes atms2) wff)) (:antecedents j))
      j)))

(defn contradiction?
  "Checks whether a formula node f is the bottom node."
  [f]
  (= (:datum f) :bottom))

;; Label Management System (LMS)

(defn update-label
  "Replace the label of node n with the given label in a ATMS and return
   the modified ATMS."
  [atms n new-label]
  (merge atms {:nodes (assoc (:nodes atms)
                             (:datum n)
                             (merge n {:label new-label}))}))

(defn purge-node-label
  "Purge the label of a node n and update the ATMS by replacing the
   label with the purged label. Returns the updated ATMS."
   [atms n]
  (update-label atms n (purge-label atms (:label n))))

; Skipped definitions of "LMS.presume and LMS.assume in Ulrich's implementation.
; These functions each have one line defintiions and were both each used
; only once,so the calls to these functions have been replaced by their
; definitions here.

(defn extend-label
  "Adds a set of environments, delta, to the label of a node in an ATMS
   and return a {:atms x :delta y} map, where x is the extended ATMS and y is
   the difference between the new and old label of the node."
  [atms n delta]
  (let [old-label (:label n)
        new-delta (if (= :bottom (:datum n))
                    delta
                    (purge-label atms delta))
        new-label (reduce minimize-label
                    (cons (union new-delta old-label) new-delta))
        x (update-label atms n new-label)
        y (difference new-label old-label)]
    {:atms x :delta y}))

(defn purge-justification
  "Purge the label of each antecedent node of a justification j in an ATMS.
   Return the modified ATMS."
  [atms j]
  (reduce (fn [atms2 wff]
            (purge-node-label
              atms2
              (get (:nodes atms2) wff)))
          (cons atms (:antecedents j))))

(defn justification-label
  "Returns the label of a justification node."
  [atms j]
  (cross (set (map (fn [wff] (:label (get (:nodes atms) wff)))
                (:antecedents j)))))

(defn justification-delta-label
  [atms j n delta]
  (purge-label (cross (set (map (fn [n2]
                                  (if (eq-formula-nodes? n n2)
                                    delta
                                    (:label n)))
                             (:predecessors j))))))

(defn nogood
  "Add an environment e to the set of nogoods of an ATMS."
  [atms e]
  (:atms (extend-label atms (get (:nodes atms) :bottom) #{e})))

(defn add-nogoods
  "Add each nogood in delta to the nogoods of the ATMS."
  [atms delta]
  (reduce nogood (cons atms delta)))

;; ATMS main functions

(defstruct item
  :node  ; formula node
  :delta ; set of environments
  )

(defn adjoin-item
  "Add an item to the queue.  If there is an item in the queue already
   for the node of the item, replace the delta of the existing item with
   the union of the previous delta and the new delta."
  [queue item]
  (cond 
    (empty? queue) (list item)
    (empty? (:delta item)) queue

    (eq-formula-nodes? (:node item) (:node (first queue)))
    (cons {:node (:node item)
           :delta (union (:delta item)
                         (:delta (first queue)))}
          (rest queue))

    :else
    (cons (first queue) (adjoin-item (rest queue) item))))

(defn adjoin-items
  "Adjoin each of a list of items to the queue."
  [queue items]
  (reduce adjoin-item (cons queue items)))

(defn update-formula
  "Updates the environment of a formula, fnode, in the ATMS with
   the environments in delta and returns a {:atms :items} pair,
   where :atms is the updated ATMS and :items is a sequence
   of items, i.e. {:node :delta} structures."
  [atms fnode delta]
  (if (contradiction? fnode)
    {:atms (add-nogoods atms delta) :items []}
    (let [{atms2 :atms delta2 :delta} (extend-label atms fnode delta)
          items (if (empty? delta2)  ; not changed
                  [] ; empty sequence of items
                  (map (fn [j]
                         (struct-map item
                           :node (:consequent j)
                           :delta (justification-delta-label
                                    atms
                                    j
                                    fnode
                                    delta2)))
                    (:sucessors fnode)))]
      {:atms atms2 :items items})))

; update-items: called "update" in Ulrich's implementation
(defn update-items
  "Update the atms with each of the items in the queue."
  [atms queue]
  (if (empty? queue)
    atms
    (let [{fnode :node delta :delta} (first queue)
          {atms2 :atms items :items} (update-formula atms fnode delta)]
      (recur atms2 (adjoin-items items (rest queue))))))

; update-atms: called "start-update" in Ulrich's implementation
(defn update-atms
  "Update the ATMS for the fnode formula node with the environments in
   the label delta."
  [atms fnode delta]
  (if (empty? delta)
    atms
    (update-items atms [(struct-map item :node fnode :delta delta)])))

(defn assume
  "Make the datum assumable in the ATMS, update the ATMS and then
   return the updated ATMS."
  [atms datum]
  (let [node (get (:nodes atms) datum (make-formula-node datum))
        atms2 (if (get (:nodes atms) datum) atms (add-formula-node atms node))]
    (update-atms atms2 node #{#{datum}})))

(defn presume
   "Make the datum a presumption, i.e. fact, in the ATMS,
    update the ATMS and then return the updated ATMS."
  [atms datum]
  (let [node (get (:nodes atms) datum (make-formula-node datum))
        atms2 (if (get (:nodes atms) datum) atms (add-formula-node atms node))]
    (update-atms atms2 node #{#{}})))

(defn justify
  "Make a justification and add it to an ATMS.
Update the ATMS and return the updated ATMS."
  [atms antecedents consequent]
  (if (empty? antecedents)
    (presume atms consequent)
    (let [anodes (map (fn [wff] (get-formula-node atms wff))
                   antecedents)
          cnode (get-formula-node atms consequent)
          j (struct justification-node antecedents consequent)
          atms2 (reduce add-formula-node (cons atms (cons cnode anodes)))
          atms3 (link-justification atms2 j)]
      (update-atms atms3 cnode (justification-label atms3 j)))))

; inconsistent? is defined above

(defn derivable?
  "Check whether the datum is derivable from the assumptions in the
   environment, given the justifications in the ATMS."
  [atms env datum]
  (let [node (get (:nodes atms) datum)]
    (and node
       (implicit-member? env (:label node)))))

; nogoods is defined above

(defn supports 
  "Return a set of the environments from which the datum can be
   derived, given the justifications in the ATMS."
  [atms datum]
  (let [node (get (:nodes atms) datum)]
    (if (not node)
        #{}
        (:label node))))

;; TESTS

(def atms1 (make-atms))
(def atms1 (assume atms1 :normal))
(def atms1 (assume atms1 :bird))
; (def atms1 (justify atms1 #{:bird :normal} :flies))

;(def atms1 (assume atms1 :penguin))
;(def atms1 (nogood atms1 #{:normal :not-normal}))
;(def atms1 (justify atms1 #{:penguin} :not-normal))

(supports atms1 :flies)
