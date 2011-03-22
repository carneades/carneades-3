;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1


(ns carneades.engine.search
  (:use clojure.contrib.pprint))

(defstruct node :depth :label :parent :state)

(defstruct resource :amount)

(defn make-root 
  "Returns the root node of a search space containing the state s."
  [s]
  (struct node 0 nil nil s))

(defn root? 
  "Check whether the node n is the root node."
  [n]
  (not (:parent n)))

(defstruct problem 
  :root  ; node
  :space ; node -> (seq-of node), the children of the node
  :goal) ; state -> boolean

; strategy =  space node -> (seq-of node)
; A strategy generates all of the nodes in a
; space by traversing the space in some way, starting
; at the given node. 

(defn search
  "Search the problem space p, using the strategy s, 
   and returns a sequence of nodes with a state satisfing 
   the goal of the problem. If n is an integer, not nil, 
   then at most n nodes of the space are visited during the search. 
   If n is 0, then all nodes will be visited and the search will 
   not terminate if the sequence is infinite and contains no goal states."
  ([p s n]
     (let [goal? (:goal p)]
       (filter (fn [node]
                 (goal? (:state node)))
	       (if (and n (>= n 0))
		 (take n (s (:space p) (:root p)))
                 (s (:space p) (:root p))))))
  ([p s] (search p s nil)))

(defn traverse
  "Expands the problem space p, using the strategy s,
   and returns a sequence of nodes. If n is an integer, not nil,
   then at most n nodes of the space are visited.
   If n is 0, then all nodes will be visited and the traverse will
   not terminate if the sequence is infinite."
  ([p s n]
    (if (and n (>= n 0))
      (take n (s (:space p) (:root p)))
      (s (:space p) (:root p))))
  ([p s] (search p s nil)))

(defn path
  "Returns a sequence of the labels from the root node the node n."
  [n]
  (if (root? n)
    []
    (conj (path (:parent n))  (:label n))))

(defn depth-first
 "Returns a sequence of all the nodes in the space, starting at the given 
  node, by traversing the space in a depth-first manner."
  [space node]
  (letfn [(expand [open-nodes]
		  (if (empty? open-nodes)
		    []
		    (cons (first open-nodes)
			  (lazy-seq (expand (concat (space (first open-nodes))
						    (rest open-nodes)))))))]
    (expand [node])))

(defn breadth-first
  "Returns a sequence of all the nodes in the space, starting at the given 
   node, by traversing the space in a breadth-first manner."
  [space node]
  (letfn [(expand [open-nodes]
		  (if (empty? open-nodes)
		    []
		    (cons (first open-nodes)
			  (lazy-seq (expand (concat (rest open-nodes)
						    (space (first open-nodes))))))))]
    (expand [node])))

				    
; to do: other search strategies

;; Tests 

(comment

(def root (make-root "0"))

(defn finite-space [n] 
  (let [d (inc (:depth n))]
    (if (<= d 2)  ; max-depth = 2
      (for [i (range 3)] 
	(struct-map node 
	  :depth d 
	  :parent n 
	  :state (str (:state n) \/  i))))))

(defn infinite-space [n]
  (for [i (range 3)] 
    (struct-map node 
      :depth (inc (:depth n))   (make-root (struct state 2 8 3 1 6 4 7 0 5))
      :parent n 
      :state (str (:state n) \/  i))))

(defn goal? [s] 
  (let [goals #{"0/0/0/", "0/1/0", "0/2/2"}]
    (goals s)))

(def p1 (struct problem root finite-space goal?))
(map :state (search p1 depth-first))
(map :state (search p1 breadth-first))

(def p2 (struct problem root infinite-space goal?))
(map :state (search p2 depth-first 100))
(map :state (search p2 breadth-first 20))

)