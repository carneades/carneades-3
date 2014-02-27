;; Copyright (c) 2010 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.


(ns ^{:doc "Defining and searching problem spaces using various strategies: 
            depth-first, breadth-first etc."}
    carneades.engine.search
  (:use clojure.pprint))

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

(defn traverse
  "Expands the problem space p, using the strategy s,
   and returns a sequence of nodes. If n is an integer, not nil,
   then at most n nodes of the space are visited.
   If n is 0, then all nodes will be visited and the traverse will
   not terminate if the sequence is infinite."
  ([p s] (traverse p s nil))
  ([p s n]
    (if (and n (>= n 0))
      (take n (s (:space p) (:root p)))
      (s (:space p) (:root p)))))

(defn search
  "Search the problem space p, using the strategy s, 
   and returns a sequence of nodes with a state satisfing 
   the goal of the problem. If n is an integer, not nil, 
   then at most n nodes of the space are visited during the search. 
   If n is 0, then all nodes will be visited and the search will 
   not terminate if the sequence is infinite and contains no goal states."
  ([p s] (search p s nil))
  ([p s n]
    (let [goal? (:goal p)]
      (filter (fn [node] (goal? (:state node)))
              (traverse p s n)))))

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
