;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.examples.8puzzle
 [:use carneades.engine.search])

(defstruct state :r1c1 :r1c2 :r1c3
                 :r2c1 :r2c2 :r2c3
		 :r3c1 :r3c2 :r3c3)

(defn goal? [s]
  (= s (struct state 1 2 3
                     8 0 4
                     7 6 5)))

; moves are :up :down :right :left

(defn move
  "Apply a move m to a node n to produce the successor node, where a
move is one of :up, :down, :right, or :left."
  [m n]
  (if n
    (let [depth (inc (:depth n)),
	  s (:state n),
	  {a :r1c1 b :r1c2 c :r1c3
	   d :r2c1 e :r2c2 f :r2c3
	   g :r3c1 h :r3c2 i :r3c3} s]
      (cond
       (= m :up)
       (cond
        (zero? d)
        (struct node depth :up n (merge s {:r1c1 0, :r2c1 a}))

        (zero? e)
        (struct node depth :up n (merge s {:r1c2 0, :r2c2 b}))

        (zero? f)
        (struct node depth :up n (merge s {:r1c3 0, :r2c3 c}))

        (zero? g)
        (struct node depth :up n (merge s {:r2c1 0, :r3c1 d}))

        (zero? h)
        (struct node depth :up n (merge s {:r2c2 0, :r3c2 e}))

        (zero? i)
        (struct node depth :up n (merge s {:r2c3 0, :r3c3 f})))

       (= m :down)
       (cond
        (zero? a)
        (struct node depth :down n (merge s {:r1c1 d,  :r2c1 0}))

        (zero? b)
        (struct node depth :down n (merge s {:r1c2 e,  :r2c2 0}))

        (zero? c)
        (struct node depth :down n (merge s {:r1c3 f,  :r2c3 0}))

        (zero? d)
        (struct node depth :down n (merge s {:r2c1 g,  :r3c1 0}))

        (zero? e)
        (struct node depth :down n (merge s {:r2c2 h,  :r3c2 0}))

        (zero? f)
        (struct node depth :down n (merge s {:r2c3 i,  :r3c3 0})))

       (= m :right)
       (cond
        (zero? a)
        (struct node depth :right n (merge s {:r1c1 b,  :r1c2 0}))

        (zero? d)
        (struct node depth :right n (merge s {:r2c1 e,  :r2c2 0}))

        (zero? g)
        (struct node depth :right n (merge s {:r3c1 h,  :r3c2 0}))

        (zero? b)
        (struct node depth :right n (merge s {:r1c2 c,  :r1c3 0}))

        (zero? e)
        (struct node depth :right n (merge s {:r2c2 f,  :r2c3 0}))

        (zero? h)
        (struct node depth :right n (merge s {:r3c2 i,  :r3c3 0})))

       (= m :left)
       (cond
        (zero? b)
        (struct node depth :left n (merge s {:r1c1 0,  :r1c2 a}))
        
        (zero? e)
        (struct node depth :left n (merge s {:r2c1 0,  :r2c2 d}))

        (zero? h)
        (struct node depth :left n (merge s {:r3c1 0,  :r3c2 g}))
        
        (zero? c)
        (struct node depth :left n (merge s {:r1c2 0, :r1c3 b}))
        
        (zero? f)
        (struct node depth :left n (merge s {:r2c2 0, :r2c3 e}))
        
        (zero? i)
        (struct node depth :left n (merge s {:r3c2 0, :r3c3 h})))
       ))))

(defn moves
  "map the node n to a sequence of successor nodes"
  [n]
  (mapcat (fn [m] 
            (let [n2 (move m n)]
              (if n2 (list n2) '())))		 
          [:up :down :right :left]))

(defn show [s]
  (if (empty? s)
    (println "stream empty!")
    (println (path (first s)))))

; base case.  The starting state is a goal state.
(def pr0
  (struct problem
    (make-root (struct state 1 2 3 8 0 4 7 6 5)) 
    moves
    goal?))

; pr5 is named for compatibility with the example in Scheme
(def pr5  
  (struct problem 
          (make-root (struct state 2 8 3 1 6 4 7 0 5))
          moves
          goal?))

(defn main []
  (show (search pr0 breadth-first 1000))
  (show (search pr5 breadth-first 1000)))



