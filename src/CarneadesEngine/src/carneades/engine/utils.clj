;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.engine.utils
  (:use clojure.contrib.pprint))


(defn boolean? [x]
  (instance? Boolean x))

(defn nonemptyseq? [x]
  "Returns true if x is a nonempty sequence"
  (and (seq? x) (not (empty? x))))

(defn conj-ifnot [pred col x]
  "conjoin col and x if the binary predicate pred is false
   for all values in col "
  (if (some #(pred x %) col)
    col
    (conj col x)))

;; Example:
;;(conj-ifnot premise= (list (pm '(father T D))
;; (pm '(mother Pierre XYZ)))
;; (pm (struct fatom "%s -> %s" '(father T D))))
;; returns:
;; ({:atom (father T D), :polarity true, :role nil, :type
;; :carneades.engine.argument/ordinary-premise}
;; {:atom (mother Pierre XYZ), :polarity true, :role nil,
;; :type :carneades.engine.argument/ordinary-premise})

(defn union-if
  "Returns the union of s1 and s2, (pred x y) is true for each
   (x y) couple in the union"
  ([pred] {})
  ([pred s1] s1)
  ([pred s1 s2]
     (if (< (count s1) (count s2))
       (reduce #(conj-ifnot pred %1 %2) s1 s2)
       (reduce #(conj-ifnot pred %1 %2) s2 s1))))

;; (union-if premise= (list (pm '(father T D))
;; (pm '(mother Pierre XYZ)))
;; (list (pm (struct fatom "x y z" '(father T D) ))))

(defn interleaveall 
  "Returns a lazy seq of the first item in each coll, then the second etc.
   If the sequences do not have the same length, the remaining elements are 
   appended and interleaved at the end of the returned sequence. "
  ([c1] c1)
  ([c1 c2]
     (lazy-seq
      (let [s1 (seq c1) s2 (seq c2)]
        (cond (and s1 s2)
              (cons (first s1) (cons (first s2) 
                                     (interleaveall (rest s1) (rest s2))))
              :else (or s1 s2)))))
  ([c1 c2 & colls]
     (lazy-seq 
      (let [ss (map seq (conj colls c2 c1))]
        (when (some identity ss)
          (concat (map first (filter not-empty ss))
                  (let [notemptyrest (filter not-empty (map rest ss))]
                    (if (empty? notemptyrest)
                      '()
                      (apply interleaveall notemptyrest)))))))))

(defn mapinterleave [f & colls]
  "Returns the result of applying interleaveall to the result of applying map
   to f and colls. If the result of applying map is a sequence with one element
   then the sequence is returned"
  (let [col (apply map f colls)]
    (cond (empty? col) col
          (= (count col) 1) (first col)
          :else (apply interleaveall col))))

;; safe get
(defmacro sget [map key]
  "Like get but if *assert* is true, throws an exception if the key is 
   not present "
  (if *assert*
    `(let [notfound# (gensym)
           v# (get ~map ~key notfound#)]
       (if (= v# notfound#)
         (throw (Exception. (format "Key '%s' not found" ~key)))
         v#))
    `(get ~map ~key)))
