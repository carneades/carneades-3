;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.engine.utils
  (:use clojure.java.io
        clojure.contrib.pprint)
  (:require [clojure.contrib.string :as str]))


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

(defn conjoin [f & fs]
  "returns a predicate that returns true when all of the predicates return true.

   Translated from the book Ansi Common Lisp, Prentice Hall, Paul Graham"
  (if (empty? fs)
    f
    (let [conjoined (apply conjoin fs)]
      (fn [& args]
        (and (apply f args) (apply conjoined args))))))

(defn disjoin [f & fs]
  "returns a predicate that returns true when one of the predicates return true.

   Translated from the book Ansi Common Lisp, Prentice Hall, Paul Graham"
  (if (empty? fs)
    f
    (let [conjoined (apply conjoin fs)]
      (fn [& args]
        (or (apply f args) (apply conjoined args))))))

(defn nilify [f]
  "returns a predicate that returns true when f returns true, nil otherwise"
  (fn [& args]
    (or (apply f args) nil)))

;; from
;; http://bitumenframework.blogspot.com/2010/10/stack-traces-for-clojure-app.html

(defn get-stack-trace
  ([stack-trace]
     (map #(let [class-name  (or (.getClassName  %) "")
                 method-name (or (.getMethodName %) "")
                 file-name   (or (.getFileName   %) "")
                 line-number (.getLineNumber %)]
             [file-name line-number class-name method-name])
          (into [] stack-trace)))
  ([]
     (get-stack-trace (.getStackTrace (Thread/currentThread)))))


(defn get-clj-stack-trace
  ([classname-begin-tokens classname-not-begin-tokens]
    (let [clj-stacktrace? (fn [[file-name line-number class-name method-name]]
                            (and (.contains file-name ".clj")
                              (or (empty? classname-begin-tokens)
                                (some #(.startsWith class-name %)
                                  classname-begin-tokens))
                              (every? #(not (.startsWith class-name %))
                                classname-not-begin-tokens)))]
      (filter clj-stacktrace? (get-stack-trace))))
  ([]
    (get-clj-stack-trace [] ["clojure."])))


(defn print-table
  [width-vector title-vector many-value-vectors]
  (assert (= (type width-vector) (type title-vector) (type many-value-vectors)
            (type [])))
  (let [col-count (count width-vector)]
    (assert (every? #(= (count %) col-count) many-value-vectors)))
  (assert (= (count width-vector) (count title-vector)))
  (let [fix-width (fn [text width]
                    (apply str
                      (take width (apply str text (take width (repeat " "))))))
        sep-vector (into [] (map #(apply str (repeat % "-")) width-vector))]
    (doseq [each (into [title-vector sep-vector] many-value-vectors)]
      (doseq [i (take (count width-vector) (iterate inc 0))]
        (print (fix-width (each i) (width-vector i)))
        (print " | "))
      (println))))


(defn dump-stack
  ([stack-trace-vector]
    (print-table [20 5 45 10] ["File" "Line#" "Class" "Method"]
      (into [] stack-trace-vector)))
  ([]
    (dump-stack (get-clj-stack-trace))))

(defn split-str [s n]
  "splits string s in strings of length n"
  (loop [[s r] [s s]
         sq []]
    (if (empty? r)
      sq
      (let [f (str/take n r)]
        (recur [f (str/drop n r)] (conj sq f))))))

(defn same-directory? [lkif-path path]
  (= (.getParent (file lkif-path)) (.getParent (file path))))

(defn make-relative [path relative-to]
  (printf "make-relative\n")
  (printf "path = %s, relative-to = %s\n" path relative-to)
  (printf "result = %s\n"
          (let [f (file path)
                f2 (file relative-to)
                dirsize (count (.getPath f2))
                dirsize (+ dirsize (count (java.io.File/separator)))]
            (subs (.getPath f) dirsize)))
  (let [f (file path)
        f2 (file relative-to)
        dirsize (count (.getPath f2))
        dirsize (+ dirsize (count (java.io.File/separator)))]
    (subs (.getPath f) dirsize)))

(defn make-absolute [path relative-to]
  (.getPath (file (str relative-to java.io.File/separator path))))

(defn in-directory? [path dir]
  "returns true if path is directly under or below directory dir"
  (throw (Exception. "NYI")))