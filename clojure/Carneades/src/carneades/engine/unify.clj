;;; Carneades Argumentation Library and Tools.
;;; Copyright (C) 2010 Thomas F. Gordon, Fraunhofer FOKUS, Berlin
;;; 
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License version 3 (LGPL-3)
;;; as published by the Free Software Foundation.
;;; 
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for details.
;;; 
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(ns carneades.engine.unify
  (:use clojure.test clojure.contrib.def
        carneades.engine.statement
        carneades.engine.utils))

;; Based on the implementation of the unification algorithm from
;; the book "The Scheme Programming Language", by Kent Dybvig.

(defn occurs? [u t]
  "Returns true iff u occurs in v"
  (and (compound-term? t)
       (loop [args (term-args t)]
         (and (compound-term? args)
              (or (= u (term-functor args))
                  (occurs? u (term-functor args))
                  (recur (term-args args)))))))

(defn sigma [u v s]
  "Returns a new substitution procedure extending s by
   the substitution of u with v"
  (letfn [(sig [x]
               (cond (variable? x) (if (= x u) v x)
                     (nonemptyseq? x) (cons (first x) (map sig (rest x)))
                     (fatom? x) (struct fatom (:form x)
                                        (cons (first (:term x))
                                              (map sig (rest (:term x)))))
                     :else x))]
    (fn [x]
      (sig (s x)))))

(declare unify)
; mutual recursive: it may be useful to use (trampoline)
; in a near future

(defn try-subst [u v s ks kf occurs-check]
  "Tries to substitute u for v but may require a
   full unification if (s u) is not a variable, and it may
   fail if it sees that u occurs in v."
  (let [u (s u)]
    (if-not (variable? u)
      (unify u v s ks kf occurs-check)
      (let [v (s v)]
        (cond (= u v) (ks s)
              (and occurs-check (occurs? u v)) (kf :cycle)
              :else (ks (sigma u v s)))))))

(defn unify
  " Attempts to unify u and v with a continuation-passing
    style that returns a substitution to the success argument
    ks or an error message to the failure argument kf. The
    substitution itself is represented by a procedure from
    variables to terms. The occurs-check flag determines whether
    the occurs check is performed.

    unify: term term -> substitution | nil
    (unify u v) is a simplified interface, where the initial
    substitution is the identity procedure, the initial success
    continuation returns the unified term, the initial failure
    continuation returns nil and the occurs-check is not performed."
  ([u v s ks kf occurs-check]
     (cond (variable? u) (try-subst u v s ks kf occurs-check)
           (variable? v) (try-subst v u s ks kf occurs-check)
           (and (constant? u) (constant? v)) (if (= u v) (ks s) (kf :clash))
           (and (compound-term? u)
                (compound-term? v)
                (= (term-functor u) (term-functor v))
                (= (count (term-args u)) (count (term-args v))))
           (letfn [(unif [u v s]
                         (if (empty? u)
                           (ks s)
                           (unify (first u)
                                  (first v)
                                  s
                                  #(unif (term-args u) (term-args v) %)
                                  kf
                                  occurs-check)))]
             (unif (term-args u) (term-args v) s))
           :else (kf :clash)))
  ([u v]
     (unify u v identity #(% u) (fn [state] nil) false)))

(defn genvar []
  "generate a fresh, unique variable"
  (gensym "?"))

(defn rename-variables [m t]
  (throw (Exception. "NYI")))

(deftest test-unify
  (is (= '?y (unify '?x '?y)))
  (is (= '(f (h) (h)) (unify '(f ?x (h)) '(f (h) ?y))))
  (is (let [stm '(f (g ?x) ?y)]
        (nil? (unify stm '(f ?y ?x)
                     identity #(% stm) (fn [state] nil) true))))
  (is (= '(f (g ?x) (g ?x)) (unify '(f (g ?x) ?y) '(f ?y (g ?x)))))
  (is (= '(f (g ?x) (g ?x)) (unify '(f (g ?x) ?y) '(f ?y ?z)))))
