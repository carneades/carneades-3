;;; Copyright (c) 2010-2011 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Unification algorithm from the implementation of the book
           'The Scheme Programming Language', by Kent Dybvig."}
  carneades.engine.unify
  (:use carneades.engine.statement
        carneades.engine.utils))

(defn- occurs?
  "Returns true iff u occurs in v"
  [u t]
  (and (compound-term? t)
       (loop [args (term-args t)]
         (and (compound-term? args)
              (or (= u (term-functor args))
                  (occurs? u (term-functor args))
                  (recur (term-args args)))))))

(defn apply-substitutions
  [s t]
  (let [t2 (cond
             (variable? t) (get s t t),
             (statement? t) (assoc t :atom (apply-substitutions s (literal-atom t))),
             (nonemptyseq? t) (cons (apply-substitutions s (first t))
                                    (map (fn [x] (apply-substitutions s x)) (rest t))),
             (vector? t) (reduce conj [(apply-substitutions s (first t))]
                                 (apply-substitutions s (rest t)))
             :else t)]
    (if (and (variable? t2) (contains? s t2))
      (apply-substitutions s t2)
      t2)))

(def identity-map (hash-map))

(defn- sigma
  "Returns a new substitution table extending s by
   the substitution of u with v"
  [u v s]
  (assoc s u v))

(declare unify)
; mutual recursive: it may be useful to use (trampoline)
; in a near future

(defn- try-subst
  "Tries to substitute u for v but may require a
   full unification if (s u) is not a variable, and it may
   fail if it sees that u occurs in v."
  [u v s ks kf occurs-check]
  (let [u (apply-substitutions s u)]
    (if-not (variable? u)
      (unify u v s ks kf occurs-check)
      (let [v (apply-substitutions s v)]
        (cond (= u v) (ks s)
              (and occurs-check (occurs? u v)) (kf :cycle)
              :else (ks (sigma u v s)))))))

(defn unify
  "Attempts to unify u and v with a continuation-passing
   style that returns a substitution to the success argument
   ks or an error message to the failure argument kf. The
   substitution itself is represented by a procedure from
   variables to terms. The occurs-check flag determines whether
   the occurs check is performed.

   unify: term term -> substitutions | nil
   (unify u v) is a simplified interface, where the initial
   substitution is the identity procedure, the initial success
   continuation returns the unified term, the initial failure
   continuation returns nil and the occurs-check is not performed."
  ([u v s ks kf occurs-check]
    (letfn [(unif [u v s]
                  (if (empty? u)
                    (ks s)
                    (unify (first u) (first v)
                           s
                           #(unif (rest u)
                                  (rest v) %)
                           kf
                           occurs-check)))]
      (cond (variable? u) (try-subst u v s ks kf occurs-check)
            (variable? v) (try-subst v u s ks kf occurs-check),
            (and (constant? u) (constant? v)) (if (= u v) (ks s) (kf :clash)),
            (statement? u) (unify (literal->sliteral u) v s),
            (statement? v) (unify u (literal->sliteral v) s),
            (and (termseq? u)
                 (termseq? v)
                 (= (count u) (count v))) (unif u v s)
            (and (compound-term? u)
                 (compound-term? v)
                 ;; (= (term-functor u) (term-functor v))
                 (= (count (term-args u))
                    (count (term-args v)))) (unif u v s)
            ;; (unif (term-args u) (term-args v) s),

            :else (kf :clash))))
  ([u v s]
    (unify u v s identity (fn [state] nil) false))
  ([u v]
    (unify u v identity-map identity (fn [state] nil) false)))

(defn genvar
  "Generate a fresh, unique variable"
  []
  (gensym "?"))


(defn rename-variables
  "hashmap term -> [hashmap term]

   Systematically rename the variables in term, keeping track of the
   replacements in the map"
  [m trm]

  (cond (variable? trm) (if-let [v (m trm)]
                          [m v]
                          (let [v (genvar)]
                            [(assoc m trm v) v]))
        (nonemptyseq? trm) (let [[m2 trm2] (rename-variables m (first trm))
                                 [m3 trm3] (rename-variables m2 (next trm))]
                             [m3 (cons trm2 trm3)])
        (statement? trm) (let [[m2 trm2] (rename-variables m (first (literal-atom trm)))
                               [m3 trm3] (rename-variables m2 (next (literal-atom trm)))]
                           [m3 (assoc trm :atom
                                      (cons trm2 trm3))])
        (vector? trm) (let [[m2 trm2] (rename-variables m (first trm))
                            [m3 trm3] (rename-variables m2 (next trm))]
                        [m3 (reduce conj [trm2] trm3)])
        :else [m trm]))
