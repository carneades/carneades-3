;; Copyright (c) 2010 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.


(ns ^{:doc "This library is implemented to convert every logical formula into an
            equivalent 'disjunctive normalform' (DNF). A DNF is a disjunction of a
            conjunction of literals.
            
            WARNING:  This code is currently not used elsewhere in the Carneades engine.
            The old rule module, which used this code, has been replaced by the new
            scheme module.  Currently schemes are defined using Clojure records.  No
            macros have been defined for a higher-level rule language.  Should the need
            arise for a more user friendly, higher-level DSL, this DNF code could be useful.
            
            You can find a more detailed, that is a more mathematic, description of
            logical formulas below.
            
            DNF is the only way, the 'rule'-library can handle logical formulas.
            To support every valid formula, it is converted, before used.
            The algorithm used to convert the formulas to DNF runs through two main
            steps:
              - negations are only allowed before atoms (only in literals)
                  (not (not t1)) -> t1
                  (not (and t1 t2) -> (or (not t1) (not t2))
                  (not (or t1 t2) -> (and (not t1) (not t2))
              - usage of distributivity to bring inner disjunctions to an outer level
                  (and t1 (or t2 t3)) -> (or (and t1 t2) (and t1 t3))
                  (and (or t1 t2) t3) -> (or (and t1 t3) (and t2 t3))
            Before these steps could be run through, equivalences have to be substituted
            for implications, which themselves have to be substituted for disjunctions.
            You do not need to care, if you only use the 'to-dnf' method.
            
            The library was extended to the use of the predicates 'assuming' and
            'unless'.
            These extensions were made in order to distinguish special rules for burden
            of proof.
            Throughout the conversions the predicates are handled as identity and
            negation.
            
            !!! BEWARE !!!
            Worst case conversion includes exponential time complexity. For example,
            a conversion from CNF to DNF with n variables could induce 2^n operations.
            As this library is especially designed for the 'rules'-library and these
            rules won't be too long, this is ok for 'carneades' to still run quick.


            <atom>            
            <literal> = <atom> | (not <atom>)
            <assumption> = (assuming <literal>)
            <exception> = (unless <literal>)

            extended literals
            <extliteral> = <literal> | <assumption> | <exception>                
            <negation> = (not <formula>)
            pure conjunction
            
            conjunctions of extended literals
            <conjunction> = (and <formula> <formula>+)                           
            <lconjunction> = <extliteral> | (and <extliteral> <extliteral>+)     

            pure disjunction
            <disjunction> = (or <formula> <formula>+)                           
            <implication> = (if <formula> <formula>)
            <equivalence> = (iff <formula> <formula>)

            disjunctive normalform
            <dnf> = <lconjunction> | (or <lconjunction> <lconjunction>+)          
            <formula> = <extliteral> | <conjunction> | <disjunction> | <negation>
            | <implication> | <equivalence>

            predicates "}
  carneades.engine.dnf
  (:use clojure.walk
        carneades.engine.utils))

(declare formula? formulas*? associative-conversion)

(defn atom?
  "Returns true if the formula is an atom"
  [formula]
  (or (not (seq? formula))
      (and (not-empty formula)
           (let [sym (first formula)]
            (not-any? #(= % sym) '(not or and if iff unless assuming))))))

(defn literal?
  "Returns true if the formula is a literal"
  [formula]
  (or (atom? formula)
      (and (nonemptyseq? formula)
           (= (count formula) 2)
           (= (first formula) 'not)
           (atom? (second formula)))))

(defn assumption?
  "Returns true if the formula is an assumption"
  [formula]
  (and (nonemptyseq? formula)
       (= (count formula) 2)
       (= (first formula) 'assuming)
       (literal? (second formula))))

(defn exception?
  "Returns true if the formula is an exception"
  [formula]
  (and (nonemptyseq? formula)
       (= (count formula) 2)
       (= (first formula) 'unless)
       (literal? (second formula))))

(defn extliteral?
  "formula -> bool"
  [formula]
  (or (literal? formula)
      (assumption? formula)
      (exception? formula)))

(defn literals*?
  "formula -> bool"
  [formula]
  (if (nonemptyseq? formula)
    (if (literal? (first formula))
      (literals*? (rest formula))
      false)
    (if (seq? formula)
      true
      (literal? formula))))

(defn extliterals*?
  "formula -> bool"
  [formula]
  (if (nonemptyseq? formula)
    (if (extliteral? (first formula))
      (extliterals*? (rest formula))
      false)
    (if (seq? formula)
      true
      (extliteral? formula))))

(defn negation?
  "formula -> bool"
  [formula]
  (and (nonemptyseq? formula)
       (= (count formula) 2)
       (= (first formula) 'not)
       (formula? (second formula))))

(defn conjunction?
  "formula -> bool"
  [formula]
  (and (nonemptyseq? formula)
       (> (count formula) 2)
       (= (first formula) 'and)
       (formulas*? (rest formula))))

(defn disjunction?
  "formula -> bool"
  [formula]
  (and (nonemptyseq? formula)
       (> (count formula) 2)
       (= (first formula) 'or)
       (formulas*? (rest formula))))

(defn lconjunction?
  "formula -> bool"
  [formula]
  (or (extliteral? formula)
      (and (nonemptyseq? formula)
           (> (count formula) 2)
           (= (first formula) 'and)
           (extliterals*? (rest formula)))))

(defn lconjunctions*? [formula]
  (if (nonemptyseq? formula)
    (if (lconjunction? (first formula))
      (lconjunctions*? (rest formula))
      false)
    (if (seq? formula)
      true
      (lconjunction? formula))))

(defn implication?
  "formula -> bool"    
  [formula]
  (and (nonemptyseq? formula)
       (= (count formula) 3)
       (= (first formula) 'if)
       (formula? (second formula))
       (formula? (nth formula 2))))

(defn equivalence?
  "formula -> bool"
  [formula]
  (and (nonemptyseq? formula)
       (= (count formula) 3)
       (= (first formula) 'iff)
       (formula? (second formula))
       (formula? (nth formula 2))))

(defn dnf?
  "formula -> bool"
  [formula]
  (or (lconjunction? formula)
      (and (nonemptyseq? formula)
           (> (count formula) 2)
           (= (first formula) 'or)
           (lconjunctions*? (rest formula)))))

(defn formula?
  "formula -> bool"
  [formula]
  (or (extliteral? formula)
      (negation? formula)
      (conjunction? formula)
      (disjunction? formula)
      (implication? formula)
      (equivalence? formula)))

(defn formulas*?
  "formula -> bool"
  [formula]
  (if (nonemptyseq? formula)
    (if (formula? (first formula))
      (recur (rest formula))
      false)
    (if (seq? formula)
      true
      (formula? formula))))

(defn equivalence-conversion
  "formula -> formula

   Returns a formula, where every occurence of an 
   implication is substituted a=>b -> (not a) or b
  "
  [formula]
  
  (cond (equivalence? formula)
        (let [rformula (map equivalence-conversion (rest formula))]
          (list 'and
                (list 'if (first rformula) (second rformula))
                (list 'if (second rformula) (first rformula))))
        (literal? formula) formula
        (formula? formula) (cons (first formula)
                                 (map equivalence-conversion
                                      (rest formula)))
        :else formula))

(defn implication-conversion
  "Gets a formula and returns a formula, where every occurence of an
  implication is substituted
  a=>b -> (not a) or b"
  [formula]
  (cond (implication? formula) (let [[x y] (map implication-conversion
                                                (rest formula))]
                                 (list 'or (list 'not x) y))
        (literal? formula) formula
        (formula? formula?) (cons (first formula)
                                  (map implication-conversion (rest formula)))
        :else formula))

(defn negate
  "formula -> formula

   Returns a negated formula"
  [formula]
  
  (list 'not formula))

(defn negation-conversion
  "formula -> formula

   the resulting formula has negations only in literals
   "
  [formula]
  (cond (literal? formula) formula
        (conjunction? formula) (cons (first formula)
                                     (map negation-conversion (rest formula)))
        (disjunction? formula) (cons (first formula)
                                     (map negation-conversion (rest formula)))
        (negation? formula) (let [f (second formula)]
                              ;; formula (not (not t)) -> t
                              ;; (second (second formula)) = t
                              (cond (negation? f) (negation-conversion
                                                   (second (second formula)))
                                    ;; formula = (not (and t1 t2 ...))
                                    ;; -> (or (not t1) (not t2) ...)
                                    (conjunction? f)
                                    (cons 'or (map negation-conversion
                                                   (map negate
                                                        (rest
                                                         (second formula)))))
                                    ;; formula = (not (or t1 t2 ...))
                                    ;; -> (and (not t1) (not t2) ...)
                                    (disjunction? f)
                                    (cons 'and (map negation-conversion
                                                    (map negate
                                                         (rest
                                                          (second formula)))))
                                    ;; formula = (not (assuming t))
                                    ;; -> (assuming (not t))
                                    (assumption? f)
                                    (list 'assuming
                                          (negation-conversion
                                           (cons 'not (rest (second formula)))))
                                    ;; formula = (not (unless t))
                                    ;; -> (unless (not t))
                                    (exception? f)
                                    (list 'unless (negation-conversion
                                                   (cons 'not
                                                         (rest
                                                          (second formula)))))
                                    :else (negation-conversion f)))
        :else formula))

(defn distri
  "Gets a list of formulas and a disjunction and returns a disjunction of 
   conjunctions
   
   f.e.: '(a b c),'(or d e f) |-> (or (and d a b c) (and e a b c) (and f a b c))
   it us used as a single application of the distributive law, where the 
   elements of the list of formulas as well as the disjunction were operands 
   of a conjunction"
  [formulas dis]
  (cons 'or (map #(cons 'and (conj formulas %)) (rest dis))))

(defn distributive-conversion
  "Uses distributiv law to bring inner disjunctions to an outer level
   It works recursively, where nested expressions are converted from inside to 
   outside after every conversion, a simplification through the associative law 
   is used"
  [formula]
  (cond (literal? formula) formula
        (negation? formula) (list (first formula)
                                  (distributive-conversion (second formula)))
        (disjunction? formula) (cons (first formula)
                                     (map distributive-conversion
                                          (rest formula)))
        (conjunction? formula) (let [rformulas ;; apply the conversion
                                     ;; recursively to all operands
                                     (map distributive-conversion
                                          (rest formula))
                                     ;; split the operands to disjunctions and
                                     ;; no disjunctions
                                     disj (filter disjunction? rformulas)
                                     nodisj (filter (complement disjunction?)
                                                     rformulas)]
                                 (cond (empty? disj)
                                       ;; no disjunctions in the conjunction
                                       (cons (first formula)
                                             rformulas)
                                       (= (count disj) 1)
                                       ;; one disjunction -> apply distributive
                                       ;;law once
                                       (associative-conversion
                                        (distri nodisj (first disj)))
                                       :else
                                       ;;  more than one disjunction -> apply
                                       ;; distributive law once and start again
                                       (distributive-conversion
                                        (associative-conversion
                                         (distri (conj (rest disj) nodisj)
                                                 (first disj))))))
        :else formula))

(defn x-flatten [formulas dis? con?]
  {:pre [(nonemptyseq? formulas)]}
  (if (not (empty? formulas))
    (if (> (count formulas) 1)
      (if (con? (first formulas))
        (concat (x-flatten (rest (first formulas)) dis? con?)
                (x-flatten (rest formulas) dis? con?))
        (cons (first formulas) (x-flatten (rest formulas) dis? con?)))
      (if (con? (first formulas))
        (x-flatten (rest (first formulas)) dis? con?)
        formulas))
    formulas))

(defn con-flatten
  "Gets a list of formulas, which are originally operands of a conjunction
   returns a list of formulas, in which no formula is a conjunction
   formulas, who were conjunctions, were substituted for their operands"
  [formulas]
  (x-flatten formulas disjunction? conjunction?))

(defn dis-flatten
  "Gets a list of formulas, which are originally operands of a disjunction
   returns a list of formulas, in which no formula is a disjunction
   formulas, who were disjunctions, were substituted for their operands"
  [formulas]
  (x-flatten formulas conjunction? disjunction?))

(defn associative-conversion
  "formula -> formula

   uses associative law to simplify nested conjunctions and disjunctions to 
   lists
   "
  [formula]
  (cond (literal? formula) formula
        (negation? formula) (list (first formula)
                                  (associative-conversion (second formula)))
        (conjunction? formula) (cons
                                (first formula)
                                (con-flatten
                                 (map associative-conversion (rest formula))))
        (disjunction? formula) (cons
                                (first formula)
                                (dis-flatten
                                 (map associative-conversion (rest formula))))
        :else formula))


(defn to-dnf [formula]
  ;{:pre [(formula? formula)]}
  " <formula> -> <formula>
    
   Converts every valid logical formula into disjunctive normalform"
  [formula]
  {:pre [(formula? formula)]}
  (if (not (dnf? formula))
    (associative-conversion
     (distributive-conversion
      (associative-conversion
       (negation-conversion 
        (implication-conversion 
         (equivalence-conversion formula))))))
    formula))

;; (defn filter-literals [dnf]
;;   "returns a set of all literals contained in a dnf"
;;   (set (mapcat identity dnf)))

;; (defn filter-literals [clause]
;;   "returns a set of all literals contained in a clause"
;;   (set (mapcat identity clause)))
