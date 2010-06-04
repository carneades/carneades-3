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

(ns carneades.engine.abduction
  (:use clojure.contrib.def
        clojure.contrib.pprint
        [clojure.contrib.seq-utils :only (group-by flatten)]
        carneades.engine.utils
        carneades.engine.argument
        carneades.engine.statement))

(set! *assert* true)

(defvar *verum-clause* '(true))
(defvar *verum* (list *verum-clause*))
(defvar *falsum-clause* '(false))
(defvar *falsum* (list *falsum-clause*))

(declare argument-in-label
         argument-out-label
         conjunct-dnf-with-dnf conjunct-clause-with-dnf
         combine-conjunction-of-dnf
         true-filter
         collect-labels-conj) 

;; dispatch on the proof standard
(defmulti ps-in-label (fn [ag asm s] (proof-standard ag s)))

(defmulti ps-out-label (fn [ag asm s] (proof-standard ag s)))

(defmethod ps-in-label :default [ag asm s]
           (list (list s)))

(defmethod ps-out-label :default [ag asm s]
           (list (list (statement-complement s))))

(defn statement-in-label [ag asm s]
  {:pre [(set? asm)]}
  "argument-graph (set-of statement) statement -> dnf"
  (cond (contains? asm s) *verum*
        (contains? asm (statement-complement s)) (list (list s))
        :else (ps-in-label ag asm s)))

(defn statement-out-label [ag asm s] 
  {:pre [(set? asm)]}
  (cond (contains? asm s) (list (list (statement-complement s)))
        (contains? asm (statement-complement s)) *verum*
        :else (ps-out-label ag asm s)))

(defn argument-in-label [ag asm arg]
  {:pre [(set? asm)]}
  (letfn [(collect-labels
           [f statements]
           (collect-labels-conj #(f ag asm (premise-statement %)) statements))]
    (let [groups (group-by exception? (argument-premises arg))
          ex (get groups true ())
          pr (get groups false ())
          [pr-labels pr-labels-true] (collect-labels statement-in-label pr)
          [ex-labels ex-labels-true] (collect-labels statement-out-label ex)]
      (cond (and (= pr-labels-true ex-labels-true)) *verum*
            pr-labels-true (map true-filter
                                (combine-conjunction-of-dnf ex-labels))
            ex-labels-true (map true-filter
                                (combine-conjunction-of-dnf pr-labels))
            :else (map true-filter (combine-conjunction-of-dnf
                                    (concat pr-labels ex-labels)))))))

(defn collect-labels-conj
  "returns [labels alltrue]
   alltrue is true if coll is empty or if each value is equal to *verum*"
  [get-label coll]
  {:pre [(not (nil? coll))]}
  (reduce (fn [[labels alltrue] p]
            (let [label (get-label p)]
              (if (or (not= label *verum*) (not alltrue))
                [(conj labels label) false]
                [(conj labels label) true])))
          [() true] coll))

(defn collect-labels-conj-break
  "returns [labels alltrue onefalse]
   stop collecting when *falsum* is encountered
   alltrue is true if coll is empty or if each value is equal to *verum*"
  [get-label coll]
  {:pre [(not (nil? coll))]}
  (loop [coll coll
         labels ()
         alltrue true
         onefalse false]
    (if (empty? coll)
      [labels alltrue onefalse]
      (let [label (get-label (first coll))]
        (if (= label *falsum*)
          [(conj labels label) alltrue true]
          (if (or (not= label *verum*) (not alltrue))
            (recur (next coll) (conj labels label) false onefalse)
            (recur (next coll) (conj labels label) alltrue onefalse)))))))

(defn collect-labels-disj [get-label coll]
  {:pre [(not (nil? coll))]}
  "stop collecting when a seq of label contains *verum-clause*
   returns [labels dis-is-true dis-is-false]"
  (loop [coll coll
         labels ()]
    (if (empty? coll)
      (if (empty? labels)
        [labels false true]  ;; the disjunction is false
        [labels false false]) ;; we don't know yet
      (let [p (first coll)
            label (get-label p)]
        (if (.contains label *verum-clause*)
          [labels true false] ;; the disjunction is true
          (recur (next coll) (concat labels label)))))))

(defn argument-out-label [ag asm arg]
  {:pre [(set? asm)]}
  (letfn [(collect-labels
           [f statements]
           (collect-labels-disj #(f ag asm (premise-statement %)) statements))]
    (let [groups (group-by exception? (argument-premises arg))
          ex (get groups true ())
          pr (get groups false ())
          [pr-labels pr-labels-true] (collect-labels statement-out-label pr)
          [ex-labels ex-labels-true] (collect-labels statement-in-label ex)]
      (cond (or pr-labels-true ex-labels-true) *verum*
            (and (empty? pr-labels) (empty? ex-labels)) *falsum*
            (empty? pr-labels) ex-labels
            :else (map true-filter (concat pr-labels ex-labels))))))

(defn combine-conjunction-of-dnf [dnfs]
  (cond (empty? dnfs) ()
        (empty? (next dnfs)) (first dnfs)
        :else (conjunct-dnf-with-dnf (first dnfs)
                (combine-conjunction-of-dnf (next dnfs)))))

;;  '((A B) (AA BB)) '((C D)) -> ((A B C D) (AA BB C D))
(defn conjunct-dnf-with-dnf [dnf dnf2]
  (mapcat (fn [c] (conjunct-clause-with-dnf c dnf2)) dnf))

;; '(A B) '( (X Y Z) (X2 Y2 Z2) ) -> ((A B X Y Z) (A B X2 Y2 Z2))
(defn conjunct-clause-with-dnf [clause dnf]
  (map #(concat % clause) dnf))

(defn true-filter [col]
  (remove true? col))

(defmethod ps-in-label :dv [ag asm s]
      {:pre [(set? asm)]}
  (letfn [(collect-arguments-labels-disj
           [f arguments]
           (collect-labels-disj
            #(f ag asm %) arguments))
          (collect-arguments-labels-conj
           [f arguments]
           (collect-labels-conj-break #(f ag asm %) arguments))]
    (let [pro-args (pro-arguments ag s)
          con-args (con-arguments ag s)
          [pro-labels pro-labels-true]
          (collect-arguments-labels-disj argument-in-label pro-args)
          [con-labels con-labels-true con-labels-onefalse]
          (collect-arguments-labels-conj argument-out-label con-args)]
      (cond con-labels-onefalse (list (list s))
            (and pro-labels-true con-labels-true) *verum*
            pro-labels-true (conj (map true-filter
                                       (combine-conjunction-of-dnf con-labels))
                                  (list s))
            con-labels-true (conj pro-labels (list s))
            :else (conj (map true-filter
                             (combine-conjunction-of-dnf
                              (conj con-labels pro-labels)))
                        (list s))))))

(defmethod ps-out-label :dv [ag asm s]
      {:pre [(set? asm)]}
  (letfn [(collect-arguments-conj
           [f arguments]
           (collect-labels-conj-break #(f ag asm %) arguments))
          (collect-arguments-disj
           [f arguments]
           (collect-labels-disj #(f ag asm %) arguments))]
      (let [pro-args (pro-arguments ag s)
            con-args (con-arguments ag s)
            [pro-labels pro-labels-true pro-labels-onefalse]
            (collect-arguments-conj argument-out-label pro-args)
            [con-labels con-labels-true]
            (collect-arguments-disj argument-in-label con-args)]
        (cond (or pro-labels-true con-labels-true) *verum*
              pro-labels-onefalse (conj con-labels (statement-complement s))
              :else (conj (map true-filter
                               (concat con-labels
                                       (combine-conjunction-of-dnf pro-labels)))
                          (statement-complement s))))))

(defmethod ps-in-label :ba [ag asm s]
      {:pre [(set? asm)]}
  (let [pro-args (pro-arguments ag s)
        con-args (con-arguments ag s)]
   (loop [pro-args pro-args
          labels ()]
     (if (empty? pro-args)
       (conj labels (list s))
       (let [pro-arg (first pro-args)
             w (sget pro-arg :weight)
             greater-cons (filter #(<= w (sget % :weight)) con-args)
             pro-label (argument-in-label ag asm pro-arg)
             [con-labels _ onefalse] (collect-labels-conj-break
                                      #(argument-out-label ag asm %)
                                      greater-cons)
             collectedlabels (if onefalse
                               *falsum*
                               (combine-conjunction-of-dnf
                                (conj (remove #(= *verum* %) con-labels)
                                      (if (= pro-label *verum*)
                                        ()
                                        pro-label))))]
         (cond (empty? collectedlabels) *verum*
               (= collectedlabels *falsum*) (recur (next pro-args) labels)
               :else (recur (next pro-args)
                            (concat labels collectedlabels))))))))

(defmethod ps-out-label :ba [ag asm s]
    {:pre [(set? asm)]}
  (let [pro-args (pro-arguments ag s)
        con-args (con-arguments ag s)]
    (loop [pro-args pro-args
           labels ()]
      (if (empty? pro-args)
        (if (empty? labels)
          *verum*
          (conj (combine-conjunction-of-dnf labels)
              (list (statement-complement s))))
        (let [pro-arg (first pro-args)
              w (sget pro-arg :weight)
              greater-cons (filter #(<= w (sget % :weight)) con-args)
              pro-label (argument-out-label ag asm pro-arg)
              [con-labels con-labels-true con-labels-false]
              (collect-labels-disj #(argument-in-label ag asm %)
                                   greater-cons)]
          (cond (or (= pro-label *verum*) con-labels-true)
                (recur (next pro-args) labels)
                (and (= pro-label *falsum*) (con-labels-false))
                (list (list (statement-complement s)))
                (= pro-label *falsum*) (recur (next pro-args)
                                              (conj labels con-labels))
                con-labels-false (recur (next pro-args)
                                        (conj labels pro-label))
                :else (recur (next pro-args)
                             (conj labels
                                   (concat pro-label con-labels)))))))))
