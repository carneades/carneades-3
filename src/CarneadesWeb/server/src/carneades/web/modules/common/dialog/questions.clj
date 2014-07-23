;; Copyright (c) 2012 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Get questions and translation from formal logic statements to human languages"}
  carneades.web.modules.common.dialog.questions
  (:require [clojure.string :as s]
            [clojure.pprint :refer :all]
            [carneades.engine.theory :as scheme]
            [carneades.database.db :as db]
            [carneades.database.argument-graph :as ag-db]
            [carneades.project.fs :as project]
            [carneades.engine.policy :as policy]
            [carneades.engine.argument-graph :as ag]
            [carneades.engine.theory.translation :as ttr]
            [carneades.engine.statement :refer :all]
            [carneades.engine.argument-evaluation :refer :all]
            [carneades.engine.aspic :refer :all]
            [carneades.web.modules.common.dialog.utils :refer :all]
            [carneades.engine.unify :refer [genvar apply-substitutions]]
            [carneades.database.export :refer [export-to-argument-graph]]
            [carneades.database.evaluation :refer [evaluate-graph]]))

(defn functional?
  [role]
  (and (= (:min role) 1) (= (:max role) 1)))

(defn get-predicate
  [stmt policy]
  (let [pred (literal-predicate stmt)]
    (-> policy :language pred)))

(defn- get-question-text
  [stmt policy lang]
  (let [predicate (get-predicate stmt policy)
        translator (ttr/make-language-translator (:language policy))]
    (cond (and (scheme/role? predicate)
               (coll? (:type predicate)))
          (let [[s o v] stmt
                stmt2 (list s o (genvar))]
            (:translation (translator {:literal stmt2
                                       :lang lang
                                       :direction :positive})))
          (ground? stmt) (:translation (translator {:literal stmt
                                                    :lang lang
                                                    :direction :question}))
          :else (:translation (translator {:literal stmt
                                           :lang lang
                                           :direction :positive})))))

(defn- get-hint
  [questiondata lang]
  (-> questiondata :hint lang))

(defn- get-category
  "Returns the category and the category name"
  [policy pred lang]
  (let [category_key (-> policy :language pred :category)
        _ (prn "category_key=" category_key)
        _ (prn "lang=" lang)
        category_name (-> policy :language category_key :text lang)]
    [category_key category_name]))

(defn- get-widgets
  "Returns the appropriate widget. This is for backward compatibility.
The type field is now used instead of the widget field, in the policy. On the client-side
widget is still used. New Types of :string maps to :widgets 'text."
  [predicate]
  (or (:widgets predicate)
      (replace {:string 'text} [(or (:type predicate) :string)])))

(defn get-answers-choice-for-grounded-predicate
  "Builds the data for a yes/no question of a predicate."
  [policy stmt lang default-fn]
  (let [yes (get-in policy [:language 'yes :text lang])
        no (get-in policy [:language 'no :text lang])
        maybe (get-in policy [:language 'maybe :text lang])]
    (merge {:answers [yes no maybe]
            :formalanswers ['yes 'no 'maybe]
            :grounded true
            :widgets '[radio]}
           (default-fn stmt true))))

(defn get-answers-choice-for-predicate-helper
  "Builds the data for a generic question of a predicate."
  [policy stmt lang default-fn]
  (let [predicate (get-predicate stmt policy)
        formalanswers (-> predicate :answers)
        termargs (term-args stmt)
        ;; filter out answer for grounded variables (since they are not asked)
        formalanswers (keep (fn [[term answer]] (when (variable? term) answer)) (partition 2 (interleave termargs formalanswers)))
        answers (map (fn [sym] (-> policy :language sym :text lang)) formalanswers)
        widgets (get-widgets predicate)
        widgets (keep (fn [[term widget]] (when (variable? term) widget)) (partition 2 (interleave termargs widgets)))]
    (merge {:answer answers
            :formalanswers formalanswers
            :grounded false
            :widgets widgets}
           (default-fn stmt false))))

(defn- get-answers-choices-for-predicate
  [policy stmt lang default-fn]
  (prn "[get-answers-choices-for-predicate] lang=" lang)
  (let [predicate (get-predicate stmt policy)
        arity (scheme/get-arity predicate)]
    (if (or (ground? stmt) (zero? arity))
      (get-answers-choice-for-grounded-predicate policy stmt lang default-fn)
      (get-answers-choice-for-predicate-helper policy stmt lang default-fn))))

(defn get-typename
  [type policy lang]
  (if (coll? type)
    (map (fn [t]
           (get-in policy [:language t :text lang]))
         type)
    (get-in policy [:language type :text lang])))

(defn get-default-role-answer
  [stmt]
  (let [obj (second (term-args stmt))]
   (when (not (variable? obj))
     obj)))

(defn get-answers-choices-for-role
  [policy stmt lang default-fn]
  (let [predicate (get-predicate stmt policy)
        {:keys [min max type]} predicate
        typename (get-typename type policy lang)
        yes (get-in policy [:language 'yes :text lang])
        no (get-in policy [:language 'no :text lang])
        maybe (get-in policy [:language 'maybe :text lang])
        grounded (and (not (coll? type)) (ground? stmt))]
    (merge {:min min
            :max max
            :type type
            :typename typename
            :answers [yes no maybe]
            :formalanswers (when grounded '[yes no maybe])
            :grounded grounded}
           (default-fn stmt grounded))))

(defn- get-answers-choices
  [policy stmt lang default-fn]
  (let [predicate (get-predicate stmt policy)]
    (cond (scheme/role? predicate)
          (get-answers-choices-for-role policy stmt lang default-fn)
          (or (scheme/predicate? predicate)
              (scheme/concept? predicate))
          (get-answers-choices-for-predicate policy stmt lang default-fn)
          :else (throw (Exception. (str "NYI:" predicate))))))

(defn get-first-question
  [id stmt lang policy default-fn]
  {:pre [(not (nil? policy))]}
  (let [pred (literal-predicate stmt)
        predicate ((:language policy) pred)
        text (get-question-text stmt policy lang)
        [category category-name] (get-category policy pred lang)
        hint (get-hint predicate lang)
        answers-choices (get-answers-choices policy stmt lang default-fn)]
    (merge {:id id
            :category category
            :category_name category-name
            :hint hint
            :text text
            :statement stmt
            :role (scheme/role? predicate)
            :concept (scheme/concept? predicate)}
           answers-choices
           (default-fn stmt (:grounded answers-choices)))))

(declare get-other-questions)

(defn get-questions
  [id stmt lang policy default-fn]
  (let [pred (literal-predicate stmt)
        first-question (get-first-question id stmt lang policy default-fn)
        [category category-name] (get-category policy pred lang)
        pred-by-categories (group-by (fn [[pred val]] (:category val)) (-> policy :language))
        other-preds (disj (set (map first (pred-by-categories category))) pred)
        other-questions (get-other-questions id stmt other-preds lang policy default-fn)
        ]
    (cons first-question other-questions)))

(defn get-other-questions
  [id stmt others lang policy default-fn]
  (map (fn [id pred]
         (let [predicate (get-in policy [:language pred])
               arity (scheme/get-arity predicate)
               [_ o _] stmt
               stmtq (cond (scheme/role? predicate) (list (:symbol predicate) o (genvar))
                           (scheme/concept? predicate) (list (:symbol predicate) o)
                           :else (doall (cons (symbol pred) (repeatedly arity genvar))))]
           (get-first-question id stmtq lang policy default-fn)))
       (iterate inc (inc id)) others))

(defn get-structured-questions
  "Returns the data necessary for the web client to build the questions.
default-fn is a function returning the default formalized answer for a question."
  ([stmt lang last-id policy]
     (get-structured-questions stmt lang last-id policy (constantly nil)))
  ([stmt lang last-id policy default-fn]
     {:pre [(not (nil? lang))]}
     (let [id (inc last-id)
           pred (literal-predicate stmt)
           questions (get-questions id stmt (keyword lang) policy default-fn)]
       [questions (+ last-id (count questions))])))

(defn askable?
  [policy p]
  ;; {:pre [(do
  ;;          (prn "                ASKABLE:" p)
  ;;          true)]
  ;;  :post [(do
  ;;           (prn "                ===>" %)
  ;;           true)]}
  (let [predicate (get-in policy [:language (literal-predicate p)])]
    (and (literal-pos? p)
         (or (:askable predicate) (:widgets predicate))
         (or (scheme/predicate? predicate)
             (scheme/concept? predicate)
             (and (scheme/role? predicate)
                  (ground? (first (term-args p))))))))

(defn askable-statements-atoms
  "Returns the list of askable statements in the ag stored in db"
  [project dbname policy]
  (prn "[askable-statements-atoms]")
  (prn "project=" project)
  (prn "policy=")
  (pprint policy)
  (db/with-db (db/make-connection project dbname "guest" "")
    (let [statements (ag-db/list-statements)]
      (map :atom (filter (partial askable? policy) statements)))))

(defn remove-superfluous-questions
  [questions policy]
  (first
   (reduce (fn [[questions seen] question]
             (let [p (literal-predicate (:statement question))
                   pred (get-predicate (:statement question) policy)]
              (if (seen p)
                (if (or (scheme/concept? pred)
                        (and (scheme/role? pred)
                             (or (= (:max pred) 1)
                                 (coll? (:type pred)))))
                  [questions seen]
                  [(cons question questions) (conj seen p)])
                [(cons question questions) (conj seen p)])))
           [() #{}]
           questions)))

(defn get-default-values-for-x
  [ag atoms-for-pred stmt value-fn]
  (reduce (fn [m s]
            (let [n (ag/get-statement-node ag s)
                  val (value-fn s n)]
              (if val
                (-> m
                    (update-in [:values] concat [val])
                    (update-in [:facts-uuid] conj (:id n))
                    (update-in [:nb-facts] inc))
                m)))
          {:nb-facts 0
           :facts-uuid []}
          atoms-for-pred))

(defn get-default-values-for-grounded
  [ag atoms-for-pred stmt]
  (get-default-values-for-x ag atoms-for-pred stmt
                            (fn [s n]
                              (condp = (:value n)
                                0.0 '[no]
                                1.0 '[yes]
                                0.5 '[maybe]
                                nil))))

(defn get-default-values-for-role
  [ag atoms-for-pred stmt]
  ;; {:pre [(do (prn "atoms-for-pred =") (pprint atoms-for-pred) (prn "---") true)]
  ;;  :post [(do (prn "get-default-values-for-role") (pprint %) (prn "---") true)]}
  (get-default-values-for-x ag atoms-for-pred stmt
                            (fn [s n]
                              (let [obj (second (term-args s))]
                               (when (= (:value n) 1.0)
                                 [obj])))))

(defn get-default-values
  [ag policy atoms-by-pred stmt grounded]
  {:post [(do (prn "default values for " stmt " is ") (prn %) true)]}
  (let [predicate (get-predicate stmt policy)
        atoms-for-pred (atoms-by-pred (:symbol predicate))]
   (if grounded
     (get-default-values-for-grounded ag atoms-for-pred stmt)
     (get-default-values-for-role ag atoms-for-pred stmt))))

(defn put-atoms-being-accepted-in-the-front
  "Sorts the atoms and places those having a :value of 1.0 in the front."
  [ag atoms]
  (sort-by (fn [a]
             (- (:value (ag/get-statement-node ag a))))
           atoms))

(defn get-questions-for-answers-modification
  "Returns a list of questions to modify the answer in the ag stored in db."
  [project db policy-name lang]
  (let [policy (project/load-theory project policy-name)
        ag (export-to-argument-graph (db/make-connection project db "guest" ""))
        atoms (askable-statements-atoms project db policy)
        atoms (put-atoms-being-accepted-in-the-front ag atoms)
        atoms-by-pred (group-by first atoms)
        ;; keep only one atom per predicate
        atoms (map first (vals atoms-by-pred))
        questions (map-indexed (fn [idx atom]
                                 (get-first-question
                                  idx
                                  atom
                                  lang
                                  policy
                                  (partial get-default-values ag policy atoms-by-pred)))
                               atoms)
        questions (remove-superfluous-questions questions policy)]
    questions))

(defn smart-update-statement
  "Updates the literal in the db with a new weight.
If the literal corresponds to a functional role in the policy, set
all other roles having the same predicate but different objects to a weight
of 0.0"
  [literal weight policy]
  (let [id (ag-db/get-statement literal)]
    (ag-db/update-statement id {:weight weight})
   (let [pred (get-in policy [:language (literal-predicate literal)])]
     (when (and (scheme/role? pred)
                (scheme/functional-role? pred)
                (set? (:type pred)))
       ;; exclude the other possible values
       (let [[_ _ old-obj] literal]
        (doseq [obj (disj (:type pred) old-obj)]
          (let [other-role (scheme/replace-role-obj literal obj)
                other-id (ag-db/get-statement other-role)]
            (ag-db/update-statement other-id {:weight 0.0}))))))))

(defn update-statements-weights
  "Updates the weight of the statements in the db."
  [project db username password statements policy]
  (let [dbconn (db/make-connection project db username password)]
   (db/with-db dbconn
     (doseq [[literal weight] statements]
       (smart-update-statement literal weight policy)))))

(defn modify-statements-weights
  "Updates the values of the given statements in the db.
Statements are represented as a collection of [statement value] element."
  [project db username password statements policy]
  (update-statements-weights project db username password statements policy)
  (evaluate-graph project db username password))

(defn pseudo-delete-statements
  "Set the weight of the statements to 0.5 in the db."
  [project db username password ids]
  (let [dbconn (db/make-connection project db username password)]
   (db/with-db dbconn
     (doseq [id ids]
       (ag-db/update-statement id {:weight 0.5})))))
