;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Get questions and translation from formal logic statements to human languages"}
  impact.web.logic.questions
  (:use clojure.data.json
        clojure.pprint
        impact.web.core
        carneades.engine.statement
        [carneades.engine.unify :only (genvar apply-substitutions)]
        impact.web.logic.translate
        [carneades.engine argument-evaluation aspic]
        [carneades.database.export :only [export-to-argument-graph]]
        [carneades.database.evaluation :only [evaluate-graph]])
  (:require [clojure.string :as s]
            [carneades.engine.scheme :as scheme]
            [carneades.database.db :as db]
            [carneades.engine.policy :as policy]
            [carneades.engine.argument-graph :as ag]))

(defn functional?
  [role]
  (and (= (:min role) 1) (= (:max role) 1)))

(defn get-predicate
  [stmt theory]
  (let [pred (literal-predicate stmt)]
    (-> theory :language pred)))

(defn- get-question-text
  [stmt theory lang]
  (let [predicate (get-predicate stmt theory)]
    (cond (and (scheme/role? predicate)
               (coll? (:type predicate)))
          (let [[s o v] stmt
                stmt2 (list s o (genvar))]
            (scheme/format-statement stmt2 (:language theory) lang :positive))
          (ground? stmt) (scheme/format-statement stmt (:language theory) lang :question)
          :else (scheme/format-statement stmt (:language theory) lang :positive))))

(defn- get-hint
  [questiondata lang]
  (-> questiondata :hint lang))

(defn- get-category
  "Returns the category and the category name"
  [theory pred lang]
  (let [category_key (-> theory :language pred :category)
        category_name (-> theory :language category_key :text lang)]
    [category_key category_name]))

(defn- get-widgets
  "Returns the appropriate widget. This is for backward compatibility.
The type field is now used instead of the widget field, in the theory. On the client-side
widget is still used. New Types of :string maps to :widgets 'text."
  [predicate]
  (or (:widgets predicate)
      (replace {:string 'text} [(or (:type predicate) :string)])))

(defn get-answers-choice-for-grounded-predicate
  "Builds the data for a yes/no question of a predicate."
  [theory stmt lang default-fn]
  (let [yes (get-in theory [:language 'yes :text lang])
        no (get-in theory [:language 'no :text lang])
        maybe (get-in theory [:language 'maybe :text lang])]
    (merge {:answers [yes no maybe]
            :formalanswers ['yes 'no 'maybe]
            :grounded true
            :widgets '[radio]}
           (default-fn stmt true))))

(defn get-answers-choice-for-predicate-helper
  "Builds the data for a generic question of a predicate."
  [theory stmt lang default-fn]
  (let [predicate (get-predicate stmt theory)
        formalanswers (-> predicate :answers)
        termargs (term-args stmt)
        ;; filter out answer for grounded variables (since they are not asked)
        formalanswers (keep (fn [[term answer]] (when (variable? term) answer)) (partition 2 (interleave termargs formalanswers)))
        answers (map (fn [sym] (-> theory :language sym :text lang)) formalanswers)
        widgets (get-widgets predicate)
        widgets (keep (fn [[term widget]] (when (variable? term) widget)) (partition 2 (interleave termargs widgets)))]
    (merge {:answer answers
            :formalanswers formalanswers
            :grounded false
            :widgets widgets}
           (default-fn stmt false))))

(defn- get-answers-choices-for-predicate
  [theory stmt lang default-fn]
  (prn "[get-answers-choices-for-predicate] lang=" lang)
  (let [predicate (get-predicate stmt theory)
        arity (scheme/get-arity predicate)]
    (if (or (ground? stmt) (zero? arity))
      (get-answers-choice-for-grounded-predicate theory stmt lang default-fn)
      (get-answers-choice-for-predicate-helper theory stmt lang default-fn))))

(defn get-typename
  [type theory lang]
  (if (coll? type)
    (map (fn [t]
           (get-in theory [:language t :text lang]))
         type)
    (get-in theory [:language type :text lang])))

(defn get-default-role-answer
  [stmt]
  (let [obj (second (term-args stmt))]
   (when (not (variable? obj))
     obj)))

(defn get-answers-choices-for-role
  [theory stmt lang default-fn]
  (let [predicate (get-predicate stmt theory)
        {:keys [min max type]} predicate
        typename (get-typename type theory lang)
        yes (get-in theory [:language 'yes :text lang])
        no (get-in theory [:language 'no :text lang])
        maybe (get-in theory [:language 'maybe :text lang])
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
  [theory stmt lang default-fn]
  (let [predicate (get-predicate stmt theory)]
    (cond (scheme/role? predicate)
          (get-answers-choices-for-role theory stmt lang default-fn)
          (or (scheme/predicate? predicate)
              (scheme/concept? predicate))
          (get-answers-choices-for-predicate theory stmt lang default-fn)
          :else (throw (Exception. (str "NYI:" predicate))))))

(defn get-first-question
  [id stmt lang theory default-fn]
  (let [pred (literal-predicate stmt)
        predicate ((:language theory) pred)
        text (get-question-text stmt theory lang)
        [category category-name] (get-category theory pred lang)
        hint (get-hint predicate lang)
        answers-choices (get-answers-choices theory stmt lang default-fn)]
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
  [id stmt lang theory default-fn]
  (let [pred (literal-predicate stmt)
        first-question (get-first-question id stmt lang theory default-fn)
        [category category-name] (get-category theory pred lang)
        pred-by-categories (group-by (fn [[pred val]] (:category val)) (-> theory :language))
        other-preds (disj (set (map first (pred-by-categories category))) pred)
        other-questions (get-other-questions id stmt other-preds lang theory default-fn)
        ]
    (cons first-question other-questions)))

(defn get-other-questions
  [id stmt others lang theory default-fn]
  (map (fn [id pred]
         (let [predicate (get-in theory [:language pred])
               arity (scheme/get-arity predicate)
               [_ o _] stmt
               stmtq (cond (scheme/role? predicate) (list (:symbol predicate) o (genvar))
                           (scheme/concept? predicate) (list (:symbol predicate) o)
                           :else (doall (cons (symbol pred) (repeatedly arity genvar))))]
           (get-first-question id stmtq lang theory default-fn)))
       (iterate inc (inc id)) others))

(defn get-structured-questions
  "Returns the data necessary for the web client to build the questions.
default-fn is a function returning the default formalized answer for a question."
  ([stmt lang last-id theory]
     (get-structured-questions stmt lang last-id theory (constantly nil)))
  ([stmt lang last-id theory default-fn]
     (let [id (inc last-id)
           pred (literal-predicate stmt)
           questions (get-questions id stmt (keyword lang) theory default-fn)]
       [questions (+ last-id (count questions))])))

(defn askable?
  [theory p]
  ;; {:pre [(do
  ;;          (prn "                ASKABLE:" p)
  ;;          true)]
  ;;  :post [(do
  ;;           (prn "                ===>" %)
  ;;           true)]}
  (let [predicate (get-in theory [:language (literal-predicate p)])]
    (and (literal-pos? p)
         (or (:askable predicate) (:widgets predicate))
         (or (scheme/predicate? predicate)
             (scheme/concept? predicate)
             (and (scheme/role? predicate)
                  (ground? (first (term-args p))))))))

(defn askable-statements-atoms
  "Returns the list of askable statements in the ag stored in db"
  [db theory]
  (db/with-db (db/make-database-connection db "guest" "")
    (let [statements (db/list-statements)]
      (map :atom (filter (partial askable? theory) statements)))))

(defn remove-superfluous-questions
  [questions theory]
  (first
   (reduce (fn [[questions seen] question]
             (let [p (literal-predicate (:statement question))
                   pred (get-predicate (:statement question) theory)]
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
  [ag theory atoms-by-pred stmt grounded]
  {:post [(do (prn "default values for " stmt " is ") (prn %) true)]}
  (let [predicate (get-predicate stmt theory)
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
  [db theory-name lang]
  (let [theory (policy/policies (symbol theory-name))
        ag (export-to-argument-graph (db/make-database-connection db "guest" ""))
        atoms (askable-statements-atoms db theory)
        atoms (put-atoms-being-accepted-in-the-front ag atoms)
        atoms-by-pred (group-by first atoms)
        ;; keep only one atom per predicate
        atoms (map first (vals atoms-by-pred))
        questions (map-indexed (fn [idx atom]
                                 (get-first-question
                                  idx
                                  atom
                                  lang
                                  theory
                                  (partial get-default-values ag theory atoms-by-pred)))
                               atoms)
        questions (remove-superfluous-questions questions theory)]
    questions))

(defn smart-update-statement
  "Updates the literal in the db with a new weight.
If the literal corresponds to a functional role in the theory, set
all other roles having the same predicate but different objects to a weight
of 0.0"
  [literal weight theory]
  (let [id (db/get-statement literal)]
   (db/update-statement id {:weight weight})
   (let [pred (get-in theory [:language (literal-predicate literal)])]
     (when (and (scheme/role? pred)
                (scheme/functional-role? pred)
                (set? (:type pred)))
       ;; exclude the other possible values
       (let [[_ _ old-obj] literal]
        (doseq [obj (disj (:type pred) old-obj)]
          (let [other-role (scheme/replace-role-obj literal obj)
                other-id (db/get-statement other-role)]
            (db/update-statement other-id {:weight 0.0}))))))))

(defn update-statements-weights
  "Updates the weight of the statements in the db."
  [db username password statements theory]
  (let [dbconn (db/make-database-connection db username password)]
   (db/with-db dbconn
     (doseq [[literal weight] statements]
       (smart-update-statement literal weight theory)))))

(defn modify-statements-weights
  "Updates the values of the given statements in the db.
Statements are represented as a collection of [statement value] element."
  [db username password statements theory]
  (update-statements-weights db username password statements theory)
  (evaluate-graph db username password))

(defn pseudo-delete-statements
  "Set the weight of the statements to 0.5 in the db."
  [db username password ids]
  (let [dbconn (db/make-database-connection db username password)]
   (db/with-db dbconn
     (doseq [id ids]
       (db/update-statement id {:weight 0.5})))))