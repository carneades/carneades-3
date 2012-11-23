(ns ^{:doc "Get questions and translation from formal logic statements to human languages"}
  impact.web.logic.questions
  (:use clojure.data.json
        clojure.pprint
        impact.web.core
        carneades.engine.statement
        [carneades.engine.unify :only (genvar apply-substitutions)]
        impact.web.logic.translate)
  (:require [clojure.string :as s]
            [carneades.engine.scheme :as scheme]))

(defn functional?
  [role]
  (and (= (:min role) 1) (= (:max role) 1)))

(defn- get-question-text
  [stmt theory lang]
  (let [predicate (get-predicate stmt theory)]
    (cond (and (scheme/role? predicate)
               (functional? predicate)
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

(defn get-predicate
  [stmt theory]
  (let [pred (literal-predicate stmt)]
    (-> theory :language pred)))

(defn- get-answers-choices-for-predicate
  [theory stmt lang]
  (prn "[get-answers-choices-for-predicate]")
  (let [predicate (get-predicate stmt theory)
        arity (scheme/get-arity predicate)]
    (if (or (ground? stmt) (zero? arity))
      {:answers ["Yes" "No" "Maybe"] :formalanswers ['yes 'no 'maybe] :yesnoquestion true :widgets '[radio]}
      (let [formalanswers (-> predicate :answers)
            termargs (term-args stmt)
            ;; filter out answer for grounded variables (since they are not asked)
            formalanswers (keep (fn [[term answer]] (when (variable? term) answer)) (partition 2 (interleave termargs formalanswers)))
            answers (map (fn [sym] (-> theory :language sym :text lang)) formalanswers)
            widgets (get-widgets predicate)
            widgets (keep (fn [[term widget]] (when (variable? term) widget)) (partition 2 (interleave termargs widgets)))]
        {:answer answers :formalanswers formalanswers :yesnoquestion false :widgets widgets}))))

(defn get-typename
  [type theory lang]
  (if (coll? type)
    (map (fn [t]
           (get-in theory [:language t :text lang]))
         type)
    (get-in theory [:language type :text lang])))

(defn get-answers-choices-for-role
  [theory stmt lang]
  (prn "[get-answers-choices-for-role]")
  (let [predicate (get-predicate stmt theory)
        {:keys [min max type]} predicate
        typename (get-typename type theory lang)]
    ;; TODO: translation of the types
    {:min min
     :max max
     :type type
     :typename typename
     :yesnoquestion (and (not (coll? type)) (ground? stmt))}))

(defn- get-answers-choices
  [theory stmt lang]
  (let [predicate (get-predicate stmt theory)]
    (cond (scheme/role? predicate) (get-answers-choices-for-role theory stmt lang)
          (scheme/predicate? predicate) (get-answers-choices-for-predicate theory stmt lang)
          :else (throw (Exception. (str "NYI:" predicate))))))

(defn get-first-question
  [id stmt lang theory]
  (let [pred (literal-predicate stmt)
        predicate ((:language theory) pred)
        text (get-question-text stmt theory lang)
        [category category-name] (get-category theory pred lang)
        hint (get-hint predicate lang)
        answers-choices (get-answers-choices theory stmt lang)]
    (merge
     {:id id
      :category category
      :category_name category-name
      :hint hint
      ;; :question question ;; DEPRECATED
      :text text
      :statement stmt
      :role (scheme/role? predicate)
      ;; :predicate predicate
      }
     answers-choices)))

(declare get-other-questions)

(defn- get-questions
  [id stmt lang theory]
  (let [pred (literal-predicate stmt)
        first-question (get-first-question id stmt lang theory)
        [category category-name] (get-category theory pred lang)
        pred-by-categories (group-by (fn [[pred val]] (:category val)) (-> theory :language))
        other-preds (disj (set (map first (pred-by-categories category))) pred)
        other-questions (get-other-questions id other-preds lang theory)
        ]
    (cons first-question other-questions)))

(defn get-other-questions
  [id others lang theory]
  (map (fn [id pred]
         (prn "pred =" pred)
         (let [predicate (get-in theory [:language pred])
               arity (scheme/get-arity predicate)
               stmtq (doall (cons (symbol pred) (repeatedly arity genvar)))]
           (get-first-question id stmtq lang theory)))
       (iterate inc (inc id)) others))

(defn get-structured-questions
  [stmt lang last-id theory]
  (prn "[get-structured-questions] stmt =" stmt)
  (let [id (inc last-id)
        pred (literal-predicate stmt)
        questions (get-questions id stmt (keyword lang) theory)]
    [questions (+ last-id (count questions))]))
