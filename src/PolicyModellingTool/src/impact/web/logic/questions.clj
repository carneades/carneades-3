(ns ^{:doc "Get questions and translation from formal logic statements to human languages"}
  impact.web.logic.questions
  (:use clojure.data.json
        clojure.pprint
        impact.web.core
        carneades.engine.statement
        [carneades.engine.unify :only (genvar apply-substitutions)]
        impact.web.logic.translate)
  (:require [clojure.string :as s]))

(defn insert-args
  [question stmt arity]
  ;; we use %n$s in string formats to specify arg orders
  (let [s (rest (literal-atom stmt))]
    (apply format question (map str (take arity s)))))

(defn- get-question-text
  [stmt questiondata lang]
  (let [klang (keyword lang)
        selector (if (ground? stmt) :question :positive)
        question (-> questiondata :forms klang selector)
        notrans (nil? question)
        arity (:arity questiondata)
        question (or question
                     (-> questiondata :forms :en selector))]
    (-> (if notrans
          (s/replace (translate (s/replace question "%s" "_") "en" lang)
                     "_" "%s")
          question)
        (insert-args stmt arity))))

(defn- get-hint
  [questiondata lang]
  (let [klang (keyword lang)
        hint (-> questiondata :hint klang)
        nohint (nil? hint)]
    (if nohint
      (translate (-> questiondata :hint :en) "en" lang)
      hint)))

(defn- get-category
  "Returns the category and the category name"
  [theory pred lang]
  (let [klang (keyword lang)
        category_key (-> theory :language pred :category)
        category_name (-> theory :language category_key :text klang)]
    (if (nil? category_name)
      [category_key (translate category_name "en" lang)]
      [category_key category_name])))

(defn- get-answers-choices
  [theory stmt lang]
  (let [pred (literal-predicate stmt)
        questiondata (-> theory :language pred)
        arity (-> questiondata :arity)]
    (if (or (ground? stmt) (zero? arity))
      ;; TODO: translations of these sentences.
      {:answers ["Yes" "No" "Maybe"] :formalanswers ['yes 'no 'maybe] :yesnoquestion true :widgets '[radio]}
      (let [formalanswers (-> questiondata :answers)
            klang (keyword lang)
            termargs (term-args stmt)
            ;; filter out answer for grounded variables (since they are not asked)
            formalanswers (keep (fn [[term answer]] (when (variable? term) answer)) (partition 2 (interleave termargs formalanswers)))
            answers (map (fn [sym] (-> theory :language sym :text klang)) formalanswers)
            ;; if answers are available in the language take them, otherwise take the english answers
            ;; and translate them on the fly
            answers (if (seq answers)
                      answers
                      (map #(translate % "en" lang)
                           (map (fn [sym] (-> theory :language :en :text lang)) formalanswers)))
            widgets (-> questiondata :widgets)
            widgets (keep (fn [[term widget]] (when (variable? term) widget)) (partition 2 (interleave termargs widgets)))]
        {:answer answers :formalanswers formalanswers :yesnoquestion false :widgets widgets}))))

(defn- get-question
  [id stmt lang theory]
  (let [pred (literal-predicate stmt)
        questiondata ((:language theory) pred)
        question (get-question-text stmt questiondata lang)
        [category category-name] (get-category theory pred lang)
        hint (get-hint questiondata lang)
        answers-choices (get-answers-choices theory stmt lang)]
    (merge
     {:id id
      :category category
      :category_name category-name
      :hint hint
      :question question
      :statement stmt}
     answers-choices)))

(defn- get-followups
  [id stmt lang theory]
  (let [pred (literal-predicate stmt)
        arity (-> (:language theory) pred :arity)
        followups (-> (:language theory) pred :followups)]
    (map (fn [id pred-of-follow]
           (let [arity (-> (:language theory) pred-of-follow :arity)
                 stmtq (doall (cons (symbol pred-of-follow) (repeatedly arity genvar)))]
            (get-question id stmtq lang theory)))
         (iterate inc (inc id)) followups)))

(defn get-structured-questions
  [stmt lang last-id theory]
  (prn "[get-structured-questions] stmt =" stmt)
  (let [id (inc last-id)
        pred (literal-predicate stmt)
        question (get-question id stmt lang theory)
        refsquestions (get-followups id stmt lang theory)
        structured-questions (cons question refsquestions)]
    [structured-questions (+ last-id (count structured-questions))]))
