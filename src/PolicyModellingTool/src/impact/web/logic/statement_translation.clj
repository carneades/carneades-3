(ns ^{:doc "Translation from formal logic statements to human languages"}
  impact.web.logic.statement-translation
  (:use clojure.data.json
        clojure.pprint
        impact.web.core
        carneades.engine.statement
        [carneades.engine.unify :only (genvar apply-substitutions)]
        impact.web.logic.translate)
  (:require [clojure.string :as s]))

(defn insert-args
  [question stmt]
  ;; we should use %n$s in string formats to specify arg orders?
  (let [s (rest (literal-atom stmt))
        argnumbers (count (re-seq #"%s" question))]
    (apply format question (map str (take argnumbers s)))))

(defn- get-question-text
  [stmt questiondata lang]
  (let [klang (keyword lang)
        question (-> questiondata :forms klang :question)
        notrans (nil? question)
        question (or question
                     (-> questiondata :forms :en :question))]
    (-> (if notrans
          (s/replace (translate (s/replace question "%s" "_") "en" lang)
                     "_" "%s")
          question)
        (insert-args stmt))))

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
    (if (zero? arity)
      [["Yes" "No" "Maybe"] ['yes 'no 'maybe] true] ;; TODO: translations of these sentences?
      (if-let [formalanswers (-> questiondata :answers)]
        (let [klang (keyword lang)
              answers (map (fn [sym] (-> theory :language sym :text klang)) formalanswers)]
          ;; if answers are available in the language take them, otherwise take the english answers
          ;; and translate them on the fly
          (if (seq answers)
            [answers formalanswers false]
            [(map #(translate % "en" lang)
                  (map (fn [sym] (-> theory :language :en :text lang)) formalanswers))
             formalanswers]))
        [[] [] false]))))

(defn- get-question
  [id stmt lang theory]
  (let [pred (literal-predicate stmt)
        questiondata ((:language theory) pred)
        question (get-question-text stmt questiondata lang)
        [category category-name] (get-category theory pred lang)
        hint (get-hint questiondata lang)
        arity (-> questiondata :arity)
        [answers formalanswers yesnoquestion] (get-answers-choices theory stmt lang)
        widget (if yesnoquestion "radio" (-> questiondata :widget))]
    {:id id
     :category category
     :category_name category-name
     :hint hint
     :widget widget
     :question question
     :statement stmt
     :arity arity
     :formalanswers formalanswers
     :answers answers
     :yesnoquestion yesnoquestion}))

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
  [stmt substitutions lang last-id theory]
  (prn "[get-structured-questions] stmt =" stmt)
  (let [stmt stmt;; (apply-substitutions substitutions stmt)
        id (inc last-id)
        pred (literal-predicate stmt)
        question (get-question id stmt lang theory)
        refsquestions (get-followups id stmt lang theory)
        structured-questions (cons question refsquestions)]
    [structured-questions (+ last-id (count structured-questions))]))
