(ns ^{:doc "Translation from formal logic statements to human languages"}
  impact.web.logic.statement-translation
  (:use clojure.data.json
        clojure.pprint
        carneades.engine.statement
        impact.web.logic.translate)
  (:require [clojure.xml :as xml]
            [clojure.contrib.zip-filter.xml :as zf]
            [clojure.zip :as zip]
            [clojure.string :as s]))

(defn load-questions
  "Dynamically loads the questions data written in Clojure"
  [url]
  (load-string (slurp url))
  (deref (ns-resolve 'resources.public.kb.questions 'questions)))

(defn insert-args
  [question stmt]
  (prn "question =")
  (prn question)
  (let [s (rest (statement-atom stmt))
        argnumbers (count (re-seq #"%s" question))]
    (apply format question (map str (take argnumbers s)))))

(defn- get-answers
  [questiondata lang]
  (let [klang (keyword lang)
        answers (-> questiondata :answers klang)]
    ;; if answers are available in the language take them, otherwise take the english answers
    ;; and translate them on the fly
    (if (seq answers)
      answers
      (map #(translate % "en" lang) (-> questiondata :answers :en)))))

(defn- get-question-text
  [stmt questiondata lang]
  (let [klang (keyword lang)
        question (-> questiondata :forms klang :question)
        notrans (nil? question)
        question (or question
                     (-> questiondata :forms :en :question))
        ;; formated-question (insert-args question stmt)
        ]
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
  [questiondata lang]
  (let [klang (keyword lang)
        cat (-> questiondata :category klang)
        nocat (nil? cat)]
    (if nocat
      (translate (-> questiondata :category :en) "en" lang)
      cat)))

(defn- get-question
  [id stmt lang questions]
  (let [questiondata (questions (statement-predicate stmt))
        question (get-question-text stmt questiondata lang)
        category (get-category questiondata lang)
        optional false
        hint (get-hint questiondata lang)
        type (-> questiondata :type)
        ;; TODO rename formal answers into machine answers
        formalanswers (-> questiondata :answers :machine)
        answers (get-answers questiondata lang)
        ;; TODO: arg positioning is irrelevant,
        ;; we should use %n$s in string formats to specify arg orders
        q {:id id
           :category category
           :optional optional
           :hint hint
           :type type
           :question question
           :statement stmt}]
    (assoc q :formalanswers formalanswers :answers answers)))

(defn- get-followups
  [id stmt lang questions]
  (let [pred (statement-predicate stmt)
        followups (-> questions pred :followups)]
    (map (fn [id stmt]
           (get-question id (list (symbol stmt) '?x) lang questions))
         (iterate inc (inc id)) followups)))

(defn get-structured-questions
  [stmt lang last-id questions]
  (let [id (inc last-id)
        pred (statement-predicate stmt)
        question (get-question id stmt lang questions)
        refsquestions (get-followups id stmt lang questions)
        structured-questions (cons question refsquestions)]
    [structured-questions (+ last-id (count structured-questions))]))
