(ns ^{:doc "Translation from formal logic statements to human languages"}
  impact.web.logic.statement-translation
  (:use clojure.data.json
        clojure.pprint
        carneades.engine.statement
        impact.web.logic.translate)
  (:require [clojure.xml :as xml]
            [clojure.contrib.zip-filter.xml :as zf]
            [clojure.zip :as zip]))

(defn load-questions
  [url]
  (prn "slurping " url)
  (load-string (slurp url))
  (ns-resolve 'resources.public.kb.questions 'questions))

(defn insert-args
  [question stmt]
  (let [s (rest (statement-atom stmt))
        argnumbers (count (re-seq #"%s" question))]
    (apply format question (map str (take argnumbers s)))))

(defn- get-answers
  [loc lang]
  (let [answers (zf/xml-> loc :question :answers :text (zf/attr= :lang lang) zf/text)]
    ;; if answers are available in the language take them, otherwise take the english answers
    ;; and translate them on the fly
    (if (seq answers)
      answers
      (map #(translate % "en" lang) (zf/xml-> loc :question :answers :text (zf/attr= :lang "en") zf/text)))))

(defn- get-question-text
  [stmt questiondata lang]
  (prn "questiondata =")
  (pprint questiondata)
  (prn "plop")
  (let [klang (keyword lang)
        question (-> questiondata :forms klang :question)
        notrans (nil? question)
        question (or question
                     (-> questiondata :forms klang :question))
        formated-question (insert-args question stmt)]
    (if notrans
      (translate formated-question "en" lang)
      formated-question)))

(defn- get-hint
  [loc lang]
  "todo")

(defn- get-question
  [id stmt lang questions]
  (let [questiondata (questions (statement-predicate stmt))
        question (get-question-text stmt questiondata lang)
        category "category"
        optional false
        hint (get-hint nil lang)
        type "text"
        formalanswers []
        answers [] ;; (get-answers loc lang)
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

(defn get-loc
  [stmt translations]
  (zf/xml1-> translations :predicate (zf/attr= :pred stmt)))

(defn get-structured-questions
  [stmt lang last-id questions]
  ;; (prn "GET STRUCTURED QUESTION FOR =")
  ;; (prn stmt)
  (let [id (inc last-id)
        pred (statement-predicate stmt)
        question (get-question id stmt lang questions)
        ;; refs (zf/xml-> loc :question :qrefs :qref (zf/attr :pred))
        ;; locs (map #(get-loc % translations) refs)
        ;; ;; TODO: reference statements with more than one argument
        refsquestions () ;; (map (fn [id loc stmt] (get-question id (list (symbol stmt) '?x) loc lang translations)) (iterate inc (inc id)) locs refs)
        questions (apply list question refsquestions)]
    [questions (+ last-id (count questions))]))
