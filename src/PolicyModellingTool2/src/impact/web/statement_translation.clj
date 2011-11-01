(ns ^{:doc "Translation from formal logic statements to human languages"}
  impact.web.statement-translation
  (:use carneades.engine.statement
        impact.web.translate
        clojure.data.json)
  (:require [clojure.xml :as xml]
            [clojure.contrib.zip-filter.xml :as zf]
            [clojure.zip :as zip]))

(defn load-translations
  [url]
  (let [content (xml/parse url)
        z (zip/xml-zip content)]
    z))

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
  [stmt loc lang]
  (let [question (zf/xml1-> loc :question :format (zf/attr= :lang lang) zf/text)
        notrans (nil? question)
        question (or question
                     (zf/xml1-> loc :question :format (zf/attr= :lang "en") zf/text))
        formated-question (insert-args question stmt)]
    (if notrans
      (translate formated-question "en" lang)
      formated-question)))

(defn- get-question
  [id stmt loc lang translations]
  (let [lang "fr"
        question (get-question-text stmt loc lang)
        category (zf/xml1-> loc :question :category zf/text)
        optional false
        hint (zf/xml1-> loc :question :hint zf/text)
        type (zf/xml1-> loc :question (zf/attr :type))
        formalanswers (zf/xml-> loc :question :formalanswers :text zf/text)
        answers (get-answers loc lang)
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
  [stmt lang last-id translations]
  ;; (prn "GET STRUCTURED QUESTION FOR =")
  ;; (prn stmt)
  (let [id (inc last-id)
        loc (get-loc (str (statement-predicate stmt)) translations) 
        question (get-question id stmt loc lang translations)
        refs (zf/xml-> loc :question :qrefs :qref (zf/attr :pred))
        locs (map #(get-loc % translations) refs)
        ;; TODO: reference statements with more than one argument
        refsquestions (map (fn [id loc stmt] (get-question id (list (symbol stmt) '?x) loc lang translations)) (iterate inc (inc id)) locs refs)
        questions (apply list question refsquestions)]
    [questions (+ last-id (count questions))]))
