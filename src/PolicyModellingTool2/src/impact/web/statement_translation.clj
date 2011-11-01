(ns ^{:doc "Translation from formal logic statements to human languages"}
  impact.web.statement-translation
  (:use carneades.engine.statement
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
  [question stmt args]
  (let [s (rest (statement-atom stmt))]
    (apply format question (take (count args) s))))

(defn- get-question
  [id stmt loc lang translations]
  (let [question (zf/xml1-> loc :question :format (zf/attr= :lang lang) zf/text)
        category (zf/xml1-> loc :question :category zf/text)
        optional false
        hint (zf/xml1-> loc :question :hint zf/text)
        type (zf/xml1-> loc :question (zf/attr :type))
        formalanswers (zf/xml-> loc :question :formalanswers :text zf/text)
        answers (zf/xml-> loc :question :answers :text (zf/attr= :lang lang) zf/text)
        ;; TODO: arg positioning is irrelevant,
        ;; we should use %n$s in string formats to specify arg orders
        argnumbers (zf/xml-> loc :question :format (zf/attr= :lang lang) :args :arg (zf/attr :nr))
        q {:id id
           :category category
           :optional optional
           :hint hint
           :type type
           :question (insert-args question stmt argnumbers)
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
