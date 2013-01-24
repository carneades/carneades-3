;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns catb.views.pmt.questions
  (:use [jayq.util :only [log clj->js]]
        [jayq.core :only [$ append]]
        [catb.i18n :only [i18n]])
  (:require [clojure.string :as s]))

(defn get-radio-widget-html
  "Generates a radio widget HTML code for a question"
  [question]
  (apply str 
         (map (fn [formalanswer answer]
                (format "<input id=\"iq%s\" class=\"radiobutton inputfield required\" name=\"inputq%s\" value=\"%s\" type=\"radio\"/>%s"
                        (:id question) (:id question) formalanswer answer))
              (:formalanswers question)
              (:answers question))))

(defn get-answer-widget-html
  "Returns the HTML code for the widget of a question"
  [question widget-type]
  (condp = widget-type
    "radio" (get-radio-widget-html question)
    "text" (format "<input class=\"inputfield required\" type=\"text\" name=\"%s\" /> "
                   (gensym "text"))))

(defn select-widget
  "Returns the code for a select widget"
  [values names]
  (str (format "<select type=\"select\" class=\"combobox required\"> ")
       (apply str
              (map (fn [value name]
                     (format "<option class=\"dropdown-menu inputfield\" value=\"%s\">%s</option>"
                             value name))
                   values names))
       "</select>"))

(defn radio-widget
  "Returns the code for a radio widget"
  [values names]
  (let [inputname (gensym "name")]
    (apply str
           (map (fn [value name]
                  (format "<input class=\"radiobutton inputfield required\" name=\"%s\" value=\"%s\" type=\"radio\"/>%s  "
                          inputname value name))
                values names))))

(defn get-answer-widgets-html
  "Returns the HTML code for the widgets of a question"
  [question]
  (doall
   (map (fn [widget-type]
              (get-answer-widget-html question widget-type))
        (:widgets question))))

(defn replace-variables-by-widgets
  "Replaces the variables in the text by the HTML content of the widgets"
  [text widgets]
  (let [wid (atom widgets)]
    (s/replace text #"\?\w+"
               (fn [& _]
                 (let [w (first (deref wid))]
                   (swap! wid next)
                   w)))))

(defn get-ungrounded-question-html
  "Generates the HTML for an ungrounded n-ary predicate"
  [question]
  (let [widgets (get-answer-widgets-html question)
        text-with-widgets (replace-variables-by-widgets
                           (:text question)
                           widgets)]
    (format "<div id=\"q%s\">%s</div> " (:id question) text-with-widgets)))

(defn functional?
  [question]
  (and (= (:min question) 1) (= (:max question) 1)))

(defn get-yes-no-question-html
  [question]
  (str (format "<div id=\"q%s\"> " (:id question))
       (s/capitalize (:text question))
       (get-answer-widget-html question "radio")
       "</div>"))

(defn widget-for-role
  "Returns the widget for a role question"
  [question]
  (cond (and (functional? question) (coll? (:type question)))
        (select-widget (:type question) (:typename question))
        :else (throw "NYI")))

(defn get-role-question-html
  "Returns the HTML of the question for a role"
  [question]
  (let [capitalized-text (s/capitalize (:text question))
        content (if (:yesnoquestion question)
                  (str capitalized-text (radio-widget '[yes no maybe]
                                                      (:answers question)
                                                      ;; ["Yes" "No" "Maybe"]
                                                      ))
                  (replace-variables-by-widgets
                  capitalized-text
                  [(widget-for-role question)]))]
    (format "<div id=\"q%s\">%s</div>" (:id question) content)))

(defn get-concept-question-html
  "Returns the HTML of the question for a concept"
  [question]
  (let [capitalized-text (s/capitalize (:text question))
        content (if (:yesnoquestion question)
                  (str capitalized-text (radio-widget '[yes no maybe] (:answers question)
                                                      ;; ["Yes" "No" "Maybe"]
                                                      ))
                  (replace-variables-by-widgets
                   capitalized-text
                   ;; TODO: check if widget-for-role is ok for concept
                   [(widget-for-role question)]))]
    (format "<div id=\"q%s\">%s</div>" (:id question) content)))

(defn get-question-html
  "Generates the HTML for a question"
  [question]
  (cond (:role question) (get-role-question-html question)
        (:concept question) (get-concept-question-html question)
        (:yesnoquestion question) (get-yes-no-question-html question)
        :else (get-ungrounded-question-html question)))

(defn set-default-value
  "Sets the default value for the question, if one is provided."
  [question]
  (when (:default question)
    (if (:yesnoquestion question)
      (let [el ($ (str "#q" (:id question)
                       " input[value='"
                       (:default question) "']"))]
        (.attr el "checked" true))
      (let [el ($ (str "#q" (:id question) " select"))]
        (.val el (:default question))))))

(defn add-question-html
  "Adds one question to the list of questions"
  [question questionslist]
  (append questionslist (format "<p><i>%s</i></p>"
                               (or (:hint question) "")))
  (append questionslist (get-question-html question))
  (set-default-value question)
  (append questionslist "<br/>"))

(defn add-questions-html
  "Generates HTML for the questions and add them to the list."
  [questions questionslist]
  (log questions)

  (append questionslist (format "<h3>%s</h3>"
                               (.-category_name (first questions))))
  (doseq [q questions]
    (add-question-html (js->clj q :keywordize-keys true) questionslist)))

(defn add-submit-button
  [questionslist onsubmit]
  (let [button-id (str (gensym "button"))]
    (append questionslist (format "<input type=\"button\" value=\"%s\" id=\"%s\"/> "
                                  (i18n "pmt_submit")
                                  button-id))
    (append questionslist "<hr/>")
    (.click ($ (str "#" button-id)) onsubmit)))

(defn ^:export show-questions
  "Adds the HTML for the questions to the list of questions div."
  [questions questionslist onsubmit]
  (add-questions-html questions questionslist)
  (add-submit-button questionslist onsubmit)
  (.validate ($ "#questionsform"))
  (js/PM.scroll_to_bottom)
  false)
