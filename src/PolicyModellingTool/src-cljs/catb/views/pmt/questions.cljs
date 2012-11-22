(ns catb.views.pmt.questions
  (:use [jayq.util :only [log clj->js]]
        [jayq.core :only [$ append]])
  (:require [clojure.string :as s]))

(defn get-radio-widget-html
  "Generates a radio widget HTML code for a question"
  [question]
  (apply str 
         (map (fn [formalanswer answer]
                (format "<input id=\"iq%s\" class=\"radiobutton inputfield required\" name=\"inputq%s\" value=\"%s\" type=\"radio\"/>%s  "
                        (.-id question) (.-id question) formalanswer answer))
              (.-formalanswers question)
              (.-answers question))))

(defn get-answer-widget-html
  "Returns the HTML code for the widget of a question"
  [question widget-type]
  (condp = widget-type
    "radio" (get-radio-widget-html question)
    "text" (format "<input class=\"inputfield required\" type=\"text\" name=\"%s\" /> "
                   (gensym "text"))))

(defn get-answer-widgets-html
  "Returns the HTML code for the widgets of a question"
  [question]
  (doall
   (map (fn [widget-type]
              (get-answer-widget-html question widget-type))
        (.-widgets question))))

(defn generate-ungrounded-question-html
  "Generates the HTML for an ungrounded n-ary predicate"
  [question]
  (let [widgets (atom (get-answer-widgets-html question))
        text-with-widgets 
        (s/replace (.-text question)
                   #"\?\w+"
                   (fn [& _]
                     (let [widget (first (deref widgets))]
                       (swap! widgets next)
                       widget)))]
    (format "<div id=\"q%s\">%s</div> " (.-id question) text-with-widgets)))

(defn generate-question-html
  "Generates the HTML for a question"
  [question]
  (if (.-yesnoquestion question)
    (str (format "<div id=\"q%s\"> " (.-id question))
         (.-text question)
         (get-answer-widget-html question (aget (.-widgets question) 0))
         "</div>")
    (generate-ungrounded-question-html question)))

(defn add-question-html
  "Adds one question to the list of questions"
  [question questionslist]
  (append questionslist (format "<p><i>%s</i></p>"
                               (or (.-hint question) "")))
  (append questionslist (generate-question-html question))
  (append questionslist "<br/>"))

(defn ^:export show-questions
  "Adds the HTML for the questions to the list of questions div"
  [questions questionslist onsubmit]
  (log questions)
  (log questionslist)

  (append questionslist (format "<h2>%s</h2>"
                               (.-category_name (first questions))))
  (doseq [q questions]
    (add-question-html q questionslist))

  (let [button-id (str (gensym "button"))]
    (append questionslist (format "<input type=\"button\" value=\"%s\" id=\"%s\"/> "
                                  (js/jQuery.i18n.prop "pmt_submit")
                                  button-id))
    (append questionslist "<hr/>")
    (.click ($ (str "#" button-id)) onsubmit)
    (.validate ($ "#questionsform"))
    (.scrollTo js/jQuery "100%")
    false))

