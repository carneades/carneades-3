(ns catb.views.pmt.questions
  (:use [jayq.util :only [log clj->js]]
        [jayq.core :only [$ append]]))

(defn get-radio-widget-html
  [question]
  (apply str 
         (map (fn [formalanswer answer]
                (format "<input id=\"iq%s\" class=\"radiobutton inputfield required\" name=\"inputq%s\" value=\"%s\" type=\"radio\"/>%s  "
                        (.-id question) (.-id question) formalanswer answer))
              (.-formalanswers question)
              (.-answers question))))

(defn get-answer-widget-html
  [question]
  (let [widget-type (aget (.-widgets question) 0)]
    (condp = widget-type
      "radio" (get-radio-widget-html question))))

(defn generate-question-html
  "Generates the HTML for a question"
  [question questionslist]
  (if (.-yesnoquestion question)
    (str (format "<div id=\"q%s\"> " (.-id question))
         (.-text question)
         (get-answer-widget-html question)
         "</div>")
    (log "failed question")))

(defn add-question-html
  "Adds one question to the list of questions"
  [question questionslist]
  (append questionslist (format "<p><i>%s</i></p>"
                               (or (.-hint question) "")))
  (append questionslist (generate-question-html question questionslist))
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

