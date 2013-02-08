;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns catb.views.pmt.questions
  (:use [jayq.util :only [log clj->js]]
        [jayq.core :only [$ append empty inner]]
        [catb.i18n :only [i18n]]
        [catb.views.core :only [template]])
  (:require [clojure.string :as s]
            [catb.backbone.core :as bb]
            [catb.dispatch :as dispatch])
  (:require-macros [catb.backbone.macros :as bb]
                   [catb.views.menu :as menu]))

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

(defn add-facts-buttons
  [question]
  (if (not= (:max question) 1)
    "&nbsp;&nbsp;<img class=\"remove-fact fact-button\" src=\"images/list-remove.png\"/>
<img class=\"add-fact fact-button\" src=\"images/list-add.png\"/>"
    ""))

(defn get-yes-no-question-html
  [question]
  (str (format "<div id=\"q%s\">" (:id question))
       (s/capitalize (:text question))
       (get-answer-widget-html question "radio")
       (format "%s</div>" (add-facts-buttons question))))

(defn widget-for-role
  "Returns the widget for a role question"
  [question]
  (cond (and (functional? question) (coll? (:type question)))
        (select-widget (:type question) (:typename question))
        :else (throw "NYI")))

(def questions (atom {:questions [] ;; ids, stored in the order of the categories
                      :questions-by-id {} ;; by id, referencing a questions
                      :latest-questions [] ;; latests ids
                      }))

(defn create-role-answers-fetcher
  "Creates a function that will returns the answers of the role question."
  [question]
  (fn []
    (let [el ($ (str "#q" (:id question) " select"))]
      [(.val el)])))

(defn add-fetcher
  [questions id fetcher]
  (assoc-in questions [:questions-by-id id :fetch-answers] fetcher))

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
                  [(widget-for-role question)]))
        fetcher (create-role-answers-fetcher question)]
    (swap! questions add-fetcher (:id question) fetcher)
    (format "<div id=\"q%s\"><div>%s %s</div></div>" (:id question) content (add-facts-buttons question))))

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
    (format "<div id=\"q%s\"><div>%s %s</div></div>" (:id question) content (add-facts-buttons question))))

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

(defn get-question-el
  [question]
  ($ (str "#q" (:id question))))

(defn add-fact
  [question event]
  (let [el (get-question-el question)
        div (.find el "div:last")]
   (append div (get-question-html question))))

(defn add-facts-number-listener
  "Adds listeners implementing the addition or the suppression of facts."
  [question]
  (when-let [el ($ (str "#q" (:id question) " .add-fact"))]
    (.click el (partial add-fact question)))
  (when-let [el ($ (str "#q" (:id question) " .remove-fact"))]
    (.click el (fn [] (js/alert "remove")))))

(defn add-question-html
  "Adds one question to the list of questions"
  [question el]
  (append el (format "<p><i>%s</i></p>"
                               (or (:hint question) "")))
  (append el (get-question-html question))
  (add-facts-number-listener question)
  (set-default-value question)
  (append el "<br/>"))

(defn add-questions-html
  [questions el]
  (doseq [question questions]
    (add-question-html question el)))

(defn add-submit-button
  [questions-el]
  (let [button-id (str (gensym "button"))]
    (append questions-el (format "<input type=\"button\" value=\"%s\" id=\"%s\"/> "
                                  (i18n "pmt_submit")
                                  button-id))
    (append questions-el "<hr/>")
    (.click ($ (str "#" button-id)) (fn [_]
                                      (dispatch/fire :on-submit {})))))

(defn display-questions-in-category
  "Displays all the question of a category."
  [questions el]
  (let [category_name (:category_name (first questions))]
    (append el (format "<h3>%s</h3>" category_name))
    (add-questions-html questions el)
    (add-submit-button el)))

(defn display-questions
  "Displays all the questions."
  [msg]
  (let [el ($ "#questions")
        questions-to-ids (:questions-by-id (deref questions))
        ids (:questions (deref questions))]
    (empty el)
    (doseq [ids-for-category ids]
      (display-questions-in-category (map questions-to-ids ids-for-category) el))))

(dispatch/react-to #{:questions-added}
                   (fn [_ msg]
                     (display-questions msg))) 
 

(defn- questions-list->map
  "Converts a list of questions to a map indexing them by id."
  [questions-list]
  (reduce (fn [m question]
            (assoc m (:id question) question))
          {}
          questions-list))

(defn- questions-ordered
  "Returns an ordered list of questions' ids according to their category."
  [questions-list]
  (map #(map :id %) (partition-by :category questions-list)))

(defn- add-questions
  "Adds the list of new questions to the questions map."
  [questions latest-questions-list]
  (-> questions
      (update-in [:questions-by-id] merge (questions-list->map latest-questions-list))
      (assoc :latest-questions (map :id latest-questions-list))
      (update-in [:questions] concat (questions-ordered latest-questions-list))))

(defn fetch-answers
  "Fetches the answer of a question and sets their value in the question map."
  [qid]
  (get-in (deref questions) [:questions-by-id qid :fetch-answers])
  (let [answers ((get-in (deref questions) [:questions-by-id qid :fetch-answers]))]
    (swap! questions assoc-in [:questions-by-id qid :answers] answers)
    answers))

(defn fetch-latest-questions-answers
  "Fetches the answers of the latest questions.
Returns the answers indexed by question's id."
  []
  (let [latest (:latest-questions (deref questions))
        answers (map fetch-answers latest)]
    (map (fn [id ans] {:id id :values ans}) latest answers)))

(declare show-questions-or-ag)

(defn- send-answers
  [msg]
  (when (.valid ($ "#questionsform"))
    (let [answers (fetch-latest-questions-answers)]
      (log "sending answers...")
      (log (clj->js answers)) 
      (js/PM.busy_cursor_on)
      (js/PM.ajax_post js/IMPACT.simulation_url
                       (clj->js {:answers answers})
                       (fn [data]
                         (js/PM.busy_cursor_off)
                         (show-questions-or-ag data))
                       js/IMPACT.user
                       js/IMPACT.password
                       js/PM.on_error))))

(dispatch/react-to #{:on-submit} send-answers)

(defn ^:export show-questions
  "Adds the HTML for the questions to the list of questions div."
  [latest-questions-list]
  (let [latest (js->clj latest-questions-list :keywordize-keys true)]
    (swap! questions add-questions latest)
    (dispatch/fire :questions-added {:latest-questions latest}))
  false)

(defn show-ag
  [db]
  (set! js/IMPACT.db db)
  (js/PM.set_arguments_url db))

(defn ^:export show-questions-or-ag
  "Shows the remaining questions to the user or the argument graph if
all questions have been answered."
  [data]
  (if-let [questions-list (.-questions data)]
    (show-questions questions-list)
    (show-ag (.-db data))))