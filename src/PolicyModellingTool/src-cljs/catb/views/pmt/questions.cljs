;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns catb.views.pmt.questions
  (:use [jayq.util :only [log clj->js]]
        [jayq.core :only [$ append empty]]
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
  [question questionslist]
  (log "add-question-html")
  (log (clj->js question))
  (append questionslist (format "<p><i>%s</i></p>"
                               (or (:hint question) "")))
  (append questionslist (get-question-html question))
  (add-facts-number-listener question)
  (set-default-value question)
  (append questionslist "<br/>"))

(defn add-submit-button
  [questionslist onsubmit]
  (let [button-id (str (gensym "button"))]
    (append questionslist (format "<input type=\"button\" value=\"%s\" id=\"%s\"/> "
                                  (i18n "pmt_submit")
                                  button-id))
    (append questionslist "<hr/>")
    (.click ($ (str "#" button-id)) onsubmit)))

(bb/defview QuestionFact
  ;; a question's answered is composed of one or more facts
  :className "question-fact"
  :events {"submit" :on-submit}
  
  :on-submit
  ([e]
     (log e)
     (js/alert "TODO: update 'values' variable"))

  :calculate-answer
  ([]
     (log "calculate answer")
     [1 42])
  
  :render
  ([]
     (bb/with-attrs [:question :nb-facts]
       (log "adding fact for question")
       (log question)
       (add-question-html (js->clj question :keywordize-keys true) (.-$el this)))))

(bb/defview Question
  :className "question"
  :calculate-answer
  ([]
     (let [qfacts-views (aget this "qfacts-views")]
       (map (fn [v] (.-calculate-answer v)) qfacts-views)))
  
  :render
  ([]
     (bb/with-attrs [:question]
       (let [max (.-max question)
             nb (cond (nil? max) 1
                      (zero? max) 1
                      (<= max 5) max
                      :else 1)
             nb-facts (atom nb)
             qfacts-views (repeatedly nb
                                      #(bb/new QuestionFact
                                               {:model
                                                (bb/new-model {:nb-facts nb-facts
                                                               :question question})}))]
         (aset this "qfacts-views" qfacts-views)
         (doseq [qfact-view qfacts-views]
           (append (.-$el this) (.-$el qfact-view))
           (.render qfact-view))))))

(defn category-name
  [question]
  (.-category_name question))

(bb/defview QuestionsList
  :className "questions-list"

  :initialize
  ([attrs]
     (.on (.-model this) "change" (.-render this) this))

  :on-submit
  ([]
     (bb/with-attrs [:onsubmit]
       (js/alert "submit")
       (doseq [qview (aget this "qviews")]
         (let [ans (.calculate-answer qview)]
           (log "ans =")
           (log ans)))))

  :render
  ([]
     (bb/with-attrs [:questions]
       (let [el (.-$el this)
             partitioned-questions (partition-by category-name questions)]
         (empty el)
         (doseq [part partitioned-questions]
           (append el (format "<h3>%s</h3>"
                              (category-name (first part))))
           (let [qviews (map (fn [q]
                               (bb/new Question
                                       {:model (bb/new-model {:question q})}))
                             part)]
             (aset this "qviews" qviews)
             (doseq [qview qviews]
               (.append el (.-$el qview))
               (.render qview))))
         (add-submit-button el (.-on_submit this))))))

(def questions-list (atom nil))

(def questions-list-model (bb/new-model {:questions []}))

(defn ^:export show-questions
  "Adds the HTML for the questions to the list of questions div."
  [questions questionslist onsubmit]
  (when (nil? (deref questions-list))
    (.set questions-list-model "onsubmit" onsubmit)
    (reset! questions-list (bb/new QuestionsList
                                   {:model questions-list-model
                                    :el ($ "#questions")})))
  (let [old-questions (.get questions-list-model "questions")]
    (.set questions-list-model "questions" (.concat old-questions questions)))
  ;; (.validate ($ "#questionsform"))
  (js/PM.scroll_to_bottom)
  false)
