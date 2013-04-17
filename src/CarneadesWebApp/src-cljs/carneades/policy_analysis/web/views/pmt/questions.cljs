;;; Copyright (c) 2012-2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Displays questions to the user to collect facts."}
  carneades.policy-analysis.web.views.pmt.questions
  (:use [jayq.util :only [log clj->js]]
        [jayq.core :only [$ append empty inner]]
        [carneades.policy-analysis.web.i18n :only [i18n]]
        [carneades.policy-analysis.web.views.core :only [template]])
  (:require [clojure.string :as s]
            [carneades.policy-analysis.web.backbone.core :as bb]
            [carneades.policy-analysis.web.dispatch :as dispatch])
  (:require-macros [carneades.policy-analysis.web.backbone.macros :as bb]
                   [carneades.policy-analysis.web.views.menu :as menu]))

(defn current-fact-value
  "Returns the value of the fact numbered fact-number if there is any."
  [question fact-number]
  (let [col (nth (:values question) fact-number ::not-found)
        v (if (coll? col) (first col) nil)]
    v))

(defn get-radio-widget-html
  "Generates a radio widget HTML code for a question"
  [question fact-number]
  (let [inputid (gensym (format "input-q%s-" (:id question)))]
   (apply str 
          (map (fn [formalanswer answer]
                 (let [checked (if (= (current-fact-value question fact-number) formalanswer)
                                 "checked"
                                 "")]
                   (format "<input class=\"radiobutton inputfield required\" name=\"%s\" value=\"%s\" type=\"radio\" %s/>%s"
                           inputid formalanswer checked answer)))
               (:formalanswers question)
               (:answers question)))))

(defn select-widget
  "Returns the code for a select widget"
  [question fact-number]
  (str (format "<select type=\"select\" class=\"combobox required\"> ")
       (apply str
              (map (fn [value name]
                     (let [selected (if (= (current-fact-value question fact-number) value)
                                      "selected"
                                      "")]
                      (format "<option class=\"dropdown-menu inputfield\" value=\"%s\" %s>%s</option>"
                              value selected name)))
                   (:type question) (:typename question)))
       "</select>"))

(defn input-widget
  "Returns the HTML string for an input."
  [question fact-number]
  (let [value (or (current-fact-value question fact-number) "")]
    (format "<input class=\"inputfield required\" type=\"text\">%s</input>" value)))

(defn radio-widget
  "Returns the code for a radio widget"
  [values names]
  (let [inputname (gensym "name")]
    (apply str
           (map (fn [value name]
                  (format "<input class=\"radiobutton inputfield required\" name=\"%s\" value=\"%s\" type=\"radio\"/>%s  "
                          inputname value name))
                values names))))

(defn replace-variables-by-widgets
  "Replaces the variables in the text by the HTML content of the widgets"
  [text widgets]
  (let [wid (atom widgets)]
    (s/replace text #"\?\w+"
               (fn [& _]
                 (let [w (first (deref wid))]
                   (swap! wid next)
                   w)))))

(defn functional?
  "Returns true if the question is from a function role."
  [question]
  (and (= (:min question) 1) (= (:max question) 1)))

(defn build-facts-buttons
  "Returns a HTML string representing the -/+ button that are used to
  add/remove facts. Returns an empty string if no button should be created."
  [question]
  (if (and (not= (:max question) 1)
           (not (:concept question))
           (not (:grounded question)))
    "&nbsp;&nbsp;<img class=\"remove-fact fact-button\" src=\"images/list-remove.png\"/>
<img class=\"add-fact fact-button\" src=\"images/list-add.png\"/>"
    ""))

(defn widget-for-role
  "Returns the widget for a role question"
  [question fact-number]
  (cond (coll? (:type question))
        (select-widget question fact-number)
        
        (or (= (:type question ) "symbol") (= (:type question ) "string"))
        (input-widget question fact-number)
        
        :else (throw "NYI")))

(defn create-questions-map
  "Creates the map storing information about the questions."
  []
  {:order [] ;; ids, stored in the order of the categories
   :questions {} ;; by id, referencing a questions
   :latest-questions [] ;; latests ids
   :deleted () ;; ids of the statement that have been deleted
   })

(def ^{:doc "Questions ask to the user ordered and indexed."}
  questions (atom (create-questions-map)))

(defn get-question-el
  "Returns the JQuery element of a question."
  [question]
  ($ (str "#q" (:id question))))

(defn add-facts-number-listener
  "Adds listeners implementing the addition or the suppression of facts."
  [question]
  (let [el (get-question-el question)]
    (doseq [add-button (.find el ".add-fact")]
      (let [add-button ($ add-button)]
        (.off add-button "click")
        (.click add-button
                (fn [_] (dispatch/fire :add-fact {:id (:id question)})))))
    (doall
     (map (fn [remove-button idx]
            (let [remove-button ($ remove-button)]
              (.off remove-button "click")
              (.click remove-button
                      (fn [event] (dispatch/fire :remove-fact {:id (:id question)
                                                               :fact-nb idx
                                                               :event event})))))
          (.find el ".remove-fact")
          (range)))))

(defn coll-fetcher
  "Returns the value selected in a collection."
  [question]
  (let [el (get-question-el question)
        selects (.find el "select")]
    (map (fn [select]
           [(.val ($ select))])
         selects)))

(defn string-fetcher
  "Returns the value for a symbol or string."
  [question]
  (let [el (get-question-el question)
        fields (.find el ".inputfield")]
    (map (fn [field]
           [(.val ($ field))])
         fields)))

(defn create-role-values-fetcher
  "Creates a function that will returns the answers of the role question."
  [question]
  (fn []
    (cond (coll? (:type question))
          (coll-fetcher question)

          (or (= (:type question) "symbol") (= (:type question ) "string"))
          (string-fetcher question)
          )))

(defn add-fetcher
  "Adds a values fetcher to the questions for a particular question
  identified by id."
  [questions id fetcher]
  (assoc-in questions [:questions id :fetch-values] fetcher))

(defn build-role-question-fact-html
  "Returns a HTML string representing the fact of a role question."
  [question idx]
  (let [capitalized-text (s/capitalize (:text question))
        widget (widget-for-role question idx)]
    (str "<div>"
         (replace-variables-by-widgets
          capitalized-text
          [widget])
         (build-facts-buttons question)
         "</div>")))

(defn build-role-question-facts-html
  "Returns the HTML for the facts of the role question."
  [question]
  (apply str
         (map #(build-role-question-fact-html question %)
              (range (:nb-facts question)))))

(defn create-role-fact-adder
  "Returns an anonymous function that adds a fact to a role question."
  [question el]
  (fn []
    (let [id (:id question)
          lquestion (get-in (deref questions) [:questions id])
          nb-facts (:nb-facts lquestion)]
      (append el (build-role-question-fact-html question (inc nb-facts)))
      (add-facts-number-listener question)
      (swap! questions update-in [:questions id :nb-facts] inc))))

(defn fact-uuid
  "Returns the UUID of the statement linked to the fact if there is
  any, a non-True value otherwise.
This is used when deleting a fact during the modification phase."
  [question fact-nb]
  (log "question =")
  (log (clj->js question))
  (and
   (< fact-nb (count (:facts-uuid question)))
   ((:facts-uuid question) fact-nb)))

(defn create-fact-remover
  "Creates an anonymous function which will be called when a fact of a
  question is removed."
  [question el]
  (fn [event idx]
    (let [target (.-target event)
          parent (.parent ($ target))
          id (:id question)
          lquestion (get-in (deref questions) [:questions id])]
      (.remove parent)
      (when-let [uuid (fact-uuid lquestion idx)]
        ;; add the uuid to the list of statements to delete
        (swap! questions assoc-in [:questions (:id lquestion) :facts-uuid idx] nil)
        (swap! questions update-in [:deleted] conj uuid))
      (swap! questions update-in [:questions (:id lquestion) :nb-facts] dec))))

(defn build-role-question-html
  "Returns the HTML string of a role question."
  [question]
  (str (format "<div id=\"q%s\">" (:id question))
       (build-role-question-facts-html question)
       "</div>"))

(defn build-role-question
  "Returns the HTML of the question for a role and its helper functions."
  [question]
  {:pre [(not (:grounded question))]}
  (let [el ($ (build-role-question-html question))]
    {:el el
     :fetcher (create-role-values-fetcher question)
     :fact-adder (create-role-fact-adder question el)
     :fact-remover (create-fact-remover question el)}))

(defn build-grounded-question-fact-html
  "Returns a string representing a fact of a yes/no question."
  [question idx]
  (let [text (s/capitalize (:text question))]
   (str (format "<div>%s" text)
        (get-radio-widget-html question idx)
        (build-facts-buttons question)
        "</div>")))

(defn build-grounded-question-facts-html
  "Returns a string representing the facts of a yes/no questions."
  [question]
  (apply str
         (map #(build-grounded-question-fact-html question %)
              (range (:nb-facts question)))))

(defn build-grounded-question-html
  "Returns a string representing a yes/no question."
  [question]
  (str (format "<div id=\"q%s\">" (:id question))
       (build-grounded-question-facts-html question)
       "</div>"))

(defn create-grounded-question-fetcher
  "Returns an anonymous function that will fetch the values of the
  user's answers to a yes/no question."
  [question]
  (fn []
    (let [el (get-question-el question)
          inputs (.find el "input:checked")]
      (map (fn [i] [(.val ($ i))]) inputs))))

(defn create-grounded-fact-adder
  "Creates an anonymous function which will be called when a fact of a
  yes/no question is added."
  [question el]
  (fn []
    (let [id (:id question)
          lquestion (get-in (deref questions) [:questions id])
          nb-facts (:nb-facts lquestion)]
      (append el (build-grounded-question-fact-html question (inc nb-facts)))
      (add-facts-number-listener question)
      (swap! questions update-in [:questions id :nb-facts] inc))))

(defn build-grounded-question
  "Returns the HTML of the question for a yes/no question and its
 helper functions."
  [question]
  (let [el ($ (build-grounded-question-html question))]
   {:el el
    :fetcher (create-grounded-question-fetcher question)
    :fact-adder (create-grounded-fact-adder question el)
    :fact-remover (create-fact-remover question el)}))

(defn build-question
  "Constructs the question HTML and answer fetcher function."
  [question]
  (log "Question")
  (log (clj->js question))
  (cond (:grounded question) (build-grounded-question question)
        (:role question) (build-role-question question)
        ;; (:concept question) (get-concept-question-html question)
        ;; (:grounded question) (get-grounded-question-html question)
        ;; :else (get-ungrounded-question-html question)
        ))

(defn add-fact
  "Adds a fact to a question."
  [msg]
  (let [id (:id msg)
        question (get-in (deref questions) [:questions id])
        nb-facts (:nb-facts question)]
    (if (and (:max question)
             (> (inc (:nb-facts question)) (:max question)))
      (js/PM.on_error (i18n "pmt_maximum_number_of_facts"))
      ((:fact-adder question)))))

(dispatch/react-to #{:add-fact} (fn [_ msg] (add-fact msg)))

(defn remove-fact
  "Removes the fact of a question."
  [msg]
  (let [id (:id msg)
        fact-nb (:fact-nb msg)
        question (get-in (deref questions) [:questions id])
        nb-facts (:nb-facts question)
        min (or (:min question) 1)]
    (if (or (= nb-facts 1) (<= nb-facts min))
      (js/PM.on_error (i18n "pmt_cannot_remove_fact"))
      ((:fact-remover question) (:event msg) (:fact-nb msg)))))

(dispatch/react-to #{:remove-fact} (fn [_ msg] (remove-fact msg)))

(defn- add-question-html
  [question el e]
  (append el (format "<p><i>%s</i></p>"
                     (or (:hint question) "")))
  (append el e)
  (append el "<br/>"))

(defn add-question
  "Adds one question to the list of questions and stores its fetcher function."
  [question htmlel]
  (let [{:keys [el fact-adder fact-remover fetcher]} (build-question question)]
    (add-question-html question htmlel el)
    (add-facts-number-listener question)
    (swap! questions add-fetcher (:id question) fetcher)
    (swap! questions assoc-in [:questions (:id question) :fact-adder] fact-adder)
    (swap! questions assoc-in [:questions (:id question) :fact-remover] fact-remover)))

(defn add-questions
  "Adds questions to the DOM and stores their fetcher in the questions atom."
  [questions el]
  (doseq [question questions]
    (add-question question el)))

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
    (add-questions questions el)))

(defn display-questions
  "Displays all the questions."
  [_ msg]
  (log "Display question")
  (let [el ($ "#questions")
        questions-to-ids (:questions (deref questions))
        ids (:order (deref questions))]
    (empty el)
    (doseq [ids-for-category ids]
      (display-questions-in-category (map questions-to-ids ids-for-category) el))
    (add-submit-button el)
    (js/PM.scroll_to_bottom)))

(dispatch/react-to #{:questions-added} display-questions)

(defn calc-nb-facts
  "Calculates the number of facts that a question must display."
  [question]
  (or
   ;; this field is not nil when modifying the facts
   (:nb-facts question)
   (if (or (nil? (:min question))
           (zero? (:min question)))
     1
     (:min question))))

(defn- questions-list->map
  "Converts a list of questions to a map indexing them by id."
  [questions-list]
  (reduce (fn [m question]
            (let [nb-facts (calc-nb-facts question)
                  question (assoc question :nb-facts nb-facts)]
              (assoc m (:id question) question)))
          {}
          questions-list))

(defn- questions-ordered
  "Returns an ordered list of questions' ids according to their category."
  [questions-list]
  {:pre [(map? (first questions-list))]}
  (map #(map :id %) (vals (group-by :category questions-list))))

(defn- index-questions
  "Adds the list of new questions to the questions map."
  [questions latest-questions-list]
  (-> questions
      (update-in [:questions] merge (questions-list->map latest-questions-list))
      (assoc :latest-questions (map :id latest-questions-list))
      (update-in [:order] concat (questions-ordered latest-questions-list))))

(defn fetch-values
  "Fetches the answer of a question and sets their value in the question map."
  [qid]
  (let [values ((get-in (deref questions) [:questions qid :fetch-values]))]
    (swap! questions assoc-in [:questions qid :values] values)
    values))

(defn fetch-latest-questions-answers
  "Fetches the answers of the latest questions.
Returns the answers indexed by question's id."
  []
  (let [latest (:latest-questions (deref questions))
        answers (map fetch-values latest)]
    (map (fn [id ans] {:id id :values ans}) latest answers)))

(declare show-questions-or-ag show-ag)

(defn- send-answers
  "Sends the user's values to the server."
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

(dispatch/react-to #{:on-submit}
                   (fn [msg]
                     ((:submit-listener (deref questions)) msg)))

(defn send-answers-for-modification
  "Sends the answers to the server to modify them"
  []
  (when (.valid ($ "#questionsform"))
    (let [answers (fetch-latest-questions-answers)]
      (log "sending answers for modification...")
      (log (clj->js answers)) 
      (js/PM.busy_cursor_on)
      (let [{:keys [questions deleted]} (deref questions)]
       (js/PM.ajax_post js/IMPACT.simulation_url
                        (clj->js {:modify-facts {:facts (vals
                                                         questions)
                                                 :project
                                                 js/IMPACT.project
                                                 :policy js/IMPACT.current_policy
                                                 :deleted deleted
                                                 :db js/IMPACT.db}})
                        (fn [data]
                          (js/PM.busy_cursor_off)
                          (show-ag (.-db data)))
                        js/IMPACT.user
                        js/IMPACT.password
                        js/PM.on_error)))))

(defn ^:export show-questions
  "Adds the HTML for the questions to the list of questions div."
  [latest-questions-list]
  (let [latest (js->clj latest-questions-list :keywordize-keys true)]
    (swap! questions index-questions latest)
    (dispatch/fire :questions-added {:latest-questions latest})))

(defn show-ag
  "Shows the argument graph. Called once there is no more questions to ask."
  [db]
  (set! js/IMPACT.db db)
  (reset! questions (create-questions-map))
  (js/PM.set_arguments_url db))

(defn show-facts
  "Shows facts for modification."
  [questions-list]
  (reset! questions (assoc (create-questions-map)
                           :submit-listener send-answers-for-modification))
  (show-questions questions-list))

(defn show-questions-or-ag
  "Shows the remaining questions to the user or the argument graph if
all questions have been answered."
  [data]
  (if-let [questions-list (.-questions data)]
    (show-questions questions-list)
    (show-ag (.-db data))))

(defn ^:export init-show-questions
  "Initialize the questions and begins the process of showing them."
  []
  (swap! questions assoc :submit-listener send-answers) 
  (js/PM.ajax_post js/IMPACT.simulation_url
                   (clj->js {:request {:question js/IMPACT.question
                                       :project (.toJSON js/PM.project)}})
                   show-questions-or-ag
                   js/IMPACT.user
                   js/IMPACT.password
                   js/PM.on_error))
