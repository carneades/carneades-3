(ns impact.web.controllers.policy-simulation
  (:use  clojure.pprint
         clojure.data.json
         impact.web.logic.askengine
         impact.web.logic.answers
         impact.web.logic.statement-translation
         impact.web.views.pages
         impact.web.core
         [carneades.engine.statement :only (neg literal-predicate variable? literal-atom)]))

(defmulti ajax-handler (fn [json _] (ffirst json)))

(def theory-url (if *debug*
                   "http://localhost:8080/policymodellingtool/kb/copyright_policies2.clj"
                   "http://localhost:8080/policymodellingtool/kb/copyright_policies2.clj"))

;; TODO: get from a kb


(defmethod ajax-handler :request
  [json session]
  (prn "======================================== request handler! ==============================")
  (prn  (:lang session))
  (let [session (ask-engine session)]
    (if-not (:has-solution session)
      {:session session
       :body (json-str {:questions (:last-questions session)})}
      (throw (Exception. "NYI")))))


(defn strs->stmt
  "Converts a collection of a string representing a statement on the client side
   to a formal statement."
  [coll]
  (map symbol (apply list coll)))

(defn reconstruct-answers-from-json
  [jsonanswers questions]
  (map (fn [answer]
         ;; TODO: fix JavaScript to have id as integer and not as string!
         (let [id (Integer/parseInt (:id answer))
               question (questions id)
               arity (:arity question)
               stmt (strs->stmt (:statement question))
               _ (prn "stmt =" stmt)
               pred (literal-predicate stmt)
               ;; TODO: makes that generic for all arities
               ans (cond (zero? arity)
                         (if (= (:value answer) "yes") stmt (neg stmt))

                         (= 1 arity)
                         (list pred (symbol (:value answer)))

                         (= 2 arity)
                         (list pred (second (literal-atom stmt)) (symbol (:value answer)))
                         
                         (= 3 arity)
                         (list pred
                               (second (literal-atom stmt))
                               (nth (literal-atom stmt) 2)
                               (symbol (:value answer)))
                         
                         :else
                         (throw (Exception. (format "Invalid arity for question: %s" question)))
                         )
               _ (prn "ans = " ans)]
           ans))
       jsonanswers))


(defmethod ajax-handler :answers
  [json session]
  (prn "======================================== answers handler! ==============================")
  (pprint json)
  (let [answers (reconstruct-answers-from-json (-> json :answers :values)
                                               (:user-questions session))
        session (add-answers session answers)
        session (ask-engine session)]
    (if-not (:has-solution session)
      {:session session
       :body (json-str {:questions (:last-questions session)})}
      {:session session
       :body (json-str {:solution (:solution session)
                        :db (:db session)})})))

(defmethod ajax-handler :retrieve_question
  [json session]
  (let [data (:retrieve_question json)
        id (dec (:id data))
        stmt (strs->stmt (:statement data))
        [questions _] (get-structured-questions
                       stmt
                       (:lang session)
                       id
                       (:questionsdata session))]
    (prn "questions =")
    (pprint questions)
    (prn)
    {:body (json-str {:questions questions})}))

(defn process-ajax-request
  [session body params]
  (let [json (read-json (slurp body))
        res (ajax-handler json session)]
    res))


(defn reset-session
  []
  {:session {}})

(defn load-theory
  "Dynamically loads a theory"
  [url]
  (load-string (slurp url))
  (deref (ns-resolve 'carneades.examples.copyright-policies 'copyright-policies)))

(defn init-page
  []
  (prn "init of session")
  {:headers {"Content-Type" "text/html;charset=UTF-8"}
   :session {:answers {}
             :user-questions {}
             :lang "en"
             :last-id 0
             :substitutions {}
             :query '(may-publish ?Person ?Werk)  ; TODO: get it from the theory!
             :theory (load-theory theory-url)
             :engine-runs false}
   :body (index-page)})
