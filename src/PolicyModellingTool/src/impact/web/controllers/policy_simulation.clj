(ns impact.web.controllers.policy-simulation
  (:use  clojure.pprint
         clojure.data.json
         impact.web.logic.askengine
         impact.web.logic.answers
         impact.web.logic.statement-translation
         impact.web.views.pages
         impact.web.core
         (carneades.engine policy scheme)
         [carneades.engine.statement :only (neg literal-predicate variable? literal-atom)]))

(defmulti ajax-handler (fn [json _] (ffirst json)))

(defn strs->stmt
  "Converts a collection of a string representing a statement on the client side
   to a formal statement."
  [coll]
  (map symbol (apply list coll)))

(defmethod ajax-handler :request
  [json session]
  (prn "======================================== request handler! ==============================")
  (prn  (:lang session))
  (let [session (assoc session :query (get-main-issue (:theory session) (symbol (:request json))))
        session (ask-engine session)]
    (if-not (:has-solution session)
      {:session session
       :body (json-str {:questions (:last-questions session)})}
      (throw (Exception. "NYI")))))

(defn reconstruct-answers-from-json
  [jsonanswers questions]
  (map (fn [answer]
         ;; TODO: fix JavaScript to have id as integer and not as string!
         (let [id (Integer/parseInt (:id answer))
               question (questions id)
               arity (:arity question)
               stmt (strs->stmt (:statement question))
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
                         )]
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
    {:body (json-str {:questions questions})}))

(defn new-session
  []
  {:answers {}
   :user-questions {}
   :lang "en"
   :last-id 0
   :substitutions {}
   :query nil
   :theory (load-theory default-policies-file
                        (symbol default-policies-namespace)
                        (symbol default-policies-name))
   
   :engine-runs false})

(defmethod ajax-handler :reset
  [json session]
  (prn "[policy-simulation] resetting session")
  {:session (new-session)})

(defn process-ajax-request
  [session body params]
  (let [json (read-json (slurp body))
        res (ajax-handler json session)]
    res))


(defn init-page
  []
  (prn "init of session")
  {:headers {"Content-Type" "text/html;charset=UTF-8"}
   :session (new-session)
   :body (index-page)})

(defn dump-config
  []
  (config-page))
