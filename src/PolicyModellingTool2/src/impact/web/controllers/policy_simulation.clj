(ns impact.web.controllers.policy-simulation
  (:use  clojure.pprint
         clojure.data.json
         impact.web.logic.askengine
         impact.web.logic.answers
         impact.web.logic.statement-translation
         impact.web.views.pages
         impact.web.core
         [carneades.engine.statement :only (statement-predicate variable? statement-atom)]))

(defmulti ajax-handler (fn [json _] (ffirst json)))

(def kburl (if *debug*
             "http://localhost:8080/kb/impact.xml"
             "http://localhost:8080/PolicyModellingTool2/kb/impact.xml"))

(def questionsdataurl (if *debug*
                         "http://localhost:8080/kb/questions.clj"
                         "http://localhost:8080/PolicyModellingTool2/kb/questions.clj") )

;; TODO: get from a kb
(def query '(hatAnspruchOeffentlicheZugaenglichmachung ?Person ?Werk))

(defmethod ajax-handler :request
  [json session]
  (prn "======================================== request handler! ==============================")
  (prn  (:lang (:service-data session)))
  ;; TODO: read that from a KB?
  (let [service-data (:service-data session)
        service-data (ask-engine service-data query kburl)
        session (assoc session :service-data service-data)]
    (if-not (:has-solution service-data)
      {:session session
       :body (json-str {:questions (:last-questions service-data)})}
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
               question (first (filter #(= (:id %) id) questions))
               stmt (strs->stmt (:statement question))
               pred (statement-predicate stmt)
               ans (cond (variable? (second (statement-atom stmt)))
                         (list pred (symbol (:value answer)))

                         (= 2 (count (statement-atom stmt)))
                         (list pred (symbol (:value answer)))
                         
                         (or
                          (variable? (nth (statement-atom stmt) 2))
                          (= 3 (count (statement-atom stmt))))
                         (list pred (second (statement-atom stmt)) (symbol (:value answer)))
                         
                         :else
                         (throw (Exception. (format "invalid question %s" question)))
                         )]
           ans))
       jsonanswers))


(defmethod ajax-handler :answers
  [json session]
  (prn "======================================== answers handler! ==============================")
  (pprint json)
  (prn "that's it")
  (let [service-data (:service-data session)
        questions (:questions (:answers json))
        answers (reconstruct-answers-from-json (-> json :answers :values) questions)
        service-data (add-answers service-data answers)
        service-data (ask-engine service-data query kburl)
        session (assoc session :service-data service-data)]
    (if-not (:has-solution service-data)
      {:session session
       :body (json-str {:questions (:last-questions service-data)})}
      {:session session
       :body (json-str {:solution (:solution service-data)
                        :path (:lkifsol-pathname service-data)})})))

(defmethod ajax-handler :retrieve_question
  [json session]
  (let [service-data (:service-data session)
        data (:retrieve_question json)
        id (dec (:id data))
        stmt (strs->stmt (:statement data))
        [questions _] (get-structured-questions
                       stmt
                       (:lang service-data)
                       id
                       (:questionsdata service-data))]
    (prn "questions =")
    (pprint questions)
    (prn)
    {:body (json-str {:questions questions})}))

(defn process-ajax-request
  [session params]
  (let [json (get params :json)
        json (read-json json)
        res (ajax-handler json session)]
    res))


(defn reset-session
  []
  {:session {}})

(defn init-page
  []
  (prn "init of session")
  {:headers {"Content-Type" "text/html;charset=UTF-8"}
   :session {:service-data
             {:answers {}
              :state nil
              :to-engine (promise)
              :from-engine (promise)
              :last-id 0
              :goal query
              :lang "en"
              :questionsdata (load-questions questionsdataurl)
              }}
   :body (index-page)})
