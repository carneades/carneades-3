(ns impact.web.controllers
  (:use  clojure.pprint
         clojure.data.json
         impact.web.askengine
         impact.web.answers
         impact.web.views
         [carneades.engine.statement :only (statement-predicate variable? statement-atom)]))

(defmulti ajax-handler (fn [json _] (ffirst json)))

(def kburl "http://localhost:8080/kb/impact.xml")
(def query '(hatAnspruchOeffentlicheZugaenglichmachung ?Person ?Werk))

(defmethod ajax-handler :request
  [json session]
  (prn "======================================== request handler! ==============================")
  ;; TODO: read that from a KB?
  (let [service-data (:service-data session)
        service-data (ask-engine service-data query kburl)]
    {:session (assoc session :service-data service-data)
     :body (json-str {:questions (:last-questions service-data)})}))


(defmethod ajax-handler :answers
  [json session]
  (prn "======================================== answers handler! ==============================")
  ;; (prn "json answers =")
  ;; (prn (:answers json))
  ;; (prn "last questions =")
  ;; (pprint (:last-questions (:service-data session)))
  (let [service-data (:service-data session)
        last-questions (:last-questions service-data)
        answers (doall
                 (map (fn [answer]
                        ;; TODO: fix JavaScript to have id as integer and not as string!
                        (let [id (Integer/parseInt (:id answer))
                              question (first (filter #(= (:id %) id) last-questions))
                              ;; _ (prn "======================================== QUESTION")
                              ;; _ (prn question)
                              stmt (:statement question)
                              ;; this is the statement extracted from the XML
                              ;; stmt (if (seq? stmt) stmt (list stmt))
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
                          ;; (printf "%s ---------> %s\n" stmt ans)
                          ans
                         
                          ))
                      (:answers json)))
        service-data (add-answers service-data answers)
        service-data (ask-engine service-data query kburl)]
    ;; (prn "======================> last questions =")
    ;; (prn (:last-questions service-data))
    {:session (assoc session :service-data service-data)
     :body (json-str {:questions (:last-questions service-data)})}))

(defn process-ajax-request
  [session params]
  (let [json (get params "json")
        json (read-json json)
        res (ajax-handler json session)]
    res))


(defn reset-session
  []
  {:session {}})

(defn init-page
  []
  {:session {:service-data {:answers {} :state nil :to-engine (promise) :from-engine (promise) :last-id 0}}
   :body (index-page)})
