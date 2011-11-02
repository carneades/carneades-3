(ns impact.web.policy-simulation
  (:use  clojure.pprint
         clojure.data.json
         impact.web.askengine
         impact.web.answers
         impact.web.views
         impact.web.core
         [carneades.engine.statement :only (statement-predicate variable? statement-atom)]))

(defmulti ajax-handler (fn [json _] (ffirst json)))

(def kburl (if *debug*
             "http://localhost:8080/kb/impact.xml"
             "http://localhost:8080/PolicyModellingTool2/kb/impact.xml"))

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
        service-data (ask-engine service-data query kburl)
        session (assoc session :service-data service-data)]
    ;; (prn "======================> last questions =")
    ;; (prn (:last-questions service-data))
    (if-not (:has-solution service-data)
      {:session session
       :body (json-str {:questions (:last-questions service-data)})}
      {:session session
       :body (json-str {:solution (:solution service-data)
                        :path (:lkifsol-pathname service-data)})})))

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
              :lang "en"}}
   :body (index-page)})
