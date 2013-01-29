;;; Copyright (c) 2011 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.web.service
  (:use clojure.pprint
        compojure.core
         (carneades.engine uuid policy unify statement argument scheme dublin-core utils
                           argument-evaluation aspic)
         (carneades.web pack outline)
         carneades.database.db
         carneades.database.admin
         carneades.database.export
         carneades.database.import
         carneades.xml.caf.export
         carneades.web.walton-schemes
         ring.util.codec
         [carneades.engine.utils :only [sha256]]
         [carneades.database.evaluation :only [evaluate-graph]]
         [ring.middleware.format-response :only [wrap-restful-response]]
         [ring.middleware.cookies :only [wrap-cookies]])
  (:require [clojure.data.json :as json]
            [compojure.route :as route]
            [compojure.handler :as handler]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [carneades.maps.lacij :as lacij]
            [carneades.web.vote :as vote]))

;; To Do: 
;; - way to bootstrap the debate database
;; - search operations, including full text search
;; - CAF import
;; - OPML export
;; - validate input?

(defn get-username-and-password
  [request]
  (let [authorization (second (str/split (get-in request [:headers "authorization"]) #" +"))
        authdata (String. (base64-decode authorization))]
    (str/split authdata #":")))

(def ^{:dynamic true} *debatedb-name* "debates")

(defroutes carneades-web-service-routes
      
  ;; Debates
           
  (GET "/debate" [] 
       (let [db2 (make-database-connection *debatedb-name* "guest" "")]
         (with-db db2 {:body (list-debates)})))
      
  (GET "/debate/:id" [id]
       (let [db2 (make-database-connection *debatedb-name* "guest" "")]
         (with-db db2 {:body (read-debate id)})))
      
  (POST "/debate" request
        (let [m (json/read-json (slurp (:body request)))
              [username password] (get-username-and-password request)
              db (make-database-connection *debatedb-name* username password)]
          (with-db db {:body (create-debate m)})))
      
  (PUT "/debate/:id" request   
       (let [m (json/read-json (slurp (:body request)))
             [username password] (get-username-and-password request)
             db (make-database-connection *debatedb-name* username password)
             id (:id (:params request))]
         (with-db db {:body (update-debate id m)})))

  (GET "/debate-poll/:debateid" [debateid]
       (with-db (make-database-connection *debatedb-name* "guest" "")
         {:body (list-polls debateid)}))
  
  (GET "/debate-poll/:debateid/:id" request
       (let [id (get-in request [:params :id])
             dbconn (make-database-connection *debatedb-name* "guest" "")]
         (with-db dbconn
           {:body (read-poll id)})))

  (POST "/debate-poll/:debateid" request
        (let [m (json/read-json (slurp (:body request)))
              cookies (:cookies request)
              cookieid (get-in cookies ["ring-session" :value])
              policies (map str (vote/find-policies-matching-vote m))
              m (dissoc m :id :policykey :qid :issueid)
              ;; the userid of the poll is a sha256 hash of the cookie id
              ;; thus we are sure the user can vote only once for the session.
              ;; The id is hashed to prevent other users of guessing the cookie
              ;; number by calling the GET debate-poll API.
              id (sha256 cookieid)
              m (assoc m :userid id)
              debateid (get-in request [:params :debateid])
              [username password] (get-username-and-password request)
              dbconn (make-database-connection *debatedb-name* username password)]
          (with-db dbconn
            (let [id (create-poll debateid m policies)]
             {:body {:id id}}))))
  
  (PUT "/debate-poll/:debateid" request
       ;; TODO: users can modify the vote of the others!
       {:status 404}
       ;; (let [m (json/read-json (slurp (:body request)))
       ;;       debateid (get-in request [:params :debateid])
       ;;       [username password] (get-username-and-password request)
       ;;       dbconn (make-database-connection *debatedb-name* username password)]
       ;;    (with-db dbconn
       ;;      (when (update-poll (:id m) (dissoc m :id))
       ;;        {:body (read-poll (:id m))})))
       )

  ;; poll results for the PMT (not for the SCT)
  (GET "/poll-results/:debateid/:casedb" [debateid casedb]
       {:body (vote/vote-stats debateid casedb)})
  
  (GET "/aggregated-poll-results/:debateid" [debateid]
       {:body (vote/aggregated-vote-stats debateid)})
      
  ;; To Do: Deleting debates, poll-debate
       
  ;; Metadata        
  (GET "/metadata/:db" [db] 
       (let [db2 (make-database-connection db "guest" "")]
         (with-db db2 {:body (list-metadata)})))
      
  (GET "/metadata/:db/:id" [db id]
       (let [db2 (make-database-connection db "guest" "")]
         (with-db db2 {:body
                       (unzip-metadata
                        (read-metadata id))})))
      
  (POST "/metadata/:db" request
        (let [db (:db (:params request))
              m (json/read-json (slurp (:body request)))
              [username password] (get-username-and-password request)
              dbconn (make-database-connection db username password)]
          (with-db dbconn {:body
                           {:id (create-metadata
                                 (zip-metadata
                                  (map->metadata m)))}})))

  (PUT "/metadata/:db/:id" request   
       (let [m (json/read-json (slurp (:body request)))
             m (zip-metadata m)
             [username password] (get-username-and-password request)
             db (make-database-connection (:db (:params request)) username password)
             id (Integer/parseInt (:id (:params request)))]
         (with-db db {:body (do
                              (update-metadata id m)
                              (let [data (read-metadata id)
                                    x (unzip-metadata data)]
                                (prn "data: " data)
                                (prn "unzipped: " x)
                                x))})))
      
  (DELETE "/metadata/:db/:id" request
          (let [[username password] (get-username-and-password request)
                dbname (:db (:params request))
                dbconn (make-database-connection dbname username password)
                id (:id (:params request))]
            (with-db dbconn {:body (delete-metadata (Integer/parseInt id))})))
      
  ;; Statements
      
  (GET "/statement/:db" [db] 
       (let [db2 (make-database-connection db "guest" "")]
         (with-db db2 {:body (map pack-statement (list-statements))})))  
      
  (GET "/statement/:db/:id" [db id] 
       (let [db2 (make-database-connection db "guest" "")]
         (with-db db2 
           {:body           (pack-statement (read-statement id))})))
            
  (POST "/statement/:db" request  
        (let [m (json/read-json (slurp (:body request)))
              s (unpack-statement m)
              [username password] (get-username-and-password request)
              db (make-database-connection (:db (:params request)) username password)]
          (with-db db {:body
                       {:id (create-statement s)}})))
      
  (PUT "/statement/:db" request  
       (let [m (json/read-json (slurp (:body request)))
             ;; s (unpack-statement m)
             [username password] (get-username-and-password request)
             db (make-database-connection (:db (:params request)) username password)
             id (:id m)
             m (assoc m :header (zip-metadata (:header m)))]
         (with-db db {:body (do
                              (update-statement id (dissoc m :id))
                              (pack-statement (read-statement id)))})))
      
  (DELETE "/statement/:db/:id" request
          (let [[username password] (get-username-and-password request)
                db (:db (:params request))
                id (:id (:params request))
                db2 (make-database-connection db username password)]
            (with-db db2 {:body
                          (let [stmt (read-statement id)]
                            (doseq [id (premises-for-statement id)]
                              (delete-premise id))
                            (doseq [pro (:pro stmt)]
                              (delete-argument pro))
                            (doseq [con (:con stmt)]
                              (delete-argument con))
                            (delete-statement id))})))
            
  (GET "/main-issues/:db" [db]
       (let [db2 (make-database-connection db "guest" "")]
         (with-db db2 {:body (map pack-statement (main-issues))})))               

  (POST "/matching-statements/:db" request
        ;; returns a vector of {:substitutions :statement} records for the statements
        ;; with atoms matching the query in the body of the request
        (let [m (json/read-json (slurp (:body request)))
              db (:db (:params request))
              s1 (unpack-statement m)
              db (make-database-connection (:db (:params request)) "guest" "")]
          (with-db db {:body (mapcat (fn [s2]
                                       ;; (prn "unifying s1 against s2: " s1 " " (:atom  s2))
                                       (let [subs (unify s1 (:atom s2))]
                                         ;; (prn "subs = " subs)
                                         (when subs 
                                           [{:substitutions subs
                                             :statement (pack-statement s2)}])))
                                     (list-statements))})))

  (GET "/premise-of/:db/:id" [db id]
                                        ; returns a vector of arguments in which the statement with the given id
                                        ; is a premise
       (let [db2 (make-database-connection db "guest" "")]  
         (with-db db2 
           {:body           (map (fn [arg-id] (pack-argument (read-argument arg-id)))
                                 (:premise-of (read-statement id)))})))

      
  ;; Arguments  
      
  (GET "/argument/:db" [db]
       (let [db2 (make-database-connection db "guest" "")] 
         (with-db db2 {:body (map pack-argument (list-arguments))})))
      
  (GET "/argument/:db/:id" [db id]
       (let [db2 (make-database-connection db "guest" "")]  
         (with-db db2 {:body (pack-argument (read-argument id))})))
      
  (GET "/pro-arguments/:db/:id" [db id]
       (let [db2 (make-database-connection db "guest" "")]  
         (with-db db2 {:body (get-pro-arguments id)})))
      
  (GET "/con-arguments/:db/:id" [db id]
       (let [db2 (make-database-connection db "guest" "")]  
         (with-db db2 {:body (get-con-arguments id)})))
      
  (GET "/rebuttals/:db/:id" [db id]
       (let [db2 (make-database-connection db "guest" "")]  
         (with-db db2 {:body (get-rebuttals id)})))

  (GET "/undercutters/:db/:id" [db id]
       (let [db2 (make-database-connection db "guest" "")]  
         (with-db db2 {:body (get-undercutters id)})))
      
  (GET "/dependents/:db/:id" [db id]
                                        ; returns a vector of arguments in which the conclusion of the argument
                                        ; with the given id is a premise
       (let [db2 (make-database-connection db "guest" "")]  
         (with-db db2 
           {:body           (map (fn [arg-id] (pack-argument (read-argument arg-id))) 
                                 (get-dependents id))})))
      
  (POST "/argument/:db" request  
        (let [m (json/read-json (slurp (:body request)))
              arg (unpack-argument m)
              [username password] (get-username-and-password request)
              db (make-database-connection (:db (:params request)) username password)
              argument (map->argument arg)
              undercutters (make-undercutters argument)]
          ;; TODO: assumptions?
          (with-db db
            (let [id (create-argument argument)]
              {:body
               {:id id
                :arguments (cons id
                                 (map (fn [undercutter]
                                        (create-argument undercutter))
                                      undercutters))}}))))
      
  (PUT "/argument/:db" request
       (let [m (json/read-json (slurp (:body request)))
             [username password] (get-username-and-password request)
             db (make-database-connection (:db (:params request)) username password)
             id (:id m)
             arg (unpack-argument m)
             arg2 (dissoc arg :id :undercutters :dependents
                         :exceptions :rebuttals)]
         (with-db db {:body
                      (let [responses (generate-exceptions arg)
                            exceptions-ids (reduce (fn [ids response]
                                                     (conj ids (create-argument (:argument response))))
                                                   []
                                                   responses)]
                        ;; here we have the exceptions' ids but we can not pass them back
                        ;; since backbone.js expects the argument record to be returned...
                        (update-argument id arg2)
                        (argument-data id))})))
      
  (DELETE "/argument/:db/:id" request
          (let [[username password] (get-username-and-password request)
                id (:id (:params request))
                db (:db (:params request))
                db2 (make-database-connection db username password)]
            (with-db db2
              {:body              (delete-argument id)})))
      
  ;; Namespaces
      
  (GET "/namespace/:db" [db]
       (let [db2 (make-database-connection db "guest" "")] 
         (with-db db2 {:body (list-namespaces)})))
      
  (GET "/namespace/:db/:prefix" [db prefix]
       (let [db2 (make-database-connection db "guest" "")]  
         (with-db db2 {:body (read-namespace prefix)})))

  (POST "/namespace/" request  
        (let [prefix (:prefix (:params request)),
              uri (json/read-json (slurp (:body request))),
              [username password] (get-username-and-password request)
              db (make-database-connection (:db (:params request)) username password)]
          (with-db db {:body (create-namespace db prefix uri)})))
      
  (PUT "/namespace/:db" request  
       (let [prefix (:prefix (:params request)),
             uri (json/read-json (slurp (:body request))),
             [username password] (get-username-and-password request)
             db (make-database-connection (:db (:params request)) username password)]
         (with-db db {:body (update-namespace prefix uri)})))
      
  (DELETE "/namespace/:db/:prefix" request
          (let [[username password] (get-username-and-password request)
                db (:db (:params request))
                prefix (:prefix (:params request))
                db2 (make-database-connection db username password)]
            (with-db db2
              {:body              (delete-namespace prefix)})))
      
  ;; Statement Polls
      
  (GET "/statement-poll/:db" request
       (let [[username password] (get-username-and-password request)
             db (:db (:params request))
             db2 (make-database-connection db username password)]
         (with-db db2
           (let [ids (set (map :userid (list-statement-poll)))
                 polls (doall (map read-statement-poll ids))
                 polls (if (nil? polls)
                         ()
                         polls)]
             {:body             polls}))))
      
  (GET "/statement-poll/:db/:id" request
       (let [[username password] (get-username-and-password request)
             db (:db (:params request))
             dbconn (make-database-connection db username password)]  
         (with-db dbconn {:body (read-statement-poll (:id (:params request)))})))

  (POST "/statement-poll/:db" request  
        (let [poll (json/read-json (slurp (:body request))),
              [username password] (get-username-and-password request)
              db (make-database-connection (:db (:params request)) username password)]
          (with-db db
            (do
              (create-statement-poll poll)
              {:body              (read-statement-poll (:id poll))}))))
      
  (PUT "/statement-poll/:db" request  
       (let [poll (json/read-json (slurp (:body request)))
             [username password] (get-username-and-password request)
             db (make-database-connection (:db (:params request)) username password)]
         (with-db db
           (do
             (update-statement-poll poll)
             {:body             (read-statement-poll (:id poll))}))))
      
  (DELETE "/statement-poll/:db/:id" request
          (let [[username password] (get-username-and-password request)
                db (:db (:params request))
                id (:id (:params request))
                db2 (make-database-connection db username password)]
            (with-db db2
              {:body              (delete-statement-poll (Integer/parseInt id))}))) 
      
  ;; Argument Polls
      
  (GET "/argument-poll/:db" request
       (let [[username password] (get-username-and-password request)
             db (:db (:params request))
             db2 (make-database-connection db username password)] 
         (with-db db2 (let [ids (set (map :userid (list-argument-poll)))
                            polls (map read-argument-poll ids)
                            polls (if (nil? polls)
                                    ()
                                    polls)]
                        {:body                        polls}))))
      
  (GET "/argument-poll/:db/:id" request
       (let [[username password] (get-username-and-password request)
             db (:db (:params request))
             id (:db (:params request))
             dbconn (make-database-connection (:db (:params request)) username password)]  
         (with-db dbconn
           {:body           (read-argument-poll (Integer/parseInt id))})))

  (POST "/argument-poll/:db" request  
        (let [poll (json/read-json (slurp (:body request))),
              [username password] (get-username-and-password request)
              db (make-database-connection (:db (:params request)) username password)]
          (with-db db
            (do (create-argument-poll poll)
                {:body                (read-argument-poll (:id poll))}))))
      
  (PUT "/argument-poll/:db" request  
       (let [poll (json/read-json (slurp (:body request)))
             [username password] (get-username-and-password request)
             db (make-database-connection (:db (:params request)) username password)]
         (with-db db
           (do (update-argument-poll poll)
               {:body               (read-argument-poll (:id poll))}))))
      
  (DELETE "/argument-poll/:db/:id" request
          (let [[username password] (get-username-and-password request)
                db (:db (:params request))
                id (:id (:params request))
                db2 (make-database-connection db username password)]
            (with-db db2
              {:body              (delete-argument-poll (Integer/parseInt id))})))

  ;; Aggregated information
      
  (GET "/argumentgraph-info/:db" [db]
       (let [dbconn (make-database-connection db "guest" "")]
         (with-db dbconn
           (let [metadata (list-metadata)
                 main-issues (map pack-statement (main-issues))
                 outline (create-outline 5)]
             {:body             {:metadata (map unzip-metadata metadata)
                                 :main-issues main-issues
                                 :outline outline}}))))

  (GET "/statement-info/:db/:id" [db id]
       (let [dbconn (make-database-connection db "guest" "")]
         (with-db dbconn
           (let [stmt (pack-statement (read-statement id))
                 pro-data (doall (map argument-data (:pro stmt)))
                 con-data (doall (map argument-data (:con stmt)))
                 premise-of-data (doall (map argument-data (:premise-of stmt)))]
             {:body             (assoc stmt 
                                  :pro-data pro-data 
                                  :con-data con-data
                                  :premise-of-data premise-of-data)}))))

  (GET "/argument-info/:db/:id" [db id]
       (prn "argument-info")
       (let [dbconn (make-database-connection db "guest" "")]
         (with-db dbconn
           (let [arg (read-argument id)
                 arg (pack-argument arg)
                 undercutters-data (doall (map argument-data (:undercutters arg)))
                 rebuttals-data (doall (map argument-data (:rebuttals arg)))
                 dependents-data (doall (map argument-data (:dependents arg)))]
             {:body             (assoc arg
                                  :undercutters-data undercutters-data
                                  :rebuttals-data rebuttals-data
                                  :dependents-data dependents-data)}))))
      
      
  ;; XML
      
  (GET "/export/:db" [db]
       (let [dbconn (make-database-connection db "guest" "")]
         (with-db dbconn
           (let [arg-graph (export-to-argument-graph dbconn)
                 xml (with-out-str (argument-graph->xml arg-graph))]
             (if (nil? xml)
               {:status 404,
                :body "Not found."}
               {:status 200             ; 200 is OK
                :headers {"Content-Type" "application/xml"}
                :body xml})))))

  ;; SVG Maps

  (ANY "/map/:db" {params :params}
       (let [db (:db params)
             _ (prn "params =" params)
             lang (keyword (:lang params))
             options (dissoc params :db :lang)
             dbconn (make-database-connection db "guest" "")]
         (with-db dbconn
           (let [convert-option (fn [val]
                                  (try
                                    (Integer/parseInt val)
                                    (catch Exception _
                                      (keyword val))))
                 ag (export-to-argument-graph dbconn)
                 optionsseq (mapcat (fn [[k v]] [k (convert-option v)]) options)
                 svg (apply lacij/export-str ag lang optionsseq)]
             {:status 200
              :headers {"Content-Type" "image/svg+xml;charset=UTF-8"}
              :body svg}))))
      
  ;; Schemes
  (GET "/theory" []
       ;; TODO
       {:body       [walton-schemes]})

  (GET "/theory/:id" []
       ;; TODO
       {:body       walton-schemes})

  (GET "/scheme" []                     ; return all schemes
       {:body       (vals schemes-by-id)})
      
  (GET "/scheme/:id" [id]  ;; return the scheme with the given id
       {:body       (get schemes-by-id (symbol id))})
      
  (POST "/matching-schemes" request ; return all schemes with conclusions matching a goal
        (let [goal (unpack-statement (json/read-json (slurp (:body request))))]
          {:body          (get-schemes schemes-by-predicate goal {} true)}))
      
  (POST "/apply-scheme/:db/:id" request 
	;; apply the scheme with the given id to the substitutions in the body
	;; and add the resulting arguments, if they are ground, to the 
	;; database. Returns a list of the ids of the new arguments.
        (let [data (json/read-json (slurp (:body request)))
              subs (unpack-subs (:subs data))
              attributes (unpack-arg-attrs (:attributes data))
              scheme (get schemes-by-id (symbol (:id (:params request))))]
          (let [responses (instantiate-scheme scheme subs)
                [username password] (get-username-and-password request)
                dbconn (make-database-connection (:db (:params request)) username password)]
            (prn "attributes: " attributes)
            (with-db dbconn
              {:body              
               (reduce (fn [ids response]
                         (assume (:assumptions response))
                         (if (seq ids)
                           (conj ids (create-argument (:argument response)))
                           ;; the first argument is the main one. It is
                           ;; created with the attributes sent to the service
                           (conj ids (create-argument (merge
                                                       (:argument response)
                                                       attributes)))))
                       []
                       responses)}))))

  (POST "/apply-substitutions" request
	;; apply the given substitutions to the given statement
        ;; and returns the result
        (let [content (json/read-json (slurp (:body request)))
              subs (unpack-subs (:substitutions content))
              statement (unpack-statement (:statement content))]
          (prn subs)
          (prn statement)
          {:body          (apply-substitutions subs statement)}))

  ;; Policies
      
  (GET "/policies" []
       {:body       policies})

  (GET "/evaluate-policy/:db/:policykey/:qid/:policyid" [db policykey qid policyid]
       (let [dbconn (make-database-connection db "guest" "")]
         (with-db dbconn
           (let [ag (export-to-argument-graph dbconn)
                 ag (evaluate-policy (symbol qid) (symbol policyid)
                                     (policies (symbol policykey)) ag)
                 root "root"
                 passwd "pw1"
                 dbname (str "db-" (make-uuid))
                 dbconn2 (make-database-connection dbname root passwd)
                 metadata (map map->metadata (list-metadata))]
             (create-argument-database dbname root passwd (first metadata))
             (import-from-argument-graph dbconn2 ag false)
             (with-db dbconn2
               (doseq [m (rest metadata)]
                 (create-metadata m)))
             {:body             {:db dbname}}))))

  (GET "/find-policies/:db/:policykey/:qid/:issueid/:acceptability"
       [db policykey qid issueid acceptability]
       (let [dbconn (make-database-connection db "guest" "")]
         (with-db dbconn
           (let [ag (export-to-argument-graph dbconn)
                 theory (policies (symbol policykey))
                 policies (find-policies ag theory (symbol qid) (symbol issueid)
                                         (condp = acceptability
                                           "in" :in
                                           "out" :out
                                           "undecided" :undecided))]
             {:body             {:policies policies}}))))

  ;; Argument Evaluation
    
  (POST "/evaluate-argument-graph/:db" request
       (let [[username password] (get-username-and-password request)
             db (:db (:params request))]
         (evaluate-graph db username password)
         {:body true}))

  (POST "/copy-case/:db" request
        (let [[username password] (get-username-and-password request)
              dbname (-> request :params :db)]
          {:body {:db (make-copy dbname username password)}}))

  ;; Other 
      
  (GET "/" [] 
       "<h1>Carneades Web Service</h1>
            <p>This web service is part of the <a href=\"http://carneades.github.com\">
            Carneades Argumentation System</a></p>")                                                                
  (route/resources "/")
  (route/not-found "Page not found."))

(def carneades-web-service
  (-> (handler/site carneades-web-service-routes)
      (wrap-restful-response)
      (wrap-cookies)))

;;; utilities functions to test the service 
(defn get-request [resource web-app & params]
  (web-app {:request-method :get :uri resource :params (first params)}))

(defn post-request [resource web-app headers body & params]
  (web-app {:request-method :post :uri resource :headers headers :body (char-array body) :params (first params)}))

(defn put-request [resource web-app headers body & params]
  (web-app {:request-method :put :uri resource :headers headers :body (char-array body) :params (first params)}))

(defn delete-request [resource web-app headers body & params]
  (web-app {:request-method :delete :uri resource :headers headers :body (char-array body) :params (first params)}))

