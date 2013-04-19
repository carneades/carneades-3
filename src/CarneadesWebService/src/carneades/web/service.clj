;;; Copyright (c) 2011 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.web.service
  (:use clojure.pprint
        compojure.core
        (carneades.engine uuid policy unify statement argument scheme dublin-core utils
                          argument-evaluation aspic)
        (carneades.web pack outline)
        carneades.database.export
        carneades.database.import
        carneades.xml.caf.export
        ring.util.codec
        [carneades.engine.utils :only [sha256]]
        [carneades.database.evaluation :only [evaluate-graph]]
        [carneades.web.project :only [init-projects-data!]]
        [ring.middleware.format-response :only [wrap-restful-response]]
        [ring.middleware.cookies :only [wrap-cookies]])
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [carneades.database.argument-graph :as ag-db]
            [carneades.database.case :as case]
            [carneades.database.db :as db]
            [carneades.project.admin :as project]
            [compojure.route :as route]
            [compojure.handler :as handler]
            [clojure.string :as str]
            [carneades.maps.lacij :as lacij]
            [carneades.web.vote :as vote]
            ))

;; To Do: 
;; - search operations, including full text search
;; - CAF import
;; - OPML export
;; - validate input?

(defn get-username-and-password
  [request]
  (let [authorization (second (str/split (get-in request [:headers "authorization"]) #" +"))
        authdata (String. (base64-decode authorization))]
    (str/split authdata #":")))

(def state (atom {:projects (project/list-projects)
                  :projects-data (init-projects-data!)}))

(def ^{:dynamic true} *debatedb-name* "debates")

(defroutes carneades-web-service-routes
  
  ;; Projects
  
  (GET "/project" [] 
       {:body
        (:projects (deref state))})
  
  (GET "/project/:id" [id]
       {:body
        (merge (get-in (deref state) [:projects-data id :properties])
               {:id id})})
  
  (POST "/project" request
        (let [m (json/read-json (slurp (:body request)))
              [username password] (get-username-and-password request)
              ;; TODO enforces Admin role here
              ]
          (throw (ex-info "NYI" {}))))

  ;; documents for projects
  (GET "/documents/:project/:doc" [project doc]
       (let [path (str project/projects-directory file-separator project file-separator
                       "documents" file-separator doc)]
         (if (not (exists? path))
           {:status 404
            :body "File not found"}
           {:body
            (io/input-stream path)})))
  
  (PUT "/debate/:id" request   
       (let [m (json/read-json (slurp (:body request)))
             [username password] (get-username-and-password request)
             db (db/make-connection *debatedb-name* username password)
             id (:id (:params request))]
         (db/with-db db {:body (case/update-debate id m)})))

  (GET "/debate-poll/:project/:debateid" [project debateid]
       (db/with-db (db/make-connection project *debatedb-name* "guest" "")
         {:body (case/list-polls debateid)}))
  
  (GET "/debate-poll/:project/:debateid/:id" request
       (let [id (get-in request [:params :id])
             project (get-in request [:params :project])
             dbconn (db/make-connection project *debatedb-name* "guest" "")]
         (db/with-db dbconn
           {:body (case/read-poll id)})))

  (POST "/debate-poll/:project/:debateid" request
        (let [m (json/read-json (slurp (:body request)))
              project (get-in request [:params :project])
              cookies (:cookies request)
              cookieid (get-in cookies ["ring-session" :value])
              policies (map str (vote/find-policies-matching-vote
                                 project
                                 (get-in (deref state)
                                         [:projects-data
                                          project :properties])
                                 m))
              m (dissoc m :id :policykey :qid :issueid :project)
              ;; the userid of the poll is a sha256 hash of the cookie id
              ;; thus we are sure the user can vote only once for the session.
              ;; The id is hashed to prevent other users of guessing the cookie
              ;; number by calling the GET debate-poll API.
              id (sha256 cookieid)
              m (assoc m :userid id)
              _ (pprint m)
              debateid (get-in request [:params :debateid])
              [username password] (get-username-and-password request)
              dbconn (db/make-connection project *debatedb-name* username password)]
          (db/with-db dbconn
            (let [id (case/create-poll debateid m policies)]
              {:body {:id id}}))))
  
  (PUT "/debate-poll/:project/:debateid" request
       ;; TODO: users can modify the vote of the others!
       {:status 404}
       ;; (let [m (json/read-json (slurp (:body request)))
       ;;       debateid (get-in request [:params :debateid])
       ;;       [username password] (get-username-and-password request)
       ;;       dbconn (db/make-connection *debatedb-name* username password)]
       ;;    (db/with-db dbconn
       ;;      (when (update-poll (:id m) (dissoc m :id))
       ;;        {:body (read-poll (:id m))})))
       )

  ;; poll results for the PMT (not for the SCT)
  (GET "/poll-results/:project/:debateid/:casedb" [project debateid casedb]
       {:body (vote/vote-stats project debateid casedb)})
  
  (GET "/aggregated-poll-results/:project/:debateid" [project debateid]
       {:body (vote/aggregated-vote-stats project debateid)})
  
  ;; To Do: Deleting debates, poll-debate
  
  ;; Metadata        
  (GET "/metadata/:project/:db" [project db] 
       (let [db2 (db/make-connection project db "guest" "")]
         (db/with-db db2 {:body (ag-db/list-metadata)})))
  
  (GET "/metadata/:project/:db/:id" [project db id]
       (let [db2 (db/make-connection db "guest" "")]
         (db/with-db db2 {:body
                          (unzip-metadata
                           (ag-db/read-metadata id))})))
  
  (POST "/metadata/:project/:db" request
        (let [db (:db (:params request))
              m (json/read-json (slurp (:body request)))
              [username password] (get-username-and-password request)
              dbconn (db/make-connection db username password)]
          (db/with-db dbconn {:body
                              {:id (ag-db/create-metadata
                                    (zip-metadata
                                     (map->metadata m)))}})))

  (PUT "/metadata/:project/:db/:id" request   
       (let [m (json/read-json (slurp (:body request)))
             m (zip-metadata m)
             [username password] (get-username-and-password request)
             db (db/make-connection (:db (:params request)) username password)
             id (Integer/parseInt (:id (:params request)))]
         (db/with-db db {:body (do
                                 (ag-db/update-metadata id m)
                                 (let [data (ag-db/read-metadata id)
                                       x (unzip-metadata data)]
                                   x))})))
  
  (DELETE "/metadata/:project/:db/:id" request
          (let [[username password] (get-username-and-password request)
                dbname (:db (:params request))
                dbconn (db/make-connection dbname username password)
                id (:id (:params request))]
            (db/with-db dbconn {:body (ag-db/delete-metadata (Integer/parseInt id))})))
  
  ;; Statements
  
  (GET "/statement/:project/:db" [project db] 
       (let [db2 (db/make-connection project db "guest" "")]
         (db/with-db db2 {:body (map pack-statement (ag-db/list-statements))})))  
  
  (GET "/statement/:project/:db/:id" [project db id] 
       (let [db2 (db/make-connection project db "guest" "")]
         (db/with-db db2 
           {:body (pack-statement (ag-db/read-statement id))})))
  
  (POST "/statement/:project/:db" request  
        (let [m (json/read-json (slurp (:body request)))
              s (unpack-statement m)
              [username password] (get-username-and-password request)
              {:keys [project db]} (:params request)
              db (db/make-connection project db username password)]
          (db/with-db db
            {:body {:id (ag-db/create-statement s)}})))
  
  (PUT "/statement/:project/:db" request  
       (let [m (json/read-json (slurp (:body request)))
             ;; s (unpack-statement m)
             [username password] (get-username-and-password request)
             {:keys [project db]} (:params request)
             dbconn (db/make-connection project db username password)
             id (:id m)
             m (assoc m :header (zip-metadata (:header m)))]
         (db/with-db dbconn
           {:body (do
                    (ag-db/update-statement id (dissoc m :id))
                    (pack-statement (ag-db/read-statement id)))})))
  
  (DELETE "/statement/:db/:id" request
          (let [[username password] (get-username-and-password request)
                db (:db (:params request))
                id (:id (:params request))
                db2 (db/make-connection db username password)]
            (db/with-db db2 {:body
                             (let [stmt (ag-db/read-statement id)]
                               (doseq [id (ag-db/premises-for-statement id)]
                                 (ag-db/delete-premise id))
                               (doseq [pro (:pro stmt)]
                                 (ag-db/delete-argument pro))
                               (doseq [con (:con stmt)]
                                 (ag-db/delete-argument con))
                               (ag-db/delete-statement id))})))
  
  (GET "/main-issues/:db" [db]
       (let [db2 (db/make-connection db "guest" "")]
         (db/with-db db2 {:body (map pack-statement (ag-db/main-issues))})))               

  (POST "/matching-statements/:db" request
        ;; returns a vector of {:substitutions :statement} records for the statements
        ;; with atoms matching the query in the body of the request
        (let [m (json/read-json (slurp (:body request)))
              db (:db (:params request))
              s1 (unpack-statement m)
              db (db/make-connection (:db (:params request)) "guest" "")]
          (db/with-db db {:body (mapcat (fn [s2]
                                          ;; (prn "unifying s1 against s2: " s1 " " (:atom  s2))
                                          (let [subs (unify s1 (:atom s2))]
                                            ;; (prn "subs = " subs)
                                            (when subs 
                                              [{:substitutions subs
                                                :statement (pack-statement s2)}])))
                                        (ag-db/list-statements))})))

  (GET "/premise-of/:db/:id" [db id]
                                        ; returns a vector of arguments in which the statement with the given id
                                        ; is a premise
       (let [db2 (db/make-connection db "guest" "")]  
         (db/with-db db2 
           {:body           (map (fn [arg-id] (pack-argument (ag-db/read-argument arg-id)))
                                 (:premise-of (ag-db/read-statement id)))})))

  
  ;; Arguments  
  
  (GET "/argument/:project/:db" [project db]
       (let [db2 (db/make-connection project db "guest" "")] 
         (db/with-db db2 {:body (map pack-argument (ag-db/list-arguments))})))
  
  (GET "/argument/:project/:db/:id" [project db id]
       (let [db2 (db/make-connection project db "guest" "")]  
         (db/with-db db2 {:body (pack-argument (ag-db/read-argument
                                                id))})))

  (POST "/argument/:project/:db" request  
        (let [m (json/read-json (slurp (:body request)))
              arg (unpack-argument m)
              [username password] (get-username-and-password request)
              db (:db (:params request))
              project (:project (:params request))
              dbconn (db/make-connection project db username password)
              argument (map->argument arg)
              undercutters (make-undercutters argument)]
          ;; TODO: assumptions?
          (db/with-db dbconn
            (let [id (ag-db/create-argument argument)]
              {:body
               {:id id
                :arguments (cons id
                                 (map (fn [undercutter]
                                        (ag-db/create-argument undercutter))
                                      undercutters))}}))))
  
  (PUT "/argument/:project/:db" request
       (let [m (json/read-json (slurp (:body request)))
             [username password] (get-username-and-password request)
             db (:db (:params request))
             project (:project (:params request))
             dbconn (db/make-connection project db username password)
             id (:id m)
             arg (unpack-argument m)
             arg2 (dissoc arg :id :undercutters :dependents
                          :exceptions :rebuttals)]
         (db/with-db dbconn
           {:body
            (let [responses (generate-exceptions arg)
                  exceptions-ids (reduce (fn [ids response]
                                           (conj ids (ag-db/create-argument
                                                      (:argument response))))
                                         []
                                         responses)]
              ;; here we have the exceptions' ids but we can not pass them back
              ;; since backbone.js expects the argument record to be returned...
              (ag-db/update-argument id arg2)
              (argument-data id))})))
  
  (DELETE "/argument/:db/:id" request
          (let [[username password] (get-username-and-password request)
                id (:id (:params request))
                db (:db (:params request))
                db2 (db/make-connection db username password)]
            (db/with-db db2
              {:body              (ag-db/delete-argument id)})))

  
  (GET "/pro-arguments/:db/:id" [db id]
       (let [db2 (db/make-connection db "guest" "")]  
         (db/with-db db2 {:body (ag-db/get-pro-arguments id)})))
  
  (GET "/con-arguments/:db/:id" [db id]
       (let [db2 (db/make-connection db "guest" "")]  
         (db/with-db db2 {:body (ag-db/get-con-arguments id)})))
  
  (GET "/rebuttals/:db/:id" [db id]
       (let [db2 (db/make-connection db "guest" "")]  
         (db/with-db db2 {:body (ag-db/get-rebuttals id)})))

  (GET "/undercutters/:db/:id" [db id]
       (let [db2 (db/make-connection db "guest" "")]  
         (db/with-db db2 {:body (ag-db/get-undercutters id)})))
  
  (GET "/dependents/:db/:id" [db id]
                                        ; returns a vector of arguments in which the conclusion of the argument
                                        ; with the given id is a premise
       (let [db2 (db/make-connection db "guest" "")]  
         (db/with-db db2 
           {:body           (map (fn [arg-id] (pack-argument (ag-db/read-argument arg-id))) 
                                 (ag-db/get-dependents id))})))
  
  ;; Namespaces
  
  (GET "/namespace/:db" [db]
       (let [db2 (db/make-connection db "guest" "")] 
         (db/with-db db2 {:body (ag-db/list-namespaces)})))
  
  (GET "/namespace/:db/:prefix" [db prefix]
       (let [db2 (db/make-connection db "guest" "")]  
         (db/with-db db2 {:body (ag-db/read-namespace prefix)})))

  (POST "/namespace/" request  
        (let [prefix (:prefix (:params request)),
              uri (json/read-json (slurp (:body request))),
              [username password] (get-username-and-password request)
              db (db/make-connection (:db (:params request)) username password)]
          (db/with-db db {:body (ag-db/create-namespace db prefix uri)})))
  
  (PUT "/namespace/:db" request  
       (let [prefix (:prefix (:params request)),
             uri (json/read-json (slurp (:body request))),
             [username password] (get-username-and-password request)
             db (db/make-connection (:db (:params request)) username password)]
         (db/with-db db {:body (ag-db/update-namespace prefix uri)})))
  
  (DELETE "/namespace/:db/:prefix" request
          (let [[username password] (get-username-and-password request)
                db (:db (:params request))
                prefix (:prefix (:params request))
                db2 (db/make-connection db username password)]
            (db/with-db db2
              {:body              (ag-db/delete-namespace prefix)})))
  
  ;; Statement Polls
  
  (GET "/statement-poll/:project/:db" request
       (let [[username password] (get-username-and-password request)
             db (:db (:params request))
             project (:project (:params request))
             db2 (db/make-connection project db username password)]
         (db/with-db db2
           (let [ids (set (map :userid (ag-db/list-statement-poll)))
                 polls (doall (map ag-db/read-statement-poll ids))
                 polls (if (nil? polls)
                         ()
                         polls)]
             {:body             polls}))))
  
  (GET "/statement-poll/:project/:db/:id" request
       (let [[username password] (get-username-and-password request)
             db (:db (:params request))
             project (:project (:params request))
             dbconn (db/make-connection db username password)]  
         (db/with-db dbconn {:body (ag-db/read-statement-poll (:id (:params request)))})))

  (POST "/statement-poll/:project/:db" request  
        (let [poll (json/read-json (slurp (:body request))),
              [username password] (get-username-and-password request)
              db (:db (:params request))
              project (:project (:params request))
              dbconn (db/make-connection project db username password)]
          (db/with-db dbconn
            (do
              (ag-db/create-statement-poll poll)
              {:body              (ag-db/read-statement-poll (:id poll))}))))
  
  (PUT "/statement-poll/:project/:db" request  
       (let [poll (json/read-json (slurp (:body request)))
             [username password] (get-username-and-password request)
             db (:db (:params request))
             project (:project (:params request))
             dbconn (db/make-connection project db username password)]
         (db/with-db dbconn
           (do
             (ag-db/update-statement-poll poll)
             {:body             (ag-db/read-statement-poll (:id poll))}))))
  
  (DELETE "/statement-poll/:db/:id" request
          (let [[username password] (get-username-and-password request)
                db (:db (:params request))
                id (:id (:params request))
                db2 (db/make-connection db username password)]
            (db/with-db db2
              {:body              (ag-db/delete-statement-poll (Integer/parseInt id))}))) 
  
  ;; Argument Polls
  
  (GET "/argument-poll/:project/:db" request
       (let [[username password] (get-username-and-password request)
             db (:db (:params request))
             project (:project (:params request))
             dbconn (db/make-connection project db username password)] 
         (db/with-db dbconn
           (let [ids (set (map :userid (ag-db/list-argument-poll)))
                 polls (map ag-db/read-argument-poll ids)
                 polls (if (nil? polls)
                         ()
                         polls)]
             {:body                        polls}))))
  
  (GET "/argument-poll/:project/:db/:id" request
       (let [[username password] (get-username-and-password request)
             db (:db (:params request))
             id (:id (:params request))
             project (:project (:params request))
             dbconn (db/make-connection project db username password)]  
         (db/with-db dbconn
           {:body (ag-db/read-argument-poll (Integer/parseInt id))})))

  (POST "/argument-poll/:project/:db" request  
        (let [poll (json/read-json (slurp (:body request))),
              [username password] (get-username-and-password request)
              db (:db (:params request))
              project (:project (:params request))
              db (db/make-connection project db username password)]
          (db/with-db db
            (do (ag-db/create-argument-poll poll)
                {:body (ag-db/read-argument-poll (:id poll))}))))
  
  (PUT "/argument-poll/:project/:db" request  
       (let [poll (json/read-json (slurp (:body request)))
             [username password] (get-username-and-password request)
             db (:db (:params request))
             project (:project (:params request))
             dbconn (db/make-connection project db username password)]
         (db/with-db dbconn
           (do (ag-db/update-argument-poll poll)
               {:body (ag-db/read-argument-poll (:id poll))}))))
  
  (DELETE "/argument-poll/:project/:db/:id" request
          (let [[username password] (get-username-and-password request)
                project (:project (:params request))
                db (:db (:params request))
                id (:id (:params request))
                dbconn (db/make-connection project db username password)]
            (db/with-db dbconn
              {:body (ag-db/delete-argument-poll (Integer/parseInt id))})))

  ;; Aggregated information
  
  (GET "/argumentgraph-info/:project/:db" [project db]
       (let [dbconn (db/make-connection project db "guest" "")]
         (db/with-db dbconn
           (let [metadata (ag-db/list-metadata)
                 main-issues (map pack-statement (ag-db/main-issues))
                 outline (create-outline 5)]
             {:body             {:metadata (map unzip-metadata metadata)
                                 :main-issues main-issues
                                 :outline outline}}))))

  (GET "/statement-info/:project/:db/:id" [project db id]
       (let [dbconn (db/make-connection project db "guest" "")]
         (db/with-db dbconn
           (let [stmt (pack-statement (ag-db/read-statement id))
                 pro-data (doall (map argument-data (:pro stmt)))
                 con-data (doall (map argument-data (:con stmt)))
                 premise-of-data (doall (map argument-data (:premise-of stmt)))]
             {:body             (assoc stmt 
                                  :pro-data pro-data 
                                  :con-data con-data
                                  :premise-of-data premise-of-data)}))))

  (GET "/argument-info/:project/:db/:id" [project db id]
       (prn "argument-info")
       (let [dbconn (db/make-connection project db "guest" "")]
         (db/with-db dbconn
           (let [arg (ag-db/read-argument id)
                 arg (pack-argument arg)
                 undercutters-data (doall (map argument-data (:undercutters arg)))
                 rebuttals-data (doall (map argument-data (:rebuttals arg)))
                 dependents-data (doall (map argument-data (:dependents arg)))]
             {:body             (assoc arg
                                  :undercutters-data undercutters-data
                                  :rebuttals-data rebuttals-data
                                  :dependents-data dependents-data)}))))
  
  
  ;; XML
  
  (GET "/export/:project/:db" [project db]
       (let [dbconn (db/make-connection project db "guest" "")]
         (db/with-db dbconn
           (let [arg-graph (export-to-argument-graph dbconn)
                 xml (with-out-str (argument-graph->xml arg-graph))]
             (if (nil? xml)
               {:status 404,
                :body "Not found."}
               {:status 200             ; 200 is OK
                :headers {"Content-Type" "application/xml"}
                :body xml})))))

  ;; SVG Maps

  (ANY "/map/:project/:db" {params :params}
       (let [db (:db params)
             project (:project params)
             _ (prn "params =" params)
             lang (keyword (:lang params))
             options (dissoc params :db :lang)
             dbconn (db/make-connection project db "guest" "")]
         (db/with-db dbconn
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
  
  ;; Theory
  (GET "/theory/:project/:theory" [project theory]
       {:body
        (project/load-theory project theory)})

  ;; ;; Scheme
  (GET "/scheme/:project" [project]
       {:body
        (:schemes (project/load-theory project
                              (get-in (deref state)
                                      [:projects-data
                                       project
                                       :properties
                                       :scheme])))})
  
  ;; (GET "/scheme/:id" [id]  ;; return the scheme with the given id
  ;;      {:body       (get schemes-by-id (symbol id))})
  
  ;; (POST "/matching-schemes" request ; return all schemes with conclusions matching a goal
  ;;       (let [goal (unpack-statement (json/read-json (slurp (:body request))))]
  ;;         {:body          (get-schemes schemes-by-predicate goal {} true)}))
  
  ;; (POST "/apply-scheme/:db/:id" request 
  ;;       ;; apply the scheme with the given id to the substitutions in the body
  ;;       ;; and add the resulting arguments, if they are ground, to the 
  ;;       ;; database. Returns a list of the ids of the new arguments.
  ;;       (let [data (json/read-json (slurp (:body request)))
  ;;             subs (unpack-subs (:subs data))
  ;;             attributes (unpack-arg-attrs (:attributes data))
  ;;             scheme (get schemes-by-id (symbol (:id (:params request))))]
  ;;         (let [responses (instantiate-scheme scheme subs)
  ;;               [username password] (get-username-and-password request)
  ;;               dbconn (db/make-connection (:db (:params request)) username password)]
  ;;           (prn "attributes: " attributes)
  ;;           (db/with-db dbconn
  ;;             {:body              
  ;;              (reduce (fn [ids response]
  ;;                        (ag-db/assume (:assumptions response))
  ;;                        (if (seq ids)
  ;;                          (conj ids (ag-db/create-argument (:argument response)))
  ;;                          ;; the first argument is the main one. It is
  ;;                          ;; created with the attributes sent to the service
  ;;                          (conj ids (ag-db/create-argument (merge
  ;;                                                            (:argument response)
  ;;                                                            attributes)))))
  ;;                      []
  ;;                      responses)}))))

  (POST "/apply-substitutions" request
	;; apply the given substitutions to the given statement
        ;; and returns the result
        (let [content (json/read-json (slurp (:body request)))
              subs (unpack-subs (:substitutions content))
              statement (unpack-statement (:statement content))]
          (prn subs)
          (prn statement)
          {:body          (apply-substitutions subs statement)}))
  
  (GET "/evaluate-policy/:project/:db/:policykey/:qid/:policyid"
       [project db policykey qid policyid]
       (let [dbconn (db/make-connection project db "guest" "")]
         (db/with-db dbconn
           (let [ag (export-to-argument-graph dbconn)
                 policy (project/load-theory
                         project
                         (get-in (deref state) [:projects-data project :properties :policies]))
                 ag (evaluate-policy (symbol qid)
                                     (symbol policyid)
                                     policy
                                     ag)
                 root "root"
                 passwd "pw1"
                 dbname (str "db-" (make-uuid))
                 dbconn2 (db/make-connection project dbname root passwd)
                 metadata (map map->metadata (ag-db/list-metadata))]
             (ag-db/create-argument-database project dbname root passwd (first metadata))
             (import-from-argument-graph dbconn2 ag false)
             (db/with-db dbconn2
               (doseq [m (rest metadata)]
                 (ag-db/create-metadata m)))
             {:body             {:db dbname}})))
       )

  (GET "/find-policies/:project/:db/:policykey/:qid/:issueid/:acceptability"
       [project db policykey qid issueid acceptability]
       (let [dbconn (db/make-connection project db "guest" "")]
         (db/with-db dbconn
           (let [ag (export-to-argument-graph dbconn)
                 policy (project/load-theory
                         project
                         (get-in (deref state) [:projects-data project :properties :policies]))
                 policies (find-policies ag policy (symbol qid) (symbol issueid)
                                         (condp = acceptability
                                           "in" :in
                                           "out" :out
                                           "undecided" :undecided))]
             {:body             {:policies policies}}))))

  ;; Argument Evaluation
  
  (POST "/evaluate-argument-graph/:project/:db" request
        (let [[username password] (get-username-and-password request)
              db (:db (:params request))
              project (:project (:params request))]
          (evaluate-graph project db username password)
          {:body true}))

  (POST "/copy-case/:project/:db" request
        (let [[username password] (get-username-and-password request)
              project (-> request :params :project)
              dbname (-> request :params :db)]
          {:body {:db (db/make-copy project dbname username password)}}))

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

