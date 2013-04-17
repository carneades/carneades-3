;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.web.test.service
  (:use clojure.test
        clojure.pprint
        (carneades.engine statement uuid dublin-core argument)
        carneades.web.service
        clojure.data.json)
  (:require [carneades.database.db :as db]
            [carneades.database.argument-graph :as ag-db]
            [carneades.database.case :as case])
  (:import java.io.File))

;; (def tmpdir (System/getProperty "java.io.tmpdir"))

(def dbname (str "testdb-" (make-uuid-str)))

(def debatedb-name (str "testdb-debate-" (make-uuid-str)))

(def auth-header "Basic cm9vdDpwdzE=") ;; encoded root:pw1

(defn create-tmp-db
  []
 (ag-db/create-argument-database 
   dbname 
   "root" 
   "pw1" 
   (make-metadata)))

(defn delete-tmp-db
  []
  (.delete (File. (db/dbfilename dbname))))

(defn create-tmp-debatedb
  []
  (case/create-debate-database debatedb-name "root" "pw1"))

(defn delete-tmp-debatedb
  []
  (.delete (File. (db/dbfilename debatedb-name))))

(defn create-debate
  []
  (db/with-db (db/make-connection debatedb-name "root" "pw1")
    (case/create-debate {:public false :id dbname})))

(defn create-tmp-dbs
  []
  (create-tmp-db)
  (create-tmp-debatedb)
  (create-debate))

(defn delete-tmp-dbs
  []
  (delete-tmp-db)
  (delete-tmp-debatedb))

(defn db-fixture [x] (create-tmp-dbs) (x) (delete-tmp-dbs))

(use-fixtures :once db-fixture) 

(defn unrecordify
  "Converts a record to a hash-map"
  [r]
  (into {} r))

(defn make-some-string [prefix]
  (str prefix "-" (gensym)))

(deftest post-debate-poll
  (binding [*debatedb-name* debatedb-name]
    (let [poll {:mainissueatompredicate "may-publish"
                :opinion 0.8}
          cookieid (make-uuid-str)
          res (post-request (str "/debate-poll/" dbname) carneades-web-service
                            {"authorization" auth-header
                             "cookie" (str "ring-session=" cookieid)}
                            (json-str poll))
          id (:id (read-json (slurp (:body res))))
          poll (assoc poll :id id)
          res2 (get-request (str "/debate-poll/" dbname "/" id) carneades-web-service)
          created-poll (read-json (slurp (:body res2)))]
      (is (= (:mainissueatompredicate poll) (:mainissueatompredicate created-poll)))
      (is (= (:opinion poll) (:opinion created-poll))))))

;; (deftest put-debate-poll
;;   (binding [*debatedb-name* debatedb-name]
;;     (let [poll {:mainissueatompredicate "may-publish"
;;                 :opinion 0.8}
;;           cookieid (make-uuid-str)
;;           res (post-request (str "/debate-poll/" dbname) carneades-web-service
;;                             {"authorization" auth-header
;;                              "cookie" (str "ring-session=" cookieid)}
;;                             (json-str poll))
;;           poll (assoc poll :id (:id (read-json (slurp (:body res)))))
;;           new-opinion 0.777
;;           new-poll (assoc poll :opinion new-opinion)
;;           update-res (put-request (str "/debate-poll/" dbname) carneades-web-service
;;                             {"authorization" auth-header}
;;                             (json-str new-poll))
;;           res3 (get-request (str "/debate-poll/" dbname "/" (:id poll)) carneades-web-service)
;;           result (read-json (slurp (:body res3)))]
;;       (is (not= 404 (:status update-res)))
;;       (is (= new-opinion (:opinion result))))))


;; (defn make-some-metadata
;;   []
;;   {:key (make-some-string "key")
;;    :contributor (make-some-string "contributor")
;;    :creator (make-some-string "creator")
;;    :coverage (make-some-string "coverage")
;;    :date (make-some-string "date")
;;    :description {:en (make-some-string "description-en")
;;                  :de (make-some-string "description-de")}
;;    :format (make-some-string "format")
;;    :identifier (make-some-string "identifier")
;;    :language (make-some-string "language")
;;    :publisher (make-some-string "publisher")
;;    :relation (make-some-string "relation")
;;    :rights (make-some-string "rights")
;;    :source (make-some-string "source")
;;    :subject (make-some-string "subject")
;;    :title (make-some-string "title")
;;    :type (make-some-string "type")})

;; (defn make-some-statement-data
;;   []
;;   {:header (make-some-metadata)
;;    :text {:en (make-some-string "text-en")
;;           :de (make-some-string "text-de")}})

;; (defn filter-out-nils
;;   [data keys]
;;   (-> data
;;       (update-in [:description] select-keys [:en :de])
;;       (update-in [:header :description] select-keys [:en :de])
;;       (update-in [:text] select-keys [:en :de])
;;       (select-keys keys)))

;; (deftest root-route
;;   (is (= 200 (:status (get-request "/" carneades-web-service)))))

;; (deftest missing-route
;;   (is (= 404 (:status (get-request "/thisservicedoesnotexist" carneades-web-service)))))

;; (deftest post-metadata
;;   (let [data (make-some-metadata)
;;         res (post-request (str "/metadata/" dbname) carneades-web-service
;;                           {"authorization" auth-header}
;;                           (json-str data))
;;         id (:id (read-json (:body res)))
;;         res2 (get-request (str "/metadata/" dbname "/" id) carneades-web-service)
;;         result (read-json (:body res2))
;;         returned-data (filter-out-nils result (keys data))]
;;     (is (= data returned-data))))

;; (deftest put-metadata
;;   (let [data (make-some-metadata)
;;         res (post-request (str "/metadata/" dbname) carneades-web-service
;;                           {"authorization" auth-header}
;;                           (json-str data))
;;         id (:id (read-json (:body res)))
;;         newtitle (make-some-string "Title")
;;         newcreator (make-some-string "Creator")
;;         update {:title [newtitle]
;;                 :creator [newcreator]}
;;         res2 (put-request (format "/metadata/%s/%s" dbname id) carneades-web-service
;;                           {"authorization" auth-header}
;;                           (json-str update))
;;         result (read-json (:body res2))
;;         _ (prn "RESULT=" result)
;;         expected (assoc data :title [newtitle] :creator [newcreator])
;;         returned-data (filter-out-nils result (keys data))]
;;     (prn "RETURNED=" returned-data)
;;     (prn "EXPECTED=" expected)
;;     (is (= expected returned-data))))

;; (deftest delete-metadata
;;   (let [data (make-some-metadata)
;;         res (post-request (str "/metadata/" dbname) carneades-web-service
;;                           {"authorization" auth-header}
;;                           (json-str data))
;;         id (:id (:body res))
;;         res2 (delete-request (format "/metadata/%s/%s" dbname id) carneades-web-service
;;                              {"authorization" auth-header} "")
;;         res3 (get-request (format "/metadata/%s/%s" dbname id) carneades-web-service)]
;;     (is (= 404 (:status res3)))))

;; (deftest post-statement
;;   (let [statement-data (make-some-statement-data)
;;         statement (map->statement statement-data)
;;         res (post-request (str "/statement/" dbname) carneades-web-service
;;                           {"authorization" auth-header}
;;                           (json-str statement))
;;         id (:id (read-json (:body res)))
;;         res2 (get-request (str "/statement/" dbname "/" id) carneades-web-service)
;;         stmt (read-json (:body res2))
;;         _ (prn "STMT=" stmt)
;;         returned-statement (unpack-statement stmt)
;;         _ (prn "UNPACK=" returned-statement)
;;         returned-statement (filter-out-nils returned-statement (keys statement-data))
;;         returned-statement-data (unrecordify returned-statement)
;;         returned-statement-data (update-in returned-statement-data [:header] unrecordify)]
;;     (is (nil? (:atom returned-statement-data)))
;;     (is (= statement-data returned-statement-data))))

;; (deftest put-statement
;;   (let [statement-data (make-some-statement-data)
;;         statement (map->statement statement-data)
;;         res (post-request (str "/statement/" dbname) carneades-web-service
;;                           {"authorization" auth-header}
;;                           (json-str statement))
;;         id (:id (read-json (:body res)))
;;         update (merge statement {:value 0.314})
;;         res2 (put-request (str "/statement/" dbname) carneades-web-service
;;                           {"authorization" auth-header}
;;                           (json-str update))
;;         res3 (get-request (str "/statement/" dbname "/" id) carneades-web-service)
;;         stmt (read-json (:body res3))
;;         returned-statement (unpack-statement stmt)
;;         returned-data (filter-out-nils returned-statement (keys statement-data))]
;;     (is (= 200 (:status res3)))
;;     (is (= 0.314 (:value stmt)))))

;; (deftest put-statement-header
;;   (let [statement-data (make-some-statement-data)
;;         statement (map->statement statement-data)
;;         res (post-request (str "/statement/" dbname) carneades-web-service
;;                           {"authorization" auth-header}
;;                           (json-str statement))
;;         id (:id(read-json (:body res)))
;;         update (merge statement {:header (make-some-metadata)})
;;         res2 (put-request (str "/statement/" dbname) carneades-web-service
;;                           {"authorization" auth-header}
;;                           (json-str update))
;;         res3 (get-request (str "/statement/" dbname "/" id) carneades-web-service)
;;         stmt (read-json (:body res3))
;;         returned-statement stmt ;; (unpack-statement stmt)
;;         returned-data (filter-out-nils returned-statement (keys statement-data))]
;;     (is (= 200 (:status res3)))
;;     (is (= (:header update) (:header returned-data)))))

;; (deftest delete-statement
;;   (let [statement-data (make-some-statement-data)
;;         statement (map->statement statement-data)
;;         res (post-request (str "/statement/" dbname) carneades-web-service
;;                           {"authorization" auth-header}
;;                           (json-str statement))
;;         id (:id (read-json (:body res)))
;;         res (delete-request (format "/statement/%s/%s" dbname id) carneades-web-service
;;                              {"authorization" auth-header} "")
;;         res2 (get-request (str "/statement/" dbname "/" id) carneades-web-service)]
;;     (is (= 200 (:status res)))
;;     (is (= 404 (:status res2)))))

;; (deftest get-scheme
;;   (let [res (get-request "/scheme" carneades-web-service)
;;         schemes (read-json (:body res))
;;         scheme (first (filter (fn [s] (= (:id s) "negative-consequences")) schemes))]
;;     (is scheme)))

;; (deftest post-argument
;;   (let [statement-data (make-some-statement-data)
;;         conclusion-data (make-some-statement-data)
;;         conclusion (map->statement conclusion-data)
;;         premise (pm (map->statement statement-data))
;;         arg (map->argument {:premises [premise]
;;                             :conclusion conclusion})
;;         res (post-request (str "/argument/" dbname) carneades-web-service
;;                           {"authorization" auth-header}
;;                           (json-str arg))
;;         id (:id (read-json (:body res)))
;;         res2 (get-request (str "/argument/" dbname "/" id) carneades-web-service)
;;         returned-data (read-json (:body res2))
        
;;         returned-arg (unpack-argument returned-data)]
;;     (is (= 200 (:status res)))
;;     (is (= (-> conclusion :text :en) (-> returned-arg :conclusion :text :en)))
;;     (is (= (-> premise :text :en) (-> (first (-> returned-arg :premises))
;;                                       :text :en)))))

;; (deftest put-argument
;;   (let [statement-data (make-some-statement-data)
;;         conclusion-data (make-some-statement-data)
;;         conclusion (map->statement conclusion-data)
;;         premise (pm (map->statement statement-data))
;;         arg (map->argument {:premises [premise]
;;                             :conclusion conclusion})
;;         res (post-request (str "/argument/" dbname) carneades-web-service
;;                           {"authorization" auth-header}
;;                           (json-str arg))
;;         id (:id (read-json (:body res)))
;;         new-scheme "(some-other-scheme)"
;;         update (merge arg {:scheme new-scheme})
;;         res2 (put-request (str "/argument/" dbname) carneades-web-service
;;                           {"authorization" auth-header}
;;                           (json-str update))
;;         res3 (get-request (str "/argument/" dbname "/" id) carneades-web-service)
;;         returned-data (read-json (:body res3))
;;         returned-arg (unpack-argument returned-data)]
;;     (is (= 200 (:status res)))
;;     (is (= 200 (:status res2)))
;;     (is (= new-scheme (str (:scheme returned-arg))))))

;; (deftest delete-argument
;;   (let [statement-data (make-some-statement-data)
;;         conclusion-data (make-some-statement-data)
;;         conclusion (map->statement conclusion-data)
;;         premise (pm (map->statement statement-data))
;;         arg (map->argument {:premises [premise]
;;                             :conclusion conclusion})
;;         res (post-request (str "/argument/" dbname) carneades-web-service
;;                           {"authorization" auth-header}
;;                           (json-str arg))
;;         id (:id (read-json (:body res)))
;;         res2 (delete-request (str "/argument/" dbname "/" id) carneades-web-service
;;                              {"authorization" auth-header} "")
;;         res3 (get-request (str "/argument/" dbname "/" id) carneades-web-service)]
;;     (is (= 200 (:status res)))
;;     (is (= 200 (:status res2)))
;;     (is (= 404 (:status res3)))))