(ns carneades.web.test.service
  (:use clojure.test
        carneades.engine.statement
        carneades.engine.uuid
        carneades.web.service
        carneades.engine.dublin-core
        clojure.data.json)
  (:require [carneades.database.db :as db])
  (:import java.io.File))

;; (def tmpdir (System/getProperty "java.io.tmpdir"))

(def dbname (str "testdb-" (uuid->string (make-uuid))))
(def dbfilename (str db/default-db-host "/" dbname ".h2.db"))

(def auth-header "Basic cm9vdDpwdzE=") ;; encoded root:pw1

(defn make-some-string [prefix]
  (str prefix "-" (gensym)))

(defn make-some-metadata
  []
  {:key (make-some-string "key")
   :contributor (make-some-string "contributor")
   :creator (make-some-string "creator")
   :coverage (make-some-string "coverage")
   :date (make-some-string "date")
   :description {:en (make-some-string "description-en")
                 :de (make-some-string "description-de")}
   :format (make-some-string "format")
   :identifier (make-some-string "identifier")
   :language (make-some-string "language")
   :publisher (make-some-string "publisher")
   :relation (make-some-string "relation")
   :rights (make-some-string "rights")
   :source (make-some-string "source")
   :subject (make-some-string "subject")
   :title (make-some-string "title")
   :type (make-some-string "type")})

(defn make-some-statement-data
  []
  {:header (make-some-metadata)
   :text {:en (make-some-string "text-en")
          :de (make-some-string "text-de")}})

(defn create-tmp-db
  []
  (printf "Creating %s\n" dbfilename)
  (db/create-argument-database 
   dbname 
   "root" 
   "pw1" 
   (make-metadata)))

(defn delete-tmp-db
  []
  (printf "Deleting %s\n" dbfilename)
  (.delete (File. dbfilename)))

(defn db-fixture [x] (create-tmp-db) (x) (delete-tmp-db))

(use-fixtures :once db-fixture) 

(deftest root-route-test
  (is (= 200 (:status (get-request "/" carneades-web-service)))))

(deftest missing-route-test
  (is (= 404 (:status (get-request "/thisservicedoesnotexist" carneades-web-service)))))

(defn filter-out-nils
  [data keys]
  (-> data
      (update-in [:description] select-keys [:en :de])
      (update-in [:header :description] select-keys [:en :de])
      (update-in [:text] select-keys [:en :de])
      (select-keys keys)))

(deftest post-metadata-test
  (let [data (make-some-metadata)
        res (post-request (str "/metadata/" dbname) carneades-web-service
                          {"authorization" auth-header}
                          (json-str data))
        id (:body res)
        res2 (get-request (str "/metadata/" dbname "/" id) carneades-web-service)
        returned-data (filter-out-nils (read-json (:body res2)) (keys data))]
    (is (= data returned-data))))

(deftest put-metadata-test
  (let [data (make-some-metadata)
        res (post-request (str "/metadata/" dbname) carneades-web-service
                          {"authorization" auth-header}
                          (json-str data))
        id (:body res)
        newtitle (make-some-string "Title")
        newcreator (make-some-string "Creator")
        update {:title newtitle
                :creator newcreator}
        res2 (put-request (format "/metadata/%s/%s" dbname id) carneades-web-service
                          {"authorization" auth-header}
                          (json-str update))
        res3 (get-request (format "/metadata/%s/%s" dbname id) carneades-web-service)
        expected (assoc data :title newtitle :creator newcreator)
        returned-data (filter-out-nils (read-json (:body res3)) (keys data))]
    (is (= "true" (:body res2)))
    (is (= expected returned-data))))

(deftest delete-metadata-test
  (let [data (make-some-metadata)
        res (post-request (str "/metadata/" dbname) carneades-web-service
                          {"authorization" auth-header}
                          (json-str data))
        id (:body res)
        res2 (delete-request (format "/metadata/%s/%s" dbname id) carneades-web-service
                             {"authorization" auth-header} "")
        res3 (get-request (format "/metadata/%s/%s" dbname id) carneades-web-service)]
    (is (= "true" (:body res2)))
    (is (= 404 (:status res3)))))

(deftest post-statement-test
  (let [statement-data (make-some-statement-data)
        statement (map->statement statement-data)
        res (post-request (str "/statement/" dbname) carneades-web-service
                          {"authorization" auth-header}
                          (json-str statement))
        id (read-json (:body res))
        res2 (get-request (str "/statement/" dbname "/" id) carneades-web-service)
        returned-data (filter-out-nils (read-json (:body res2)) (keys statement-data))]
    ;; PROBLEMS:
    ;; id is returned as string
    ;; standard is returned as string
    ;; value nil is set to 0.5
    (prn "returned-data" returned-data)
    (is (= statement-data returned-data)))
  )