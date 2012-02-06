(ns carneades.web.test.service
  (:use clojure.test
        carneades.engine.uuid
        carneades.web.service
        carneades.engine.dublin-core
        clojure.data.json)
  (:require [carneades.database.db :as db])
  (:import java.io.File))

;; (def tmpdir (System/getProperty "java.io.tmpdir"))

(def dbname (str "testdb-" (uuid->string (make-uuid))))
(def dbfilename (str db/default-db-host "/" dbname ".h2.db"))

(def auth-header "Basic cm9vdDpwdzE=")

(defn make-some-string [prefix]
  (str prefix "-" (gensym)))

(defn make-some-metadata
  []
  {:contributor (make-some-string "contributor")
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
  (.delete (File. dbfilename)))

(defn db-fixture [x] (create-tmp-db) (x) (delete-tmp-db))

(use-fixtures :once db-fixture) 

(deftest root-route-test
  (is (= 200 (:status (request "/" carneades-web-service)))))

(deftest missing-route-test
  (is (= 404 (:status (request "/thisservicedoesnotexist" carneades-web-service)))))

(deftest post-metadata-test
  (let [data (make-some-metadata)
        res (post-request (str "/metadata/" dbname) carneades-web-service
                          {"authorization" auth-header}
                          (json-str data))
        id (:body res)
        res2 (request (str "/metadata/" dbname "/" id) carneades-web-service)
        returned-data (select-keys (read-json (:body res2)) (keys data))
        ;; add here the other languages keys if you change the make-some-metadata function
        returned-data (update-in returned-data [:description] select-keys [:en :de])]
    (is (= returned-data data))))

