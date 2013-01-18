;;; Copyright (c) 2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.database.test.admin
  (:use [clojure.test :only [deftest is use-fixtures]]
        [carneades.engine.uuid :only [make-uuid-str]])
  (:require [carneades.database.admin :as admin]
            [carneades.database.db :as db]
            [clojure.java.jdbc :as jdbc])
  (:import java.io.File))

(def dbname (str "testdb-debate-" (make-uuid-str)))

(def debate1name (make-uuid-str))

(defn create-tmp-db
  []
  (admin/create-debate-database dbname "root" "pw1")
  (db/with-db (db/make-database-connection dbname "root" "pw1")
    (admin/create-debate {:id debate1name :public false})))

(defn delete-tmp-db
  []
  (.delete (File. (db/dbfilename dbname))))

(defn db-fixture [x] (create-tmp-db) (x) (delete-tmp-db))

(use-fixtures :once db-fixture) 

(deftest test-create-poll
  (db/with-db (db/make-database-connection dbname "root" "pw1")
    (let [userid (make-uuid-str)
          casedb (make-uuid-str)
          poll-to-create {:mainissueatompredicate "may-publish"
                          :casedb casedb
                          :opinion 0.1234
                          :userid userid}
          returned-pollid (admin/create-poll debate1name poll-to-create [])
          polls (admin/list-polls debate1name)
          poll (first (filter (fn [p] (= (:userid p) userid)) polls))]
      (is (not (nil? poll)))
      (is (= (:opinion poll-to-create) (:opinion poll)))
      (is (= (:mainissueatompredicate poll-to-create) (:mainissueatompredicate poll))))))

(deftest test-read-poll
  (db/with-db (db/make-database-connection dbname "root" "pw1")
    (let [userid (make-uuid-str)
          casedb (make-uuid-str)
          poll-to-create {:mainissueatompredicate "may-publish"
                          :casedb casedb
                          :opinion 0.1234
                          :userid userid}
          pollid (admin/create-poll debate1name poll-to-create [])
          poll (admin/read-poll pollid)]
      (is (not (nil? poll)))
      (is (= (:opinion poll-to-create) (:opinion poll)))
      (is (= (:mainissueatompredicate poll-to-create) (:mainissueatompredicate poll)))
      (is (= userid (:userid poll))))))

(deftest test-update-poll
  (db/with-db (db/make-database-connection dbname "root" "pw1")
    (let [userid (make-uuid-str)
          casedb (make-uuid-str)
          poll-to-create {:mainissueatompredicate "may-publish"
                          :casedb casedb
                          :opinion 0.1234
                          :userid userid}
          pollid (admin/create-poll debate1name poll-to-create [])
          poll (admin/read-poll pollid)
          new-opinion 0.654
          modification-success (admin/update-poll pollid {:opinion new-opinion})
          modified-poll (admin/read-poll pollid)]
      (is modification-success)
      (is (= new-opinion (:opinion modified-poll))))))

(deftest test-count-polls-for-debate
  (db/with-db (db/make-database-connection dbname "root" "pw1")
    (jdbc/do-commands "DELETE FROM policy"
                      "DELETE FROM vote"
                      "DELETE FROM poll")
    (let [userid (make-uuid-str)
          casedb (make-uuid-str)
          poll-to-create {:mainissueatompredicate "may-publish"
                          :casedb casedb
                          :opinion 0.1234
                          :userid userid}
          casedb (make-uuid-str)
          poll-to-create2 {:mainissueatompredicate "may-publish"
                          :casedb casedb
                          :opinion 0.5
                          :userid userid}
          pollid (admin/create-poll debate1name poll-to-create [])
          pollid2 (admin/create-poll debate1name poll-to-create2 [])]
      (is (= 2 (admin/count-polls-for-debate debate1name))))))

(deftest test-get-policies-for-debate
  (db/with-db (db/make-database-connection dbname "root" "pw1")
    (jdbc/do-commands "DELETE FROM policy"
                      "DELETE FROM vote"
                      "DELETE FROM poll")
    (let [userid (make-uuid-str)
          casedb (make-uuid-str)
          poll-to-create {:mainissueatompredicate "may-publish"
                          :casedb casedb
                          :opinion 0.1234
                          :userid userid}
          casedb (make-uuid-str)
          poll-to-create2 {:mainissueatompredicate "may-publish"
                          :casedb casedb
                          :opinion 0.5
                          :userid userid}
          pollid (admin/create-poll debate1name poll-to-create ["a" "b" "c"])
          pollid2 (admin/create-poll debate1name poll-to-create2 ["b" "c"])
          policies (admin/get-policies-for-debate debate1name)
          fpolicies (frequencies policies)]
      (is (= 5 (count policies))))))