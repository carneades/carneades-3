;; Copyright (c) 2013 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.database.test.admin
  (:use [carneades.engine.uuid :only [make-uuid-str]])
  (:require [carneades.database.case :as case]
            [carneades.database.db :as db]
            [carneades.engine.utils :as utils]
            [clojure.java.jdbc.deprecated :as jdbc]
            [clojure.java.io :as io]
            [carneades.project.fs :as project]
            [midje.sweet :refer :all])
  (:import java.io.File))

(def state (atom nil))

(defn initial-state-value
  []
  {:project-name (str "testproject-" (make-uuid-str))
   :dbname (str "testdb-debate-" (make-uuid-str))
   :debate1name (make-uuid-str)})

(defn create-database
  []
  (reset! state (initial-state-value))
  (let [{:keys [project-name dbname debate1name]} @state]
    (case/create-debate-database project-name dbname  "root" "pw1")
    (db/with-db (db/make-connection project-name dbname "root" "pw1")
      (case/create-debate {:id debate1name :public false}))))

(defn delete-database
  []
  (utils/delete-file-recursively (project/get-project-path (:project-name @state))))

(with-state-changes [(before :facts (create-database))
                     (after :facts (delete-database))]
  (fact "It is possible to create a poll."
        (db/with-db (db/make-connection (:project-name @state)
                                        (:dbname @state)
                                        "root"
                                        "pw1")
         (let [userid (make-uuid-str)
               casedb (make-uuid-str)
               poll-to-create {:mainissueatompredicate "may-publish"
                               :casedb casedb
                               :opinion 0.1234
                               :userid userid}
               debate1name (:debate1name @state)
               returned-pollid (case/create-poll debate1name poll-to-create [])
               polls (case/list-polls debate1name)
               poll (first (filter (fn [p] (= (:userid p) userid)) polls))]
           (expect (:opinion poll) => (:opinion poll-to-create))
           (expect (:mainissueatompredicate poll) => (:mainissueatompredicate poll-to-create)))))

  (fact "The reading of a poll is coherent."
        (db/with-db (db/make-connection (:project-name @state) (:dbname @state) "root" "pw1")
          (let [userid (make-uuid-str)
                casedb (make-uuid-str)
                poll-to-create {:mainissueatompredicate "may-publish"
                                :casedb casedb
                                :opinion 0.1234
                                :userid userid}
                debate1name (:debate1name @state)
                pollid (case/create-poll debate1name poll-to-create [])
                poll (case/read-poll pollid)]
            (expect (:opinion poll) => (:opinion poll-to-create))
            (expect (:mainissueatompredicate poll) => (:mainissueatompredicate poll-to-create))
            (expect (:userid poll) => userid))))

  (fact "Updating a poll works."
        (db/with-db (db/make-connection (:project-name @state) (:dbname @state) "root" "pw1")
          (let [userid (make-uuid-str)
                casedb (make-uuid-str)
                poll-to-create {:mainissueatompredicate "may-publish"
                                :casedb casedb
                                :opinion 0.1234
                                :userid userid}
                debate1name (:debate1name @state)
                pollid (case/create-poll debate1name poll-to-create [])
                poll (case/read-poll pollid)
                new-opinion 0.654
                modification-success (case/update-poll pollid {:opinion new-opinion})
                modified-poll (case/read-poll pollid)]
            (expect modification-success => true)
            (expect (:opinion modified-poll) => new-opinion))))

  (fact "Counting the result of a poll works."
        (db/with-db (db/make-connection (:project-name @state) (:dbname @state) "root" "pw1")
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
                debate1name (:debate1name @state)
                pollid (case/create-poll debate1name poll-to-create [])
                pollid2 (case/create-poll debate1name poll-to-create2 [])]
            (expect (case/count-polls-for-debate debate1name) => 2))))

  (fact "Getting the preferred policies of a debate works."
        (db/with-db (db/make-connection (:project-name @state) (:dbname @state) "root" "pw1")
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
                debate1name (:debate1name @state)
                pollid (case/create-poll debate1name poll-to-create ["a" "b" "c"])
                pollid2 (case/create-poll debate1name poll-to-create2 ["b" "c"])
                policies (case/get-policies-for-debate debate1name)
                fpolicies (frequencies policies)]
            (expect (count policies) => 5)))))
