(ns carneades.web.modules.project.routes-test
  (:require [midje.sweet :refer :all]
            [ring.mock.request :refer :all]
            [taoensso.timbre :as timbre :refer [debug info spy]]
            [carneades.web.handler :refer [app]]
            [cheshire.core :refer [parse-string encode]]
            [carneades.engine.utils :as utils]
            [carneades.engine.uuid :refer [make-uuid-str]]
            [carneades.project.admin :as project]
            [carneades.database.admin :as db]))

;; TODO: creates a temporary project directory

(def base-url "/carneades/api")
(def user "root")
(def password "pw1")

(def state (atom nil))

(defn initial-state-value
  []
  {:project-name (str "testproject-" (make-uuid-str))})

(defn create-project
  []
  (reset! state (initial-state-value))
  (db/create-missing-dbs (:project-name @state) user password))

(defn delete-project
  []
  (utils/delete-file-recursively (project/get-project-path (:project-name @state))))

(defn parse
  [s]
  (parse-string s true))

(defn get-rule-value
  [rules sid]
  (:value (first (filter #(= (:ruleid %) sid) rules))))


(with-state-changes [(before :facts (create-project))
                     (after :facts (delete-project))]
  (fact "It is possible to post a profile and read it back."
        (let [project (:project-name @state)
              profile {:metadata {:title "One profile"}
                      :rules '[{:ruleid r1-a
                                :value 1.0}
                               {:ruleid r2-b
                                :value 0.0}
                               {:ruleid r3-c
                                :value 0.5}]
                      :default true}
             response (app (-> (request :post
                                        (str base-url
                                             "/projects/"
                                             project
                                             "/legalprofiles/"))
                               (body (encode profile))
                               (content-type "application/json")))
             body (parse (:body response))
             id (:id body)
             response2 (app (-> (request :get
                                         (str base-url
                                              "/projects/"
                                              project
                                              "/legalprofiles/"
                                              id))
                                (content-type "application/json")))
             profile' (parse (:body response2))]
         (expect (select-keys (:metadata profile') (keys (:metadata profile))) =>
                 (:metadata profile))
         (expect (:default profile') => true)
         (expect (get-rule-value (:rules profile') "r1-a") => 1.0)
         (expect (get-rule-value (:rules profile') "r2-b") => 0.0)
         (expect (get-rule-value (:rules profile') "r3-c") => 0.5))))

(fact "It is possible to update the metadata of a profile.")
