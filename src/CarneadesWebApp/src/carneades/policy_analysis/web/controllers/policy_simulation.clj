;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.policy-analysis.web.controllers.policy-simulation
  (:use [clojure.data.json :only [json-str read-json]]
        [clojure.tools.logging :only [info debug error]]
        [carneades.engine.policy :only [get-main-issue]]
        [carneades.engine.utils :only [safe-read-string exists?]]
        [carneades.policy-analysis.web.logic.askengine :only [start-engine send-answers-to-engine]]
        [carneades.policy-analysis.web.logic.questions :only [get-predicate get-questions-for-answers-modification
                                           modify-statements-weights
                                           pseudo-delete-statements]]
        [carneades.policy-analysis.web.views.pages :only [index-page config-page]]
        [carneades.engine.unify :only [apply-substitutions]]
        [carneades.engine.dialog :only [get-nthquestion add-answers]]
        [ring.util.codec :only [base64-decode]]
        [carneades.database.export :only [export-to-argument-graph]]
        [clojure.pprint :only [pprint]])
  (:require [carneades.database.db :as db]
            [carneades.database.case :as case]
            [carneades.project.admin :as project]
            [clojure.string :as str]
            [carneades.policy-analysis.web.controllers.reconstruction :as recons]))

(defmulti ajax-handler (fn [json _ _] (ffirst json)))

(defn strs->stmt
  "Converts a collection of a string representing a statement on the client side
   to a formal statement."
  [coll]
  (map symbol (apply list coll)))

(defmethod ajax-handler :request
  [json session request]
  (debug "======================================== request handler! ==============================")
  (let [{:keys [question project]} (:request json)
        ;; _ (pprint "project=") _ (pprint project)
        policy (project/load-theory (:id project) (:policies project))
        ;; _ (pprint "policy=") _ (pprint policy)
        session (assoc session
                  :query (get-main-issue policy (symbol question))
                  :policies policy)
        session (start-engine session)]
    {:session session
     :body (json-str {:questions (:last-questions session)})}))

(defn questions-or-solution
  [session]
  (if (:all-questions-answered session)
      {:session session
       :body (json-str {:db (:db session)})}
      {:session session
       :body (json-str {:questions (:last-questions session)})}))

(defmethod ajax-handler :answers
  [json session request]
  (debug "======================================== answers handler! ==============================")
  (debug json)
  (let [{:keys [last-questions dialog project]} session
        policy (:policies session)
        questions-to-answers (recons/reconstruct-answers (:answers json)
                                                         dialog
                                                         policy)
        ;; _ (do (prn "[:answers] questions-to-answers =" questions-to-answers))
        session (update-in session [:dialog] add-answers questions-to-answers)
        ;; _ (do (prn "[:answers] dialog answers =" (:dialog session)))
        session (send-answers-to-engine session)]
    (questions-or-solution session)))

(defmethod ajax-handler :modifiable-facts
  [json session request]
  (let [jsondata (json :modifiable-facts)
        db (jsondata :db)
        policy (jsondata :policies)
        project (jsondata :project)]
    {:body (json-str (get-questions-for-answers-modification
                      project
                      db
                      policy
                      (keyword (:lang session))))}))

(defn get-username-and-password
  [request]
  (let [authorization (second (str/split (get-in request [:headers "authorization"]) #" +"))
        authdata (String. (base64-decode authorization))]
    (str/split authdata #":")))

(defmethod ajax-handler :modify-facts
  [json session request]
  (let [data (:modify-facts json)
        facts (:facts data)
        db (:db data)
        project (:project data)
        theory (project/load-theory project (:policies data))
        facts (recons/reconstruct-statements facts)
        to-modify (mapcat (fn [q] (recons/reconstruct-answer q theory (:values q))) facts)
        [username password] (get-username-and-password request)]
    (pseudo-delete-statements project db username password (:deleted data))
    (modify-statements-weights project db username password to-modify theory)
    (let [dbconn (db/make-connection project db username password)
          ag (export-to-argument-graph dbconn)
          ;; restarts the engine to expand new rules
          ;; that could be now reachable with the new facts
          ;; session (start-engine session ag)
          session (assoc session :all-questions-answered true :db db)
          ]
      (questions-or-solution session)))
  )

(defn new-session
  [lang]
  {:pre [(not (nil? lang))]}
  (info "[new-session] lang =" lang)
  ;; (info "current-policy: " (deref current-policy))
  {:lang lang
   :project "copyright"})

(defmethod ajax-handler :reset
  [json session request]
  (info "[reset] json=" json)
  {:session (new-session (get-in json [:reset :lang]))})

(defmethod ajax-handler :lang
  [json session request]
  (info "[lang] new language is" (:lang json))
  {:session (assoc session :lang (:lang json))})

(defn process-ajax-request
  [request]
  (let [{:keys [session body params]} request
        json (read-json (slurp body))
        json (dissoc json :uuid)
        _ (info "JSON =")
        _ (info json)
        res (ajax-handler json session request)]
    res))

(defn init-page
  []
  (info "init of session")
  {:headers {"Content-Type" "text/html;charset=UTF-8"}
   :session (new-session "en")
   :body (index-page)})

(defn dump-config
  []
  (config-page))
