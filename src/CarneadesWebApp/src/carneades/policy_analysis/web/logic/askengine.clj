;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.policy-analysis.web.logic.askengine
  (:use clojure.pprint
        carneades.policy-analysis.web.core
        (carneades.engine aspic argument-evaluation argument-graph ask statement scheme
                          argument shell unify dialog)
        (carneades.policy-analysis.web.logic questions)
        [clojure.tools.logging :only (info debug error)])
  ;; (:require [carneades.database.argument-graph :as ag-db])
  (:import java.io.File))

(defn get-remaining-questions
  [ag session]
  (prn "[get-remaining-questions]")
  (let [{:keys [askables dialog last-id policies lang]} session
        statements (filter (fn [stmt]
                             (and
                              (askable? askables stmt)
                              (empty? (get-answers dialog policies stmt))))
                           (atomic-statements ag))]
    ;; (prn "statements =")
    ;; (prn statements)
    ;; (prn "dialog =")
    ;; (pprint dialog)
    (reduce (fn [[questions id] stmt]
              (let [[new-questions id] (get-structured-questions stmt lang id policies)
                    new-questions (filter (fn [q]
                                            (nil? (get-answers dialog policies (:statement q))))
                                          new-questions)]
                ;; we use a set to avoid duplicate questions
                [(merge questions (apply hash-map
                                         (interleave
                                          (map (comp literal-predicate :statement) new-questions)
                                          new-questions)))
                 id]))
            [{} last-id]
            statements)))

(defn- set-main-issues
  [ag goal]
  (let [main-nodes (filter
                    (fn [s] (= (literal-predicate s) (literal-predicate goal)))
                    (atomic-statements ag))]
    (reduce (fn [ag atom] (update-statement-node ag (get-statement-node ag atom) :main true))
            ag
            main-nodes)))

(defn- on-questions-answered
  [session]
  (prn "[on-questions-answered]")
  (let [ag (:ag session)
        ag (set-main-issues ag (:query session))
        answers (get-in session [:dialog :answers])
        answers-statements (keys answers)
        ;;;; adds all answers to the argument graph since some answers
        ;;;; may have not been created by the rules
        ;;;; ag (reduce enter-statement ag answers-statements)
        ;; accepts answers with a weight of 1.0
        accepted-statements (filter (fn [s] ((answers s) 1.0)) answers-statements)
        ag (accept ag accepted-statements)
        ;; rejects answers with a weight of 0.0
        rejected-statements (filter (fn [s] ((answers s) 0.0)) answers-statements)
        ag (reject ag rejected-statements)
        ag (enter-language ag (-> session :policies :language))
        ag (evaluate aspic-grounded ag)
        project (:project session)
        dbname (store-ag project ag)
        session (assoc session
                  :all-questions-answered true
                  :db dbname)]
    session))

(defn- on-construction-finished
  [session]
  (prn "[on-construction-finished]")
  (let [ag (deref (:future-ag session))
        session (assoc session :ag ag)
        [questions id] (get-remaining-questions ag session)]
    (if (empty? questions)
      (on-questions-answered session)
      (let [questions (vals questions)
            dialog (add-questions (:dialog session) questions)]
        (prn "remaining =" questions)
        (assoc session
          :last-questions questions
          :last-id id
          :dialog dialog)))))

(defn- ask-user
  [session]
  {:pre [(not (nil? (:policies session)))]}
  (let [{:keys [last-question lang last-id policies]} session
        [last-questions last-id] (get-structured-questions last-question
                                                           lang
                                                           last-id
                                                           policies)
        dialog (add-questions (:dialog session) last-questions)]
    (assoc session
      :last-questions last-questions
      :last-id last-id
      :dialog dialog)))

(declare continue-engine get-ag-or-next-question)

(defn receive-question
  [session]
  (first (:questions session)))

(defn- on-question
  [session]
  (if-let [question (receive-question session)]
    (let [send-answer (:send-answer session)
          questions (rest (:questions session))
          [lastquestion substitutions] question
          session (assoc session
                    :substitutions substitutions
                    :last-question lastquestion
                    :questions questions)]
      (if-let [answers (get-answers (:dialog session) (:policies session) lastquestion)]
        (continue-engine session answers)
        (ask-user session)))
    ;; else no more question == construction finished
    (do
      (prn "[askengine] argument construction is finished!")
      (on-construction-finished session))))

(defn- get-ag-or-next-question
  [session]
  (prn "[get-ag-or-next-question]")
  (cond (:ag session) (on-questions-answered session)
        (future-done? (:future-ag session)) (on-construction-finished session)
        :else (do
                (prn "[askengine] waiting for the question...")
                (on-question session))))

(defn start-engine
  ([session ag]
     {:pre [(not (nil? (:policies session)))]}
     (info "Starting the query process")
     (let [policy (:policies session)
           query (:query session)
           [argument-from-user-generator questions send-answer]
           (make-argument-from-user-generator (fn [p] (askable? policy p)))
           engine (make-engine ag 500 #{} (list (generate-arguments-from-theory policy)
                                                argument-from-user-generator))
           future-ag (future (argue engine query))
           session (assoc session
                     :future-ag future-ag
                     :questions questions
                     :send-answer send-answer
                     :dialog (make-dialog)
                     :last-id 0)]
       (get-ag-or-next-question session)))
  ([session]
     (start-engine session (make-argument-graph))))

(defn- continue-engine
  [session answers]
  (let [{:keys [send-answer questions]} session]
    (send-answer (build-answer (:substitutions session)
                               (:last-question session)
                               answers))
    (get-ag-or-next-question session)))

(defn send-answers-to-engine
  "Returns the modified session."
  [session]
  {:pre [(not (nil? session))]}
  (info "Sending answers back to the engine")
  (let [answers (get-answers (:dialog session) (:policies session) (:last-question session))]
    (continue-engine session answers)))
