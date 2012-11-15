(ns impact.web.logic.askengine
  (:use clojure.pprint
        impact.web.core
        (carneades.engine aspic argument-evaluation argument-graph ask statement scheme
                          argument argument-graph shell unify dialog)
        (impact.web.logic questions))
  (:import java.io.File))

(defn askable?
  [theory p]
  (let [predicate (get-in theory [:language (literal-predicate p)])]
    (and (or (:askable predicate) (:widgets predicate))
         (or (predicate? predicate)
             (concept? predicate)
             (and (role? predicate)
                  (ground? (first (term-args p))))))))

(defn get-remaining-questions
  [ag session]
  (prn "[get-remaining-questions]")
  (let [{:keys [askables dialog last-id theory lang]} session
        statements (filter (fn [stmt]
                             (and
                              (askable? askables stmt)
                              (empty? (get-answers dialog stmt))))
                           (atomic-statements ag))]
    (prn "statements =")
    (prn statements)
    (prn "dialog =")
    (pprint dialog)
    (reduce (fn [[questions id] stmt]
              (let [[new-questions id] (get-structured-questions stmt lang id theory)
                    new-questions (filter (fn [q]
                                            (nil? (get-answers dialog (:statement q))))
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
        _ (prn "main issue?")
        _ (prn (:query session))
        ;; _ (pprint ag)
         ;; accept all answers from the user!
        ag (accept ag (apply concat (vals (get-in session [:dialog :answers]))))
        ag (enter-language ag (-> session :theory :language))
        ag (evaluate aspic-grounded ag)
        dbname (store-ag ag)
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
  (let [{:keys [last-question lang last-id theory]} session
        [last-questions last-id] (get-structured-questions last-question
                                                           lang
                                                           last-id
                                                           theory)
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
      (if-let [answers (get-answers (:dialog session) lastquestion)]
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

(defn- start-engine
  [session]
  (let [theory (:theory session)
        query (:query session)
        ag (make-argument-graph)
        [argument-from-user-generator questions send-answer]
        (make-argument-from-user-generator (fn [p] (askable? theory p)))
        engine (make-engine ag 500 #{} (list (generate-arguments-from-theory theory)
                                            argument-from-user-generator))
        future-ag (future (argue engine query))
        session (assoc session
                  :future-ag future-ag
                  :questions questions
                  :send-answer send-answer
                  :engine-runs true)]
    (get-ag-or-next-question session)))

(defn- continue-engine
  [session answers]
  (let [{:keys [send-answer questions]} session]
    (send-answer (build-answer (:substitutions session)
                               (:last-question session)
                               answers))
    (get-ag-or-next-question session)))

(defn ask-engine
  "Returns the modified session."
  [session]
  {:pre [(not (nil? session))]}
  (if (:engine-runs session)
    (let [answers (get-answers (:dialog session) (:last-question session))]
      (continue-engine session answers))
    ;; else
    (do
      (prn "[ask-engine]")
      (prn "query =" (:query session))
      (start-engine session))))
