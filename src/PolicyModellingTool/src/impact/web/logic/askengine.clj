(ns impact.web.logic.askengine
  (:use clojure.pprint
        impact.web.core
        (carneades.engine caes argument-evaluation argument-graph ask statement scheme argument argument-graph shell unify)
        (impact.web.logic statement-translation answers))
  (:import java.io.File))

(defn- on-solution
  [session]
  (prn "on-solution")
  (let [ag (deref (:future-ag session))
        goal (:goal session)
        _ (prn "substitutions =" (:substitutions session))
        lastsolstmt (apply-substitutions (:substitutions session) (:query session))
        _ (prn "lastsolstmt = " lastsolstmt)
        main-node (get-statement-node ag lastsolstmt)
        _ (prn "main-node = " main-node)
        ag (update-statement-node ag main-node :main true)
        ag (accept ag (vals (:answers session))) ;; accept all answers from the user!
        ag (enter-language ag (-> session :theory :language))
        ag (evaluate carneades-evaluator ag)
        
        ;; _ (prn "before acepting = " )
        ;; _ (pprint ag)
        ;; _ (prn "accepting " (vals (:answers session)))

        ;; _ (prn "after accept")
        ;; _ (pprint ag)
        dbname (store-ag ag)
        session (assoc session
                  :solution (str lastsolstmt) ;; TODO translate solution
                  :db dbname
                  :has-solution true)] ;; could has-solution bu suppressed by testing nil? of :db
    session))

(defn- add-questions
  [existing-questions questions]
  (reduce (fn [existing-questions question]
            (assoc existing-questions (:id question) question)) 
          existing-questions
          questions))

(defn- ask-user
  [session]
  (let [[last-questions last-id] (get-structured-questions (:last-question session)
                                                           (:lang session)
                                                           (:last-id session)
                                                           (:theory session))
        questions (add-questions (:user-questions session) last-questions)]
    (assoc session
      :last-questions last-questions
      :last-id last-id
      :has-solution false
      :user-questions questions)))

(declare continue-engine get-ag-or-next-question)

;; (defmacro with-timeout [millis & body]
;;   `(let [future# (future ~@body)]
;;      (try
;;        (.get future# ~millis java.util.concurrent.TimeUnit/MILLISECONDS)
;;        (catch Exception x# 
;;          (do
;;            (future-cancel future#)
;;            nil)))))

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
     (if (already-answered? lastquestion session)
       (continue-engine session)
       (ask-user session)))
    (do
      (prn "Argument construction is finished!")
      (on-solution session))))

(defn- get-ag-or-next-question
  [session]
  (prn "get-ag-or-next-question")
  (let [future-ag (:future-ag session)]
    (if (future-done? future-ag)
      (on-solution session)
      (do
        (prn "waiting for the question...")
        (on-question session)))))

(defn- start-engine
  [session askables]
  (let [theory (:theory session)
        query (:query session)
        askablefn (fn [k]
                    (let [pred (literal-predicate k)
                          res (contains? askables pred)]
                      res))
        ag (make-argument-graph)
        [argument-from-user-generator questions send-answer]
        (make-argument-from-user-generator askablefn)
        engine (make-engine ag 50 #{} (list (generate-arguments-from-theory theory)
                                            argument-from-user-generator))
        future-ag (future (argue engine query))
        session (assoc session
                  :future-ag future-ag
                  :questions questions
                  :send-answer send-answer
                  :engine-runs true)]
    (get-ag-or-next-question session)))

(defn- continue-engine
  [session]
  (prn "continue engine")
  (let [{:keys [last-question send-answer questions future-ag]} session
        [subs ans] (get-answer last-question session)]
    (prn "sending answer")
    (send-answer ans)
    (prn "send finished")
    (get-ag-or-next-question (assoc session :substitutions subs))))

(defn- get-askables
  [theory]
  (set (map :symbol (filter #(not (nil? (:widget %))) (vals (-> theory :language))))))

(defn ask-engine
  "Returns the modified session."
  [session]
  {:pre [(not (nil? session))]}
  ;; TODO: changes this and get them from the questions.clj
  (let [;; askables '#{person work type-of-use search-type annoucement}
        askables (get-askables (:theory session))
        _ (prn "askables =" askables)
        query (:query session)]
    (if (:engine-runs session)
      (continue-engine session)
      (start-engine session askables))))

