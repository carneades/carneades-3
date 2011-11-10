(ns impact.web.logic.askengine
  (:use clojure.pprint
        carneades.engine.lkif
        carneades.engine.statement
        carneades.engine.argument
        carneades.engine.shell
        carneades.engine.unify
        impact.web.logic.statement-translation
        impact.web.logic.answers
        impact.web.core)
  (:import java.io.File))

(defn- on-solution
  [solutions service-data]
  (let [goal (:goal service-data)
        ;; TODO: why two last?
        lastsol (last (last solutions))
        ;; _ (do (prn "lastsol =") (pprint lastsol))
        lastsubs (:substitutions lastsol)
        ;; _ (do (prn "lastsubs =") (prn lastsubs))
        lastsolstmt (apply-substitution lastsubs goal)
        ;; _ (do (prn "lastsolstmt =") (prn lastsolstmt))
        solag (unite-solutions (last solutions))
        ;; _ (do (prn "solag =") (pprint solag))
        solag (assoc solag :main-issue lastsolstmt)
        pathname (store-ag solag)
        service-data (assoc service-data
                       :solution (str lastsolstmt) ;; TODO translate solution
                       :lkifsol-pathname pathname
                       :has-solution true)]
    service-data))

(def send-engine-msg)

(def questionsdata-url (if *debug*
                         "http://localhost:8080/kb/questions.clj"
                         "http://localhost:8080/PolicyModellingTool2/kb/questions.clj") )

(defn- on-askuser
  [service-data]
  (let [questionsdata (load-questions questionsdata-url)
        [questions last-id] (get-structured-questions (:last-question service-data)
                                                      (:lang service-data)
                                                      (:last-id service-data)
                                                      questionsdata)]
    (assoc service-data :last-questions questions :last-id last-id
           :has-solution false)))

(defn- on-answer
  [answer service-data]
  (prn "ON-ANSWER")
  (let [[lastquestion state to-engine from-engine] answer
        service-data (assoc service-data
                       :to-engine to-engine
                       :from-engine from-engine
                       :state state
                       :last-question lastquestion)]
    ;; (prn "last-question is")
    ;; (prn lastquestion)
    ;; (prn "answers =")
    ;; (prn (:answers service-data))
    ;; (prn "already-answer ?")
    ;; (prn (already-answered? lastquestion service-data))
    (if (already-answered? lastquestion service-data)
      (let [answer (get-answer lastquestion service-data)]
        ;; (prn "already-answered, answer is")
        ;; (prn answer)
        (send-engine-msg answer service-data))
      (on-askuser service-data))))

(defn- send-engine-msg
  [msg service-data]
  (prn "SENDING ------------------------>")
  (deliver (:to-engine service-data) msg)
  (let [response (deref (:from-engine service-data))]
    (prn "<------------------------------ RESPONSE")
    (condp = (first response)
      ;; TODO replace by keywords
      'solution (on-solution (rest response) service-data)
      'ask (on-answer (rest response) service-data))))

(defn- start-engine
  [query kb askables service-data]
  (let [lkif (import-lkif kb)
        goal query
        {:keys [to-engine from-engine]} service-data
        generators (list (generate-arguments-from-lkif lkif))
        ;; TODO: replace this function by a set
        askablefn (fn [k]
                    (let [pred (statement-predicate k)
                          res (contains? askables pred)]
                      res))
        ag (if (nil? (:ags lkif))
             *empty-argument-graph*
             (first (:ags lkif)))
        fcons (future-construction to-engine from-engine)
        maxnodes 50
        maxturns 2
        response (send-engine-msg (list goal maxnodes maxturns ag generators askablefn) service-data)]
    response))

(defn pr-service-data
  [res]
  (prn (dissoc res :to-engine :from-engine)))

(defn- continue-engine
  [service-data]
  (let [{:keys [last-question]} service-data
        ans (get-answer last-question service-data)
        response (send-engine-msg ans service-data)]
    response))

(defn- call-engine
  [service-data query kb askables]
  (let [{:keys [state answers]} service-data]
    (if (nil? state)
      (start-engine query kb askables service-data)
      (continue-engine service-data))))

(defn ask-engine
  [service-data query kb]
  {:pre [(not (nil? service-data))]}
  (let [askables '#{hatName betrifftWerk zumZweck UrheberSuche Bekanntmachung}
        response (call-engine service-data query kb askables)]
    response))

