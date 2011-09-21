(ns impact.web.askengine
  (:use clojure.pprint
        carneades.engine.lkif
        carneades.engine.statement
        carneades.engine.argument
        carneades.engine.shell
        carneades.engine.unify
        impact.web.translate
        impact.web.answers))

(defn- on-solutions
  [solutions goal]
  (prn "ON-SOLUTIONS")
  (let [lastsol (last solutions)
        lastsubs (:substitutions lastsol)
        lastsolstmt (apply-substitution lastsubs goal)
        solag (unite-solutions solutions)
        solag (assoc solag :main-issue lastsolstmt)
        ]))

(def send-engine-msg)

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
      (do
        ;; (prn "not already answer")
        {:type :askuser :service-data service-data}))))

(defn- send-engine-msg
  [msg service-data]
  ;; (prn "SENDING ------------------------>")
  (deliver (:to-engine service-data) msg)
  (let [response (deref (:from-engine service-data))]
    ;; (prn "<------------------------------ RESPONSE")
    ;; (prn "first of response =")
    ;; (prn (first response))
    (condp = (first response)
      ;; TODO replace by keywords
      'solution (on-solutions (rest response) (first msg))
      'ask (on-answer (rest response) service-data))))

(defn- on-askuser
  [response service-data]
  (let [translations (load-translations "http://localhost:8080/kb/translations.xml")
        service-data (:service-data response)
        [questions last-id] (get-structured-questions (:last-question service-data) "en" (:last-id service-data) translations)]
    (assoc service-data :last-questions questions :last-id last-id)))

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

(defn- pr-res
  [res]
  (dissoc res :to-engine :from-engine))

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
    (condp :type response
      :askuser (on-askuser response service-data)
      )))

