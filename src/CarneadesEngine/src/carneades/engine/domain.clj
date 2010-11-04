
(ns carneades.engine.domain
  (:use clojure.contrib.def
    ;clojure.contrib.pprint
    ;[clojure.set :only (intersection)]
    ;carneades.engine.utils
    carneades.engine.argument
    ;carneades.engine.statement
    carneades.engine.owl.reasoner
    [carneades.engine.search :only (breadth-first search)]
    ;[carneades.engine.dnf :only (to-dnf)]
    carneades.engine.unify
    )
  (:require [carneades.engine.argument-search :as as]))

(defstruct named-clause
  :id ;; symbol
  :rule ;; rule-id
  :strict ;; rule-strict?
  :domains ;; (seq-of statement)
  :head ;; rule-head
  :clause ;; the actual clause
  )

(defn inst-domain
  [domain subs ont]
  (let [type-states (as/find-best-arguments
                      search
                      breadth-first
                      nil
                      0
                      (as/state
                        domain
                        :pro
                        (list (list domain))
                        '()
                        *empty-argument-graph*
                        subs
                        '())
                      (list (generate-arguments-from-reasoner (:ontology ont) (:reasoner ont))))]
    (map :substitutions type-states)))

(defn get-subs
  [domain old-subs ont]
  (let [new-subs (map (fn [s]
                        (inst-domain domain s ont))
                   old-subs)]
    (apply concat new-subs)))

(let [domain-counter (atom 0)]
  (letfn [(reset-domain-counter []
            (reset! domain-counter 0))
          (get-domain-number [n]
            (swap! domain-counter inc)
            (symbol (str n "-" @domain-counter)))]
    (defn instantiate-domains
      [nc ont old-subs]
;      (println "---------")
;      (println "instantiating for clause:" nc)
      (let [domains (:domains nc),
            subs (reduce (fn [s g] (get-subs g s ont)) (list old-subs) domains)]
;        (println "instantiating" (count domains) "domains")
;        (println "old subs:" old-subs)
;        (println (count subs) "instantiations found:")
;        (println subs)
        (reset-domain-counter)
;        (doall ; TODO : only for debugging
          (map
            (fn [s]
              (let [new-id (get-domain-number (:id nc))]
;                (println new-id)
;                (println s)
;                (println "instantiated domains:" (map (fn [t] (apply-substitution s t)) domains))
;                (println "instantiated-clause:" (map (fn [t] (apply-substitution s t)) (:clause nc)))
                {:clause (struct named-clause
                           new-id
                           (:rule nc)
                           (:strict nc)
                           (:domains nc)
                           (map (fn [t] (apply-substitution s t)) (:head nc))
                           (map (fn [t] (apply-substitution s t)) (:clause nc))),
                 :subs s}))
            subs)
;          )
        ))))


