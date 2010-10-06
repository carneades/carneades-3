
(ns carneades.engine.guard
  (:use clojure.contrib.def
    ;clojure.contrib.pprint
    ;[clojure.set :only (intersection)]
    ;carneades.engine.utils
    carneades.engine.argument
    ;carneades.engine.statement
    carneades.engine.owl.reasoner
    [carneades.engine.search :only (breadth-first search)]
    ;[carneades.engine.dnf :only (to-dnf)]
    ;[carneades.engine.unify :only (genvar unify rename-variables)]
    )
  (:require [carneades.engine.argument-search :as as]))

(defstruct named-clause
  :id ;; symbol
  :rule ;; rule-id
  :strict ;; rule-strict?
  :guards ;; (seq-of statement)
  :head ;; rule-head
  :clause ;; the actual clause
  )

(defn inst-guard
  [guard subs ont]
  (let [type-states (as/find-best-arguments
                      search
                      breadth-first
                      nil
                      0
                      (as/state
                        guard
                        :pro
                        (list (list guard))
                        '()
                        *empty-argument-graph*
                        subs
                        '())
                      (list (generate-arguments-from-reasoner (:ontology ont) (:reasoner ont))))]
    (map :substitutions type-states)))

(defn get-subs
  [guard old-subs ont]
  (let [new-subs (map (fn [s]
                        (inst-guard guard s ont))
                   old-subs)]
    (apply concat new-subs)))

(let [counter (atom 0)]
  (letfn [(reset-counter []
            (reset! counter 0))
          (get-guard-number [n]
            (swap! counter inc)
            (symbol (str n "-" @counter)))]
    (defn instantiate-guards
      [nc ont]
;      (println "---------")
;      (println "instantiating for clause:" (:clause nc))
      (let [guards (:guards nc),
            subs (reduce (fn [s g] (get-subs g s ont)) (list identity) guards)]        
;        (println "instantiating" (count guards) "guards")
;        (println (count subs) "instantiations found")
;        (println "---------")
        (map
          (fn [s]
            (struct named-clause
               (get-guard-number (:id nc))
               (:rule nc)
               (:strict nc)
               (:guards nc)
               (map s (:head nc))
               (map s (:clause nc))))
          subs)))))


