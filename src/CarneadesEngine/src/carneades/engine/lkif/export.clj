;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Export LKIF functions"}
    carneades.engine.lkif.export
  (:use carneades.engine.statement
        carneades.engine.argument
        carneades.engine.utils)
  (:require [clojure.contrib.io :as io]
            [clojure.contrib.prxml :as prx]))

(declare text_term->sxml)

(defn combine-expression-format
  [term form]
  ;(println "combine-expression-format" term form)
  (let [s (.split form "%s")
        t (if (= (count (rest term)) (count s))
            (map text_term->sxml (rest term))
            (concat (map text_term->sxml (rest term)) [nil]))]
    (interleave s t)))


(defn functor?
  [f]
  (or
    (= f '+)
    (= f '-)
    (= f '*)
    (= f '/)
    (= f 'list)
    (= f 'cons)))

(defn text_term->sxml
  [t]
  ;(println "converting text/term" t)
  (cond
    (= Boolean (type t)) [:c t],
    (number? t) [:c (str t)],
    (string? t) [:c (str "\"" t "\"")],
    (fatom? t) [:s {:pred (statement-predicate t)} (combine-expression-format (:term t) (:form t))],
    (variable? t) [:v (.substring (str t) 1)],
    (symbol? t) [:c (str t)],
    (and (seq? t) (functor? (first t))) [:expr {:functor (first t)} (map text_term->sxml (rest t))],
    (seq? t) [:s {:pred (first t)} (map text_term->sxml (rest t))],
    :else nil;; (println "no valid text/term" t)
    ))


(defn wff->sxml
  [wff]
  ;(println "wff->sxml" wff)
  ; TODO: exists and all
  (cond
    (string? wff) [:s (str "\"" wff "\"")],
    (variable? wff) [:v (.substring (str wff) 1)],
    (symbol? wff) [:s wff],
    (fatom? wff) [:s {:pred (statement-predicate wff)} (combine-expression-format (:term wff) (:form wff))],
    (seq? wff) (condp = (first wff)
                  'not [:not (wff->sxml (second wff))],
                  'and [:and (map wff->sxml (rest wff))],
                  'or [:or (map wff->sxml (rest wff))],
                  'if [:if (map wff->sxml (rest wff))],
                  'iff [:iff (map wff->sxml (rest wff))],
                  'assuming (let [sxml (wff->sxml (second wff))]
                              (if (map? (second sxml))
                                (concat [(first sxml) (assoc (second sxml) :assumable true)] (nnext sxml))
                                (concat [(first sxml) {:assumable true}] (next sxml)))),
                  'unless [:not {:exception true} (wff->sxml (second wff))],
                  [:s {:pred (first wff)} (map text_term->sxml (rest wff))]),
    :else nil;; (println "no valid wff" wff)
    ))

(defn make-lkif-statement
  [s]
  {:id (gensym "s"),
   :value "unknown",
   :assumption "false",
   :standard "BA",
   :atom s})

(defn nodes->map
  [nodes]
  (reduce (fn [m s] (assoc m s (make-lkif-statement s))) {} (map :statement nodes)))

(defn status->value
  [st]
  ;(println "status->value" st)
  (condp = st
    :stated "unknown",
    :questioned "unknown",
    :accepted "true",
    :rejected "false",
    "unknown"))


(defn status->assumption
  [st]
  ;(println "status->assumption" st)
  (= :stated st))

(defn assumption-premise?
  [s ag]
  (let [premises (apply concat (map :premises (vals (:arguments ag))))]
    (some (fn [pr] (and (assumption? pr) (statement= (premise-statement pr) s))) premises)))

(defn atom->sxml
  [s ag]
  ;(println "atom->sxml" s)
  (cond
    (string? s) (if (assumption-premise? s ag)
                  [:s {:assumable true} s]
                  [:s s]),
    (fatom? s) (if (assumption-premise? s ag)
                 [:s {:pred (statement-predicate s), :assumable true} (combine-expression-format (:term s) (:form s))]
                 [:s {:pred (statement-predicate s)} (combine-expression-format (:term s) (:form s))]),
    (symbol? s) (if (assumption-premise? s ag)
                  [:s {:assumable true} s]
                  [:s s]),
    (seq? s) (if (assumption-premise? s ag)
                [:s {:pred (statement-predicate s), :assumable true} (map text_term->sxml (rest s))]
                [:s {:pred (statement-predicate s)} (map text_term->sxml (rest s))]),
    :else nil;; (println "no valid atom" s)
    ))

(defn statement->sxml
  [s st-map ag]
  ;(println "statement->sxml" s)
  (let [cs (get st-map s),
        st (status ag s)]
    [:statement
     {:id (:id cs), :value (status->value st), :assumption (status->assumption st), :standard (.toUpperCase (.substring (str (proof-standard ag s)) 1))}
     (atom->sxml s ag)]))

(defn statements->sxml
  [ag st-map]
  [:statements (map (fn [s] (statement->sxml s st-map ag)) (keys st-map))])

(defn premise->sxml
  [p st-map]
  (let [polarity (if (premise-pos? p)
                   "positive"
                   "negative"),
        t (cond
            (ordinary-premise? p) "ordinary",
            (assumption? p) "assumption",
            (exception? p) "exception")]
    [:premise
     {:polarity polarity, :type t, :role (:role p), :statement (:id (get st-map (premise-atom p)))}]))

(defn argument->sxml
  [arg st-map]
  [:argument
   {:id (:id arg), :title (:title arg), :direction (.substring (str (:direction arg)) 1), :scheme (:scheme arg), :weight (:weight arg)}
   [:conclusion {:statement (:id (get st-map (:conclusion arg)))}]
   [:premises (map (fn [p] (premise->sxml p st-map)) (:premises arg))]])

(defn arguments->sxml
  [ags st-map]
  [:arguments (map (fn [arg] (argument->sxml arg st-map)) ags)])

(defn arg-graph->sxml
  [ag]
  (let [id (:id ag),
        title (:title ag),
        statement-map (nodes->map (get-nodes ag)),
        main-issue (or (:id (get statement-map (:main-issue ag)))
                     ""),
        statements (statements->sxml ag statement-map),
        arguments (arguments->sxml (vals (:arguments ag)) statement-map)]
    [:argument-graph {:id id, :title title, :main-issue main-issue} statements arguments]))

(defn arg-graphs->sxml
  [args]
  [:argument-graphs (map arg-graph->sxml args)])

(defn clause->sxml
  [c]
  (if (= (count c) 1)
    (wff->sxml (first c))
    [:and (map wff->sxml c)]))

(defn body->sxml
  [body]
  [:body (map clause->sxml body)])

(defn head->sxml
  [head]
  [:head (map wff->sxml head)])

(defn domain->sxml
  [domain]
  (condp = (count domain)
    2 [:class {:pred (first domain)} [:v (.substring (str (second domain)) 1)]],
    3 [:property {:pred (first domain)} (text_term->sxml (second domain)) [:v (.substring (str (nth domain 2)) 1)]],
    ;; (println "no valid domain" domain)
    ))

(defn domains->sxml
  [domains]
  [:domains (map domain->sxml domains)])

(defn rule->sxml
  [rule]
  [:rule
   {:id (:id rule), :strict (or (:strict rule) false)}
   (head->sxml (:head rule))
   (domains->sxml (:domains rule))
   (body->sxml (:body rule))])

(defn rules->sxml
  [rules]
  (and rules
    [:rules (map rule->sxml rules)]))

(defn axiom->sxml
  [axiom]
  (map (fn [wff] [:axiom {:id (gensym "axiom")} (wff->sxml wff)]) (:head axiom)))

(defn axioms->sxml
  [axioms]
  (if (empty? axioms)
    nil
    [:axioms (map axiom->sxml axioms)]))

(defn imports->sxml
  [imp-tree]
  (let [imp-names (map (fn [{:keys [name relative-path]}]
                         (or relative-path name)) imp-tree)]
    [:imports
     (map (fn [n]
           [:import {:url n}])
            imp-names)]))


(defn theory->sxml
  [imp-tree rb]
  (and rb
    (let [id (gensym "theory"),
          imports (imports->sxml imp-tree),
          all-rules (:rules rb),
          axioms (filter (fn [r] (nil? (:body r))) all-rules),
          rules (filter (fn [r] (not (nil? (:body r)))) all-rules)]
      [:theory {:id id} imports (axioms->sxml axioms) (rules->sxml rules)])))

(defn source->sxml
  [source]
  [:source {:element (first source), :uri (second source)}])

(defn sources->sxml
  [sources]
  (and
    sources
    [:sources (map source->sxml sources)]))

(defn data->sxml
  [data]
  (let [sources (sources->sxml (:sources data)),        ,
        theory (theory->sxml (:import-tree data) (:rb data)),
        arg-graphs (arg-graphs->sxml (:ags data))]
    [:lkif sources theory arg-graphs]))


; can throw:
;   - java.io.IOException
(defn lkif-export*
  ([export-data]
    (binding [prx/*prxml-indent* 3]
      (with-out-str (prx/prxml (data->sxml export-data))))
    )
  ([export-data pathname]
    (io/write-lines (absolute pathname) (list (lkif-export* export-data)))
    )
  )
