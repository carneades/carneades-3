
(ns carneades.engine.lkif.import
  (:require  :reload-all
    [clojure.zip :as zip]
    [clojure.xml :as xml]
    [clojure.contrib.zip-filter :as filter])
  (:use
    clojure.xml
    clojure.contrib.pprint
    clojure.contrib.def
    clojure.contrib.zip-filter.xml
    carneades.engine.argument-builtins ; for testing
    carneades.engine.shell    ; for testing
    carneades.engine.statement
    carneades.engine.argument
    carneades.engine.rule)
  ;(:import )
  )

(def test-path "C:\\Users\\stb\\Documents\\Carneades Project\\carneades\\examples\\lkif-test\\lkif-test.xml")

(declare lkif-wff->sexpr lkif-atom->sexpr lkif-import)

(defn source->list
  [source-element]
  (let [element (attr source-element :element),
        uri (attr source-element :uri)
        ]
    ;(println "---------")
    ;(print source-element)
    ;(print " ")
    ;(println element)
    ;(println "---------")
    (list element uri)
    ))

(defn sources->list
  [lkif-sources]
  (if lkif-sources
    (let* [source-elements (xml-> lkif-sources :source)]
      (map source->list source-elements))
    nil))

(defn fold-left
  [f start s]
  (if (empty? s)
    start
    (fold-left f (f start (first s)) (rest s))))

(defn axiom->rule
  [a]
  (let [id (attr a :id),
        lkif-wff (first (filter/children-auto a)),
        wff (lkif-wff->sexpr lkif-wff)]
    (make-rule
      id
      true
      (list wff)
      '())))

(defn axioms->rules
  [xml]
  (if xml
    (let [axioms (xml-> xml :axiom)]
      (map axiom->rule axioms))
    xml))

(defn lkif-var->sexpr
  [lkif-var]
  (let [v (text lkif-var)]
    ;(println "lkif-var->sexpr" lkif-var)
    (symbol (str "?" v))))

(defn lkif-individual->sexpr
  [lkif-individual]
  lkif-individual)

(defn lkif-constant->sexpr
  [lkif-constant]
  (let [c (text lkif-constant),
        n (try (Integer/parseInt c) (catch NumberFormatException e false)),
        v (cond
            n n,
            (= c "true") true,
            (= c "false") false,
            true (symbol c))]
    ;(println "lkif-constant->sexpr" lkif-constant c n v)
    v
    ))

(defn lkif-expression->sexpr
  [lkif-expression]
  lkif-expression)

(defn lkif-term->sexpr
  [lkif-term]
  (let [f (condp = (:tag (first lkif-term))
            :v lkif-var->sexpr
            :i lkif-individual->sexpr
            :c lkif-constant->sexpr
            :expr lkif-expression->sexpr
            :s lkif-atom->sexpr
            (println "no term found" lkif-term))]
    ;(println "lkif-term->sexpr" lkif-term)
    (f lkif-term)))

(defn lkif-atom->sexpr
  [lkif-atom]
  (do ;(println "lkif-atom->sexpr" lkif-atom)
    (let* [pred (symbol (attr lkif-atom :pred))
           assumable? (attr lkif-atom :assumable)
           term_text* (filter/children-auto lkif-atom)
           term* (filter (fn [xml] (zip/branch? xml)) term_text*)]
      (cons pred (map lkif-term->sexpr term*)))))

(defn lkif-and->sexpr
  [lkif-and]
  lkif-and)

(defn lkif-or->sexpr
  [lkif-or]
  lkif-or)

(defn lkif-not->sexpr
  [lkif-not]
  lkif-not)

(defn lkif-if->sexpr
  [lkif-if]
  lkif-if)

(defn lkif-iff->sexpr
  [lkif-iff]
  lkif-iff)

(defn lkif-all->sexpr
  [lkif-all]
  lkif-all)

(defn lkif-exists->sexpr
  [lkif-exists]
  lkif-exists)

(defn lkif-wff->sexpr
  [lkif-wff]
  (let [f (condp = (:tag (first lkif-wff))
            :s lkif-atom->sexpr,
            :or lkif-or->sexpr,
            :and lkif-and->sexpr,
            :not lkif-not->sexpr,
            :if lkif-if->sexpr,
            :iff lkif-iff->sexpr,
            :all lkif-all->sexpr,
            :exists lkif-exists->sexpr
            (println "no wff found" lkif-wff))]
    ;(println "lkif-wff->sexpr" lkif-wff)
    (f lkif-wff)))

(defn import-rule
  [lkif-rule]
  (let* [id (symbol (attr lkif-rule :id)),
         lkif-strict (attr lkif-rule :strict),
         strict (condp = lkif-strict
                  "true" true,
                  "false" false,
                  (println "unknown value" lkif-strict)),
         head (xml1-> lkif-rule :head),
         lkif-body (xml1-> lkif-rule :body),
         body (map lkif-wff->sexpr (filter/children-auto lkif-body))]
    ;(println "import-rule" lkif-rule)
    (make-rule
      id
      strict
      (make-rule-head (cons 'and (map lkif-wff->sexpr (filter/children-auto head))))
      (condp = (count body)
        0 '()
        1 (make-rule-body (first body))
        (make-rule-body (cons 'and body))))))

(defn import-rules
  [lkif-rules]
  (if lkif-rules
    (add-rules *empty-rulebase* (map import-rule (xml-> lkif-rules :rule)))
    *empty-rulebase*))

(defn import-rb_ag
  [i optionals files]
  (if i
    (let [url (attr i :url)]
      (if (some #{url} files)
        nil
        (lkif-import url optionals)))
    i))

(defn import-rbs_ags
  [theory optionals files]
  (if theory
    (let* [imports (xml1-> theory :imports),
           import-list (if imports (xml-> imports :import) nil),
           rb_ag-list (map (fn [i] (import-rb_ag i optionals files)) imports)],
      (fold-left (fn [s rb_ag]
                   {:rb (add-rules (:rb s) (:rules (:rb rb_ag))),
                    :ag (lazy-cat (:ag s) (:ag rb_ag))})
        {:rb *empty-rulebase*,
         :ag empty}
        rb_ag-list))
    {:rb *empty-rulebase*,
     :ag empty}))

(defn theory->rb_ags
  [theory optionals files]
  (if theory
    (let* [;imported-rb_ags (import-rbs_ags theory optionals files),
           ;imported-rb (:rb imported-rb_ags),
           ;imported-ags (:ag imported-rb_ags),
           defined-rules (import-rules (xml1-> theory :rules)),
           axioms (axioms->rules (xml1-> theory :axioms))]
      ;(println "imported-rb: " imported-rb)
      ;(println "imported-ags " imported-ags)
      (println "defined-rules: " defined-rules)
      (println "axioms: " axioms)
      (add-rules defined-rules axioms); todo: add other theory parts
      )
    nil
    ))

(defn lkif-import
  "Reads an lkif-file from path and returns a lkif-struct"
  ([filename] (lkif-import filename '()))
  ([filename optionals]
    (let* [document (zip/xml-zip (xml/parse filename)),
           ;lkif-sources (xml1-> document :sources),
           lkif-theory (xml1-> document :theory),
           ;lkif-arg-graphs (xml1-> document :argument-graphs),
           ;source-list (sources->list lkif-sources),
           theory-rb_ag (theory->rb_ags lkif-theory optionals (list filename))
           ]
      ;(println "lkif-sources: " lkif-sources)
      ;(println "source-list: " source-list)
      ;(println "lkif-theory: " lkif-theory)
      ;(println "lkif-arg-graphs: " lkif-arg-graphs)
      ;(println "theory-rb-ag: " theory-rb_ag)
      theory-rb_ag
      )))

;(def theo (xml1-> (zip/xml-zip (xml/parse test-path)) :theory))

;(def t (xml-> theo :rules :rule))

;(def s (first (filter/children-auto (xml1-> (first t) :body))))

(def i (lkif-import test-path))

(defn engine [max-nodes max-turns critical-questions]
  (make-engine* max-nodes max-turns (argument-graph)
                (list (generate-arguments-from-rules i critical-questions)
                      builtins)))