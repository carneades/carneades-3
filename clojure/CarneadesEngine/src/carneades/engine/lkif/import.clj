
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
    carneades.ui.diagram.viewer ; for testing
    carneades.engine.argument-builtins ; for testing
    carneades.engine.statement
    carneades.engine.argument
    carneades.engine.rule
    carneades.engine.owl)
  ;(:import )
  )

(declare lkif-wff->sexpr lkif-atom->sexpr lkif-term->sexpr lkif-import)

(defn lkif?
  [url]
  (= (:tag (xml/parse url)) :lkif))

(defn source->list
  [source-element]
  (let [element (attr source-element :element),
        uri (attr source-element :uri)
        ]
    (list element uri)
    ))

(defn sources->list
  [lkif-sources]
  (if lkif-sources
    (let* [source-elements (xml-> lkif-sources :source)]
      (map source->list source-elements))
    nil))


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
  (symbol (attr lkif-individual) :value))

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
  (let [functor (attr lkif-expression :functor),
        term* (filter/children-auto lkif-expression)]
    (cons (symbol functor) (map lkif-term->sexpr term*))))

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

(defn parse-term_text
  [term_text]
  (if (zip/branch? term_text)
    "~a"
    (first term_text)))

(defn parse-term_text*
  [term_text*]
  ;(pprint term_text*)
  (apply str (map parse-term_text term_text*)))


(defn lkif-atom->sexpr
  [lkif-atom]
  (do ;(println "lkif-atom->sexpr" lkif-atom)
    (let* [pred (attr lkif-atom :pred),
           assumable? (attr lkif-atom :assumable),
           term_text* (filter/children-auto lkif-atom),
           term* (filter (fn [xml] (zip/branch? xml)) term_text*),
           sexpr (if pred
                   (cons (symbol pred) (map lkif-term->sexpr term*))
                   (text lkif-atom)),
           form (parse-term_text* term_text*),
           fa (struct fatom form sexpr)]
      ;(pprint fa)
      fa)))

(defn lkif-and->sexpr
  [lkif-and]
  (let* [assumable (attr lkif-and :assumable),
         wff* (filter/children-auto lkif-and),
         a (cons 'and (map lkif-wff->sexpr wff*))]
    (if (and assumable (= assumable "true"))
      (list 'assuming a)
      a)))


(defn lkif-or->sexpr
  [lkif-or]
  (let* [assumable (attr lkif-or :assumable),
         wff* (filter/children-auto lkif-or),
         o (cons 'or (map lkif-wff->sexpr wff*))]
    (if (and assumable (= assumable "true"))
      (list 'assuming o)
      o)))

(defn lkif-not->sexpr
  [lkif-not]
  (let [exception (attr lkif-not :exception),
        assumable (attr lkif-not :assumable),
        wff (first (filter/children-auto lkif-not))]
    (if (and exception (= exception "true"))
      (list 'unless (lkif-wff->sexpr wff))
      (if (and assumable (= assumable "true"))
        (list 'assuming (list 'not (lkif-wff->sexpr wff)))
        (list 'not (lkif-wff->sexpr wff))))))

(defn lkif-if->sexpr
  [lkif-if]
  (let* [assumable (attr lkif-if :assumable),
         wff* (filter/children-auto lkif-if),
         i (cons 'if (map lkif-wff->sexpr wff*))]
    (if (and assumable (= assumable "true"))
      (list 'assuming i)
      i)))

(defn lkif-iff->sexpr
  [lkif-iff]
  (let* [assumable (attr lkif-iff :assumable),
         wff* (filter/children-auto lkif-iff),
         i (cons 'iff (map lkif-wff->sexpr wff*))]
    (if (and assumable (= assumable "true"))
      (list 'assuming i)
      i)))

(defn lkif-all->sexpr
  [lkif-all]
  (throw (Exception. "\"All\" is not supported in Carneades")))

(defn lkif-exists->sexpr
  [lkif-exists]
  (throw (Exception. "\"Exists\" is not supported in Carneades")))

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
                  false),
         head (xml1-> lkif-rule :head),
         lkif-body (xml1-> lkif-rule :body),
         body (if lkif-body
                (map lkif-wff->sexpr (filter/children-auto lkif-body))
                nil)]    
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
    (map import-rule (xml-> lkif-rules :rule))
    nil))

(defn import-rb_ag
  [i optionals files]  
  (if i
    (let [url (attr i :url)]      
      (if (some #{url} files)
        {:rb *empty-rulebase*, :ag nil}
        (cond
          (lkif? url) (let [i (lkif-import url optionals (cons url files)),
                            rb (:rb i),
                            ags (:ags i)]                        
                        {:rb rb, :ag ags}),
          (owl? url) {:rb (load-ontology url optionals),
                      :ag nil} )))
    {:rb *empty-rulebase*, :ag nil}))

(defn import-rbs_ags
  [theory optionals files]
  (if theory
    (let* [imports (xml1-> theory :imports),
           import-list (if imports (xml-> imports :import) nil),
           rb_ag-list (map (fn [i] (import-rb_ag i optionals files)) import-list),
           result (reduce (fn [s rb_ag]
                            ;(println "reducing:" rb_ag "\n   " s)
                            {:rb (add-rules (:rb s) (:rules (:rb rb_ag))),
                             :ag (lazy-cat (:ag s) (:ag rb_ag))})
                    {:rb *empty-rulebase*,
                     :ag nil}
                    rb_ag-list)]
      ;(println "imported files:" (count import-list))
      ;(println "imported list:" rb_ag-list)
      ;(println "result:" result)
      result)
    {:rb *empty-rulebase*,
     :ag nil}))

(defn theory->rb_ags
  [theory optionals files]
  (if theory
    (let* [imported-rb_ags (import-rbs_ags theory optionals files),
           imported-rb (:rb imported-rb_ags),
           imported-ags (:ag imported-rb_ags),
           defined-rules (import-rules (xml1-> theory :rules)),
           axioms (axioms->rules (xml1-> theory :axioms)),
           rb (add-rules imported-rb (lazy-cat defined-rules axioms))]
      ;(println "theory loaded")
      ;(println "imported-rb: " (count imported-rb))
      ;(println "imported-ags " (count imported-ags))
      ;(println "defined-rules: " (count defined-rules))
      ;(println "axioms: " axioms)
      {:rb rb :ag imported-ags}
      )
    nil
    ))

(defn lkif-premise->premise
  [lkif-premise stmt-map]
  (let* [polarity (attr lkif-premise :polarity),
         exc (attr lkif-premise :exception),
         role (attr lkif-premise :role),
         stmt-kw (keyword (attr lkif-premise :statement)),
         stmt* (get stmt-map stmt-kw),
         stmt (if (and (list? stmt*) (= (first stmt*) 'assuming))
                (first (rest stmt*))
                stmt*),
         pr (if (and exc (= exc "true"))
              exception
              ordinary-premise)]
    (pr
      stmt              ; atom
      (or               ; polarity
        (not polarity)
        (not= polarity "negative"))
      role              ; role
      )))

(defn lkif-premises->premises
  [lkif-premises stmt-map]
  (map
    (fn [lkif-pr] (lkif-premise->premise lkif-pr stmt-map))
    (xml-> lkif-premises :premise))
  )

(defn lkif-conclusion->sexpr
  [lkif-conclusion stmt-map]
  (get stmt-map (keyword (attr lkif-conclusion :statement)))
  )

(defn parse-argument
  [lkif-arg stmt-map]
  (let [id (attr lkif-arg :id),
        title (attr lkif-arg :title),
        direction (attr lkif-arg :direction),
        scheme (attr lkif-arg :scheme),
        weight (try 
                 (Float/parseFloat (attr lkif-arg :weight))
                 (catch Exception e 0.5)),
        lkif-conclusion (xml1-> lkif-arg :conclusion),
        conclusion (lkif-conclusion->sexpr lkif-conclusion stmt-map),
        lkif-premises (xml1-> lkif-arg :premises),
        premises (lkif-premises->premises lkif-premises stmt-map)]
    ;(pprint weight)
    (argument
      (symbol id)       ; id
      false             ; applicable
      weight            ; weight
      (if direction     ; direction
        (keyword direction)
        :pro)
      conclusion        ; conclusion
      premises          ; premises
      scheme            ; scheme
      )))

(defn parse-arguments
  [lkif-args stmt-map]
  (map
    (fn [lkif-arg] (parse-argument lkif-arg stmt-map))
    (xml-> lkif-args :argument)))

(defn parse-stmt
  [lkif-stmt]
  (let [id (keyword (attr lkif-stmt :id)),
        lkif-atom (xml1-> lkif-stmt :s)]
    (list id (lkif-atom->sexpr lkif-atom))))


(defn parse-statements
  [lkif-stmts]
  (let* [lkif-stmt* (xml-> lkif-stmts :statement),
         key-val-list (apply concat (map parse-stmt lkif-stmt*))]
    (apply assoc (cons (hash-map) key-val-list))))

(defn apply-status_standard
  [ag lkif-stmt*]
  (if (empty? lkif-stmt*)
    ag
    (let* [lkif-stmt (first lkif-stmt*),
           atom (lkif-atom->sexpr (xml1-> lkif-stmt :s)),
           value (attr lkif-stmt :value),
           assumption (= (attr lkif-stmt :assumption) "true"),
           lkif-standard (attr lkif-stmt :standard)
           standard (if lkif-standard
                      (keyword (.toLowerCase lkif-standard))
                      *default-proof-standard*),
           ; applying status
           ag1 (if assumption
                 (state ag (list atom))
                 (condp = value
                   "unknown" (question ag (list atom)),
                   "true" (accept ag (list atom)),
                   "false" (reject ag (list atom)))),
           ; applying standard
           ag2 (assoc-standard ag1 standard (list atom))]
      (apply-status_standard ag2 (rest lkif-stmt*)))))


(defn parse-arg-graph
  [lkif-arg-graph]
  (let* [id (or (attr lkif-arg-graph :id)
              (gensym "g")),
         title (or (attr lkif-arg-graph :title)
                 ""),
         lkif-main-issue (attr lkif-arg-graph :main-issue),
         lkif-stmts (xml1-> lkif-arg-graph :statements),
         lkif-args (xml1-> lkif-arg-graph :arguments),
         stmt-map (parse-statements lkif-stmts),
         main-issue (and lkif-main-issue ((keyword lkif-main-issue) stmt-map)),
         arguments (parse-arguments lkif-args stmt-map),
         ag1 (assert-arguments (argument-graph id title main-issue) arguments),
         ag2 (apply-status_standard ag1 (xml-> lkif-stmts :statement))]
    ag2))

(defn parse-arg-graphs
  [lkif-arg-graphs]
  (if lkif-arg-graphs
    (map parse-arg-graph (xml-> lkif-arg-graphs :argument-graph))
    (argument-graph)))

(defn lkif-import
  "Reads an lkif-file from path and returns a lkif-struct"
  ([filename] (lkif-import filename '()))
  ([filename optionals]
    (lkif-import filename optionals (list filename)))
  ([filename optionals files]
    (let* [document (zip/xml-zip (xml/parse filename)),
           lkif-sources (xml1-> document :sources),
           lkif-theory (xml1-> document :theory),
           lkif-arg-graphs (xml1-> document :argument-graphs),
           source-list (sources->list lkif-sources),
           theory-rb_ag (theory->rb_ags lkif-theory optionals (cons filename files)),
           ags (parse-arg-graphs lkif-arg-graphs),
           rb (:rb theory-rb_ag),
           ag (:ag theory-rb_ag)
           ]
      ;(println "lkif-sources: " lkif-sources)
      ;(println "source-list: " source-list)
      ;(println "lkif-theory: " lkif-theory)
      ;(println "lkif-arg-graphs: " lkif-arg-graphs)
      ;(println "theory-rb-ag: " theory-rb_ag)
      {:sources source-list :rb rb :ags (concat ags ag)}
      )))

;(def test-path "C:\\Users\\stb\\Documents\\Carneades Project\\carneades\\examples\\Import Test\\test1.xml")
;(def i (lkif-import test-path))
;
;(def e1 (make-engine 50 1 (list (generate-arguments-from-rules (:rb i) '())
;                                                   builtins)))
;
;(def goal (list (symbol "http://www.carneades/owl1.owl#a") '?x))
;
;(def sols (e1 goal))
;(def lkif-args (xml1-> (zip/xml-zip (xml/parse test-path)) :argument-graphs))
;(def args (xml-> lkif-args :argument-graph))
;(def arg1 (first args))
;(def lkif-stmts (xml1-> arg1 :statements))
;(def lkif-stmt* (xml-> lkif-stmts :statement))
;(def lkif-stmt1 (first lkif-stmt*))
;
;(def lkif-args (xml1-> arg1 :arguments))
;(def stmt-map (parse-statements lkif-stmts))
;
;(def pr1 (first (rest (rest (:premises (first (parse-arguments lkif-args stmt-map)))))))

;(def t (xml-> theo :rules :rule))

;(def s (first (filter/children-auto (xml1-> (first t) :body))))

;(def i (lkif-import test-path))

;(defn engine [max-nodes max-turns critical-questions]
;  (make-engine* max-nodes max-turns (argument-graph)
;                (list (generate-arguments-from-rules i critical-questions)
;                      builtins)))

;(def path1 "src/carneades/examples/open_source_licensing/impact-kb.xml")
;(def path2 "src/carneades/examples/open_source_licensing/oss-rules.xml")
;(def path3 "src/carneades/examples/open_source_licensing/impact-licensing.owl")
;
;(def i (lkif-import path1))
