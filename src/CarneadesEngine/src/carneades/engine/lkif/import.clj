;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.engine.lkif.import
  (:require
    [clojure.zip :as zip]
    [clojure.xml :as xml]
    [clojure.contrib.zip-filter :as filter])
  (:use
    clojure.xml
    clojure.contrib.pprint
    clojure.contrib.def
    clojure.contrib.zip-filter.xml
;    carneades.engine.argument-builtins ; for testing
;    carneades.engine.shell    ; for testing
;    carneades.ui.diagram.viewer ; for testing
;    carneades.engine.argument-builtins ; for testing
;    carneades.engine.utils
    carneades.engine.statement
    carneades.engine.argument
    carneades.engine.rule
    ;carneades.engine.owl.rule
    carneades.engine.owl)
  (:require [clojure.string :as str])
  (:import
    (java.net MalformedURLException URL)
    (java.io File))
  )

(declare lkif-wff->sexpr lkif-atom->sexpr lkif-term->sexpr lkif-import*)

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
      '()
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
  (symbol (attr lkif-individual :value)))

(defn lkif-constant->sexpr
  [lkif-constant]
  (let [c (text lkif-constant),
        len (count c)
        n (try (Integer/parseInt c) (catch NumberFormatException e false)),
        v (cond
           (and (.startsWith c "\"") (.endsWith c "\"")) (subs c 1 (dec len))
           n n,
           (= c "true") true,
           (= c "false") false,
           :else (symbol c))]
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
    "%s"
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
           ;form (parse-term_text* term_text*), ; fatoms not used any more
           ;fa (struct fatom form sexpr)
           fa sexpr
           ]
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
  ;(println "lkif-exists->sexpr:" lkif-exists)
  (let [assumable (attr lkif-exists :assumable),
        v1 (lkif-term->sexpr (xml1-> lkif-exists :v)),
        v2 (symbol (.substring (str v1) 1)),
        wffs (rest (filter/children-auto lkif-exists)),
        t (lkif-wff->sexpr (first wffs)),
        t2 (replace-var v1 v2 t),
        p (lkif-wff->sexpr (second wffs)),
        p2 (replace-var v1 v2 p),
        e (list 'exists v2 t2 p2)]
    ;(println "exists imported:" e)
    (if (and assumable (= assumable "true"))
      (list 'assuming e)
      e)))

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

(defn import-class
  [lkif-class]
  ;(println "importing class" lkif-class)
  (let [pred (symbol (attr lkif-class :pred)),
        v (lkif-term->sexpr (xml1-> lkif-class :v))]
    (list pred v)))

(defn import-property
  [lkif-property]
  ;(print "importing property" lkif-property)
  (let [pred (symbol (attr lkif-property :pred)),
        term_text* (filter/children-auto lkif-property),
        term* (filter (fn [xml] (zip/branch? xml)) term_text*),
        sexpr (cons pred (map lkif-term->sexpr term*))]
    ;(println " - " sexpr)
    sexpr))


(defn import-domain
  [lkif-domain]
  ;(print "importing domain" lkif-domain)
  (condp = (:tag (first lkif-domain))
    :class (import-class lkif-domain),
    :property (import-property lkif-domain),
    true (println "no domain found" lkif-domain)))

(defn import-domains
  [lkif-domains]
  (if (nil? lkif-domains)
    '()
    (let [domains (map import-domain (filter/children-auto lkif-domains))]
      domains)))

(defn import-rule
  [lkif-rule]  
  (let [id (symbol (attr lkif-rule :id)),
         lkif-strict (attr lkif-rule :strict),
         strict (condp = lkif-strict
                  "true" true,
                  "false" false,
                  false),
         head (xml1-> lkif-rule :head),
         domains (import-domains (xml1-> lkif-rule :domains)),
         lkif-body (xml1-> lkif-rule :body),
         body (if lkif-body
                (map lkif-wff->sexpr (filter/children-auto lkif-body))
                nil)]    
    (make-rule
      id
      strict
      domains
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

(defn import-import
  [i files path]
    (let [url (attr i :url)]
      ;(println "i:" i)
      ;(println "uri:" url)
      (if (some #{url} files)
        {:name url,
         :import-tree nil,
         :import-kbs {},
         :import-ags {}}
        (let [is-url? (try (new URL url) (catch MalformedURLException e false)),
              prepath (if is-url?
                        nil
                        (str (.getAbsolutePath (. (new File path) getParentFile)) "/")),
              url (if (or is-url? (. (new File url) isAbsolute))
                    url
                    (str prepath url))]
          ;(println "uri:" url)
          (cond
            (lkif? url) (let [i (lkif-import* url (cons url files)),
                              rb (:rb i),
                              ags (:ags i),
                              imp-kbs (if rb
                                        (assoc (:import-kbs i) url rb)
                                        (:import-kbs i)),
                              imp-ags (if ags
                                        (assoc (:import-ags i) url ags)
                                        (:import-ags i))
                              ]
                          {:name url,
                           :import-tree (:import-tree i),
                           :import-kbs imp-kbs,
                           :import-ags imp-ags}),
            (owl? url) {:name url,
                        :import-tree nil,
                        :import-kbs (assoc {} url (load-ontology url prepath)),
                        :import-ags {}})))))

(defn import-imports
  [theory filename files]
  (if theory
    (let* [imports (xml1-> theory :imports),
           filename-list (if imports (xml-> imports :import) nil),
           import-list (map (fn [i] (import-import i files filename)) filename-list),
           imp-tree (map (fn [i] (dissoc i :import-kbs :import-ags)) import-list),
           imp-kbs (apply merge (map :import-kbs import-list)),
           imp-ags (apply merge (map :import-ags import-list))
           ]
      ;(println "imported from:" filename)
      ;(println "imported files:" (count filename-list))
      ;(println "imported list:" import-list)
      ;(println "imported tree:" imp-tree)
      {:import-tree imp-tree,
       :import-kbs imp-kbs,
       :import-ags imp-ags}
      )
    {:import-tree nil,
     :import-kbs {},
     :import-ags {}}))

(defn import-theory
  [theory filename files]
  (if theory
    (let* [imp (import-imports theory filename files),
           ;imported-rb (:rb imported-rb_ags),
           ;imported-ags (:ag imported-rb_ags),
           defined-rules (import-rules (xml1-> theory :rules)),
           axioms (axioms->rules (xml1-> theory :axioms)),
           rb (apply rulebase (concat defined-rules axioms))
           ]
      ;(println "theory loaded:" filename)
      ;(println "imported-rb: " (count imported-rb))
      ;(println "imported-ags " (count imported-ags))
      ;(println "defined-rules: " defined-rules)
      ;(println "axioms: " axioms)
      ;(println "rb:" rb)
      (assoc imp :rb rb)
      )
    {}
    ))

(defn lkif-premise->premise
  [lkif-premise stmt-map]
  (let* [polarity (attr lkif-premise :polarity),
         pr_type (attr lkif-premise :type),
         role (attr lkif-premise :role),
         stmt-kw (keyword (attr lkif-premise :statement)),
         stmt* (get stmt-map stmt-kw),
         stmt (if (and (list? stmt*) (= (first stmt*) 'assuming))
                (first (rest stmt*))
                stmt*),
         pr (condp = pr_type
              "exception" exception,
              "assumption" assumption,
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
      title             ; title
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
    (apply hash-map key-val-list)))

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
    nil))

; can throw:
;    - java.lang.IllegalArgumentException
;    - org.xml.sax.SAXException
;    - java.io.IOException
(defn lkif-import*
  "Reads an lkif-file from path and returns a lkif-struct"
  ([filename]
    (lkif-import* filename '()))
  ([filename files]
    (let* [document (zip/xml-zip (xml/parse filename)),
           lkif-sources (xml1-> document :sources),
           lkif-theory (xml1-> document :theory),
           lkif-arg-graphs (xml1-> document :argument-graphs),
           source-list (sources->list lkif-sources),
           theory (import-theory lkif-theory filename (cons filename files)),
           ags (parse-arg-graphs lkif-arg-graphs),
           ;rb (:rb theory),
           ;imp-tree (:import-tree theory),
           ;imp-rbs (:import-rbs theory),
           ;imp-ags (:import-ags theory)
           ]
      ;{:sources source-list, :rb rb, :import-tree imp-tree, :import-rbs imp-rbs, :import-ags imp-ags, :ags ags}
      ;(println "theory:" theory)
      (assoc theory :sources source-list :ags ags)
      )))

;(def path "C:\\Users\\stb\\Documents\\Carneades Project\\carneades\\src\\CarneadesExamples\\src\\carneades\\examples\\open_source_licensing\\oss-rules.xml")

;(def i (lkif-import path))

