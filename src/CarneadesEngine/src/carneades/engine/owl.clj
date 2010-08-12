
(ns carneades.engine.owl
  (:require :reload-all
    [clojure.xml :as xml])
  (:use 
    carneades.engine.statement
    carneades.engine.argument
    carneades.engine.rule)
  (:import
    (org.semanticweb.owlapi.apibinding OWLManager)
    (org.semanticweb.owlapi.model IRI OWLLogicalAxiom AxiomType ClassExpressionType)
    (org.semanticweb.owlapi.vocab OWLRDFVocabulary)
    (org.semanticweb.owlapi.reasoner OWLReasonerFactory)
    (org.semanticweb.owlapi.reasoner OWLReasoner)
    (java.net URI)
    (java.io File)
    )
  )

; TODO : forall

(declare class-expression->sexpr load-ontology)

; ---------------------------
; property expressions
; ---------------------------

(defn property-expression->sexpr
  [prop-expr argx argy]
  (let [prop (if (. prop-expr isDataPropertyExpression)
               (. prop-expr asOWLDataProperty)
               (. prop-expr asOWLObjectProperty))]
    (list (symbol (. prop toStringID)) argx argy)))

; ---------------------------
; class expressions
; ---------------------------

(defn classID->sexpr
  [class-expr argx]
  (list (symbol (.toStringID class-expr)) argx))

(defn some-values->sexpr
  [class-expr argx]
  (let [vary (gensym "?y")
        prop (. class-expr getProperty),
        prop-sexpr (property-expression->sexpr prop argx vary),
        range (. class-expr getFiller),
        range-sexpr (class-expression->sexpr range vary)] ; TODO : range is not always a class description but could be a data range
    (list 'and prop-sexpr range-sexpr))) ; TODO : maybe use exist predicate here

(defn has-value->sexpr
  [class-expr argx]
  (let [val (. class-expr getValue), ; TODO : test
        prop (. class-expr getProperty)]
    (property-expression->sexpr prop argx val)))

(defn intersection->sexpr
  [class-expr argx]
  (let [classes (. class-expr getOperands)]
    (cons 'and (map (fn [c] (class-expression->sexpr c argx)) classes))))

(defn union->sexpr
  [class-expr argx]
  (let [classes (. class-expr getOperands)]
    (cons 'or (map (fn [c] (class-expression->sexpr c argx)) classes))))

(defn complement->sexpr
  [class-expr argx]
  (let [c (. class-expr getOperand)]
    (list 'not (class-expression->sexpr c argx))))

(defn class-expression->sexpr
  [class-expr argx]
  (condp = (. class-expr getClassExpressionType)
    ClassExpressionType/OWL_CLASS (classID->sexpr class-expr argx),
    ClassExpressionType/OBJECT_SOME_VALUES_FROM (some-values->sexpr class-expr argx),
    ClassExpressionType/OBJECT_HAS_VALUE (has-value->sexpr class-expr argx),
    ClassExpressionType/DATA_HAS_VALUE (has-value->sexpr class-expr argx),
    ClassExpressionType/OBJECT_INTERSETION_OF (intersection->sexpr class-expr argx),
    ClassExpressionType/OBJECT_UNION_OF (union->sexpr class-expr argx),
    ClassExpressionType/OBJECT_COMPLEMENT_OF (complement->sexpr class-expr argx),
    ; else
    (do
      (println "unsupported class-expression type : " (. class-expr getClassExpressionType))
      (list 'foo argx))))


; ---------------------------
; class axioms
; ---------------------------

(defn subclass->rules
  [axiom]
  (let [varx '?x,
        subclass (. axiom getSubClass),
        subexpr (class-expression->sexpr subclass varx),
        superclass (. axiom getSuperClass),
        superexpr (class-expression->sexpr superclass varx)]
    (list (make-rule
            (gensym "subclass-axiom")
            false
            (make-rule-head superexpr)
            (make-rule-body subexpr))))) ; TODO : maybe do optional contrapositioning


(defn equivalent-class->rules
  [axiom]
  (let [varx '?x,
        classes (. axiom getClassExpressions),
        cl1 (first classes),
        cl-sexpr1 (class-expression->sexpr cl1 varx),
        cl2 (second classes),
        cl-sexpr2 (class-expression->sexpr cl2 varx),
        rule-< (make-rule
                 (gensym "equivalent-classes-axiom")
                 false
                 (make-rule-head cl-sexpr1)
                 (make-rule-body cl-sexpr2)),
        rule-> (make-rule
                 (gensym "equivalent-classes-axiom")
                 false
                 (make-rule-head cl-sexpr2)
                 (make-rule-body cl-sexpr1))]
    (list rule-< rule->))) ; TODO : do optional and some safety checks



(defn disjoint->rules
  [axiom]
  (let [varx '?x,
        classes (. axiom getClassExpressions),
        cl1 (first classes),
        cl-sexpr1 (class-expression->sexpr cl1 varx),
        cl2 (second classes),
        cl-sexpr2 (class-expression->sexpr cl2 varx),
        rule-< (make-rule
                 (gensym "disjoint-classes-axiom")
                 false
                 (make-rule-head (statement-complement cl-sexpr1))
                 (make-rule-body cl-sexpr2)),
        rule-> (make-rule
                 (gensym "disjoint-classes-axiom")
                 false
                 (make-rule-head (statement-complement cl-sexpr2))
                 (make-rule-body cl-sexpr1))]
    (list rule-< rule->))) ; TODO : do optional and some safety checks


; ---------------------------
; property axioms
; ---------------------------

(defn sub-property->rules
  [axiom]
  (let [varx '?x,
        vary '?y,
        subprop (. axiom getSubProperty),
        subprop-sexpr (property-expression->sexpr subprop varx vary),
        superprop (. axiom getSuperProperty),
        superprop-sexpr (property-expression->sexpr superprop varx vary)]
    (list (make-rule
            (gensym "subproperty-axiom")
            false
            (make-rule-head superprop-sexpr)
            (make-rule-body subprop-sexpr))))) ; TODO : maybe do optional contrapositioning


(defn domain->rules
  [axiom]
  (let [varx '?x,
        vary '?y,
        domain (. axiom getDomain),
        domain-sexpr (class-expression->sexpr domain varx),
        prop (. axiom getProperty),
        prop-sexpr (property-expression->sexpr prop varx vary)]
    (list (make-rule
            (gensym "domain-axiom")
            false
            (make-rule-head domain-sexpr) ; TODO : check if head is valid
            (make-rule-body prop-sexpr))))) ; TODO : maybe do optional contrapositioning

(defn range->rules
  [axiom]
  (let [varx '?x,
        vary '?y,
        domain (. axiom getRange),
        domain-sexpr (class-expression->sexpr domain vary),
        prop (. axiom getProperty),
        prop-sexpr (property-expression->sexpr prop varx vary)]
    (list (make-rule
            (gensym "range-axiom")
            false
            (make-rule-head domain-sexpr) ; TODO : check if head is valid
            (make-rule-body prop-sexpr))))) ; TODO : maybe do optional contrapositioning

(defn equivalent-property->rules
  [axiom]
  (let [varx '?x,
        vary '?y,
        props (. axiom getProperties),
        prop1 (first props),
        prop-sexpr1 (property-expression->sexpr prop1 varx vary),
        prop2 (second props),
        prop-sexpr2 (property-expression->sexpr prop1 varx vary),
        rule-< (make-rule
                 (gensym "equivalent-properties-axiom")
                 false
                 (make-rule-head prop-sexpr1)
                 (make-rule-body prop-sexpr2)),
        rule-> (make-rule
                 (gensym "equivalent-properties-axiom")
                 false
                 (make-rule-head prop-sexpr2)
                 (make-rule-body prop-sexpr1))]
    (list rule-< rule->)))



(defn inverse-property->rules
  [axiom]
  (let [varx '?x,
        vary '?y,
        prop1 (. axiom getFirstProperty),
        prop-sexpr1 (property-expression->sexpr prop1 varx vary),
        prop2 (. axiom getSecondProperty),
        prop-sexpr2 (property-expression->sexpr prop2 vary varx),
        rule-< (make-rule
                 (gensym "inverse-properties-axiom")
                 false
                 (make-rule-head prop-sexpr1)
                 (make-rule-body prop-sexpr2)),
        rule-> (make-rule
                 (gensym "inverse-properties-axiom")
                 false
                 (make-rule-head prop-sexpr2)
                 (make-rule-body prop-sexpr1))]
    (list rule-< rule->))) ; TODO : check optionals

(defn transitive-property->rules
  [axiom]
  (let [varx '?x,
        vary '?y,
        varz '?z,
        prop (. axiom getProperty),
        prop-sexpr-head (property-expression->sexpr prop varx varz),
        prop-sexpr-body1 (property-expression->sexpr prop varx vary),
        prop-sexpr-body2 (property-expression->sexpr prop vary varz)]
    (list (make-rule
            (gensym "transitive-property-axiom")
            false
            (make-rule-head prop-sexpr-head)
            (make-rule-body (list 'and prop-sexpr-body1 prop-sexpr-body2)))))) ; TODO : check optionals

(defn symmetric-property->rules
  [axiom]
  (let [varx '?x,
        vary '?y,
        prop (. axiom getProperty),
        prop-sexpr1 (property-expression->sexpr prop varx vary),
        prop-sexpr2 (property-expression->sexpr prop vary varx)]
    (list (make-rule
            (gensym "symmetric-property-axiom")
            false
            (make-rule-head prop-sexpr1)
            (make-rule-body prop-sexpr2))))) ; TODO : check optionals


; ---------------------------
; individual axioms
; ---------------------------

(defn class-assertion->rules
  [axiom]
  (let [individual (symbol (.. axiom getIndividual toStringID)),
        class-expr (. axiom getClassExpression),
        class-sexpr (class-expression->sexpr class-expr individual)]
    (list (make-rule
            (gensym "class-assertion-axiom")
            false
            (make-rule-head class-sexpr)
            '()))))

(defn prop-assertion->rules
  [axiom]
  (let [subject (symbol (.. axiom getSubject toStringID)),
        object (. axiom getObject), ; TODO : test
        prop (. axiom getProperty),
        prop-sexpr (property-expression->sexpr prop subject object)]
    (list (make-rule
            (gensym "property-assertion-axiom")
            false
            (make-rule-head prop-sexpr)
            '()))))
        


; ---------------------------
; main
; ---------------------------

(defn axiom->rules
  [axiom]
  (condp = (. axiom getAxiomType)
    ; class axioms
    AxiomType/SUBCLASS_OF (subclass->rules axiom),
    AxiomType/EQUIVALENT_CLASSES (equivalent-class->rules axiom),
    AxiomType/DISJOINT_CLASSES (disjoint->rules axiom),
    ; property axioms
    AxiomType/SUB_OBJECT_PROPERTY (sub-property->rules axiom),
    AxiomType/SUB_DATA_PROPERTY (sub-property->rules axiom),
    AxiomType/OBJECT_PROPERTY_DOMAIN (domain->rules axiom),
    AxiomType/DATA_PROPERTY_DOMAIN (domain->rules axiom),
    AxiomType/OBJECT_PROPERTY_RANGE (range->rules axiom),
    AxiomType/DATA_PROPERTY_RANGE (range->rules axiom),
    AxiomType/EQUIVALENT_OBJECT_PROPERTIES (equivalent-property->rules axiom),
    AxiomType/EQUIVALENT_DATA_PROPERTIES (equivalent-property->rules axiom),
    AxiomType/INVERSE_OBJECT_PROPERTIES (inverse-property->rules axiom),
    AxiomType/TRANSITIVE_OBJECT_PROPERTY (transitive-property->rules axiom),
    AxiomType/SYMMETRIC_OBJECT_PROPERTY (symmetric-property->rules axiom),
    ; individual axioms
    AxiomType/CLASS_ASSERTION (class-assertion->rules axiom),
    AxiomType/OBJECT_PROPERTY_ASSERTION (prop-assertion->rules axiom),
    AxiomType/DATA_PROPERTY_ASSERTION (prop-assertion->rules axiom)
    ; else
    (do
      (println "unsupported axiom type            : " (. axiom getAxiomType))
      '())))

(defn owl?
  [url]
  (= (:tag (xml/parse url)) :rdf:RDF))

(defn path->uri
  [path]
  (let [file (new File path)]
    (if (. file exists)
      (. file toURI)
      (new URI path))))

(defn imports->rules
  [ont opts]
  (let [imports (. ont getDirectImports),
        rulebases (map (fn [o] (load-ontology o opts)) imports)]
    (last (reductions (fn [rb1 rb2] (add-rules rb1 (:rules rb2))) *empty-rulebase* rulebases))))


(defn load-ontology
  "loads an owl ontology from path and translates it to rules"
  ([path] (load-ontology path '()))
  ([path optionals]
   (let [manager (OWLManager/createOWLOntologyManager),
         se (. manager setSilentMissingImportsHandling true), ; TODO : handle imports
         documentIRI (IRI/create (path->uri path)),
         ontology (. manager loadOntology documentIRI), ; TODO imports don't work
         axioms (. ontology getLogicalAxioms),
         defined-rules (apply concat (map axiom->rules axioms))
         imported-rules (imports->rules ontology optionals),
         rules (concat imported-rules defined-rules)]
     (println "# of all axioms     : " (. ontology getAxiomCount))
     (println "# of logical axioms : " (. ontology getLogicalAxiomCount))
     (println "# of rules imported : " (count rules))
     (add-rules *empty-rulebase* rules))))


;(def p1 "C:\\Users\\stb\\Documents\\Carneades Project\\carneades\\examples\\open-source-licensing\\oss-licenses.owl")
;(def p2 "C:\\Users\\stb\\Documents\\Carneades Project\\carneades\\examples\\open-source-licensing\\impact-licensing.owl")
(def p3 "http://www.co-ode.org/ontologies/pizza/pizza.owl")