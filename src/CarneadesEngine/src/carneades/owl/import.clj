;;; Copyright (c) 2010-2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.owl.import
  (:require [carneades.engine.statement :refer [literal-complement]]
            [carneades.engine.theory :as t])
  (:import [org.semanticweb.owlapi.model OWLLogicalAxiom AxiomType ClassExpressionType]))

(declare class-expression->sexpr)
;; ---------------------------
;; property expressions
;; ---------------------------

(defn property-expression->sexpr
  [prop-expr argx argy]
  (let [prop (if (. prop-expr isDataPropertyExpression)
               (. prop-expr asOWLDataProperty)
               (. prop-expr asOWLObjectProperty))]
    (list (symbol (. prop toStringID)) argx argy)))

;; ---------------------------
;; datatype expressions
;; ---------------------------

(defn datatype-expression->sexpr
  [datatype-expr argx]
  (list (symbol (.toStringID datatype-expr)) argx))

;; ---------------------------
;; class expressions
;; ---------------------------

(defn classID->sexpr
  [class-expr argx]
  (list (symbol (.toStringID class-expr)) argx))

(defn some-values->sexpr
  [class-expr argx]
  (let [vary (gensym "?y")
        prop (. class-expr getProperty),
        prop-sexpr (property-expression->sexpr prop argx vary),
        range (. class-expr getFiller),
        range-sexpr (class-expression->sexpr range vary)] ;; TODO : range is not always a class description but could be a data range
    (list 'and prop-sexpr range-sexpr))) ;; TODO : maybe use exist predicate here

(defn has-value->sexpr
  [class-expr argx]
  (let [val (. class-expr getValue), ;; TODO : test
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
    ;; else
    (do
      (println "unsupported class-expression type : " (. class-expr getClassExpressionType))
      (list 'foo argx))))

;;;; ---------------------------
;;;; class axioms
;;;; ---------------------------

(defn subclass->rules
  [axiom]
  (let [varx '?x,
        subclass (. axiom getSubClass),
        subexpr (class-expression->sexpr subclass varx),
        superclass (. axiom getSuperClass),
        superexpr (class-expression->sexpr superclass varx)]
    (list (t/make-scheme
           :id (gensym "subclass-axiom")
           :conclusion superexpr
           :premises subexpr)))) ;; TODO : maybe do optional contrapositioning


(defn equivalent-class->rules
  [axiom]
  (let [varx '?x,
        classes (. axiom getClassExpressions),
        cl1 (first classes),
        cl-sexpr1 (class-expression->sexpr cl1 varx),
        cl2 (second classes),
        cl-sexpr2 (class-expression->sexpr cl2 varx),
        rule-< (t/make-scheme
                :id (gensym "equivalent-classes-axiom")
                :conclusion cl-sexpr1
                :premises [cl-sexpr2])
        rule-> (t/make-scheme
                :id (gensym "equivalent-classes-axiom")
                :conclusion cl-sexpr2
                :premises [cl-sexpr1])]
    (list rule-< rule->))) ;; TODO : do optional and some safety checks



(defn disjoint->rules
  [axiom]
  (let [varx '?x,
        classes (. axiom getClassExpressions),
        cl1 (first classes),
        cl-sexpr1 (class-expression->sexpr cl1 varx),
        cl2 (second classes),
        cl-sexpr2 (class-expression->sexpr cl2 varx),
        rule-< (t/make-scheme
                :id (gensym "disjoint-classes-axiom")
                :conclusion (literal-complement cl-sexpr1)
                :premises [cl-sexpr2]),
        rule-> (t/make-scheme
                :id (gensym "disjoint-classes-axiom")
                :conclusion (literal-complement cl-sexpr2)
                :premises [cl-sexpr1])]
    (list rule-< rule->))) ;; TODO : do optional and some safety checks


;; ---------------------------
;; property axioms
;; ---------------------------

(defn sub-property->rules
  [axiom]
  (let [varx '?x,
        vary '?y,
        subprop (. axiom getSubProperty),
        subprop-sexpr (property-expression->sexpr subprop varx vary),
        superprop (. axiom getSuperProperty),
        superprop-sexpr (property-expression->sexpr superprop varx vary)]
    (list (t/make-scheme
           :id (gensym "subproperty-axiom")
           :conclusion superprop-sexpr
           :premises [subprop-sexpr])))) ;; TODO : maybe do optional contrapositioning


(defn domain->rules
  [axiom]
  (let [varx '?x,
        vary '?y,
        domain (. axiom getDomain),
        domain-sexpr (class-expression->sexpr domain varx),
        prop (. axiom getProperty),
        prop-sexpr (property-expression->sexpr prop varx vary)]
    (list (t/make-scheme
           :id (gensym "domain-axiom")
           :conclusion domain-sexpr ;; TODO : check if head is valid
           :premises [prop-sexpr])))) ;; TODO : maybe do optional contrapositioning

(defn object-property-range->rules
  [axiom]
  (let [varx '?x,
        vary '?y,
        range (. axiom getRange),
        range-sexpr (class-expression->sexpr range vary),
        prop (. axiom getProperty),
        prop-sexpr (property-expression->sexpr prop varx vary)]
    (list (t/make-scheme
           :id (gensym "range-axiom")
           :conclusion range-sexpr ;; TODO : check if head is valid
           :premises [prop-sexpr])))) ;; TODO : maybe do optional contrapositioning

(defn data-property-range->rules
  [axiom]
  (let [varx '?x,
        vary '?y,
        range (. axiom getRange),
        range-sexpr (datatype-expression->sexpr range vary),
        prop (. axiom getProperty),
        prop-sexpr (property-expression->sexpr prop varx vary)]
    (list (t/make-scheme
           :id (gensym "range-axiom")
           :conclusion range-sexpr ;; TODO : check if head is valid
           :premises [prop-sexpr])))) ;; TODO : maybe do optional contrapositioning

(defn equivalent-property->rules
  [axiom]
  (let [varx '?x,
        vary '?y,
        props (. axiom getProperties),
        prop1 (first props),
        prop-sexpr1 (property-expression->sexpr prop1 varx vary),
        prop2 (second props),
        prop-sexpr2 (property-expression->sexpr prop1 varx vary),
        rule-< (t/make-scheme
                :id (gensym "equivalent-properties-axiom")
                :conclusion prop-sexpr1
                :premises [prop-sexpr2]),
        rule-> (t/make-scheme
                :id (gensym "equivalent-properties-axiom")
                :conclusion prop-sexpr2
                :premises [prop-sexpr1])]
    (list rule-< rule->)))

(defn inverse-property->rules
  [axiom]
  (let [varx '?x,
        vary '?y,
        prop1 (. axiom getFirstProperty),
        prop-sexpr1 (property-expression->sexpr prop1 varx vary),
        prop2 (. axiom getSecondProperty),
        prop-sexpr2 (property-expression->sexpr prop2 vary varx),
        rule-< (t/make-scheme
                :id (gensym "inverse-properties-axiom")
                :conclusion prop-sexpr1
                :premises [prop-sexpr2]),
        rule-> (t/make-scheme
                :id (gensym "inverse-properties-axiom")
                :conclusion prop-sexpr2
                :premises [prop-sexpr1])]
    (list rule-< rule->))) ;; TODO : check optionals

(defn transitive-property->rules
  [axiom]
  (let [varx '?x,
        vary '?y,
        varz '?z,
        prop (. axiom getProperty),
        prop-sexpr-head (property-expression->sexpr prop varx varz),
        prop-sexpr-body1 (property-expression->sexpr prop varx vary),
        prop-sexpr-body2 (property-expression->sexpr prop vary varz)]
    (list (t/make-scheme
           :id (gensym "transitive-property-axiom")
           :conclusion prop-sexpr-head
           :premises [(list 'and prop-sexpr-body1 prop-sexpr-body2)])))) ;; TODO : check optionals

(defn symmetric-property->rules
  [axiom]
  (let [varx '?x,
        vary '?y,
        prop (. axiom getProperty),
        prop-sexpr1 (property-expression->sexpr prop varx vary),
        prop-sexpr2 (property-expression->sexpr prop vary varx)]
    (list (t/make-scheme
           :id (gensym "symmetric-property-axiom")
           :conclusion prop-sexpr1
           :premises [prop-sexpr2])))) ;; TODO : check optionals


;;;; ---------------------------
;;;; individual axioms
;;;; ---------------------------

(defn class-assertion->rules
  [axiom]
  (let [individual (symbol (.. axiom getIndividual toStringID)),
        class-expr (. axiom getClassExpression),
        class-sexpr (class-expression->sexpr class-expr individual)]
    (list (t/make-scheme
           :id (gensym "class-assertion-axiom")
           :conclusion class-sexpr
           '()))))

(defn prop-assertion->rules
  [axiom]
  (let [subject (symbol (.. axiom getSubject toStringID)),
        object (. axiom getObject), ;; TODO : test
        prop (. axiom getProperty),
        prop-sexpr (property-expression->sexpr prop subject object)]
    (list (t/make-scheme
           :id (gensym "property-assertion-axiom")
           :conclusion prop-sexpr
           '()))))


(defn axiom->schemes
  [axiom]
  (condp = (. axiom getAxiomType)
    ;; class axioms
    AxiomType/SUBCLASS_OF (subclass->rules axiom),
    AxiomType/EQUIVALENT_CLASSES (equivalent-class->rules axiom),
    AxiomType/DISJOINT_CLASSES (disjoint->rules axiom),
    ;; property axioms
    AxiomType/SUB_OBJECT_PROPERTY (sub-property->rules axiom),
    AxiomType/SUB_DATA_PROPERTY (sub-property->rules axiom),
    AxiomType/OBJECT_PROPERTY_DOMAIN (domain->rules axiom),
    AxiomType/DATA_PROPERTY_DOMAIN (domain->rules axiom),
    AxiomType/OBJECT_PROPERTY_RANGE (object-property-range->rules axiom),
    AxiomType/DATA_PROPERTY_RANGE (data-property-range->rules axiom),
    AxiomType/EQUIVALENT_OBJECT_PROPERTIES (equivalent-property->rules axiom),
    AxiomType/EQUIVALENT_DATA_PROPERTIES (equivalent-property->rules axiom),
    AxiomType/INVERSE_OBJECT_PROPERTIES (inverse-property->rules axiom),
    AxiomType/TRANSITIVE_OBJECT_PROPERTY (transitive-property->rules axiom),
    AxiomType/SYMMETRIC_OBJECT_PROPERTY (symmetric-property->rules axiom),
    ;; individual axioms
    AxiomType/CLASS_ASSERTION (class-assertion->rules axiom),
    AxiomType/OBJECT_PROPERTY_ASSERTION (prop-assertion->rules axiom),
    AxiomType/DATA_PROPERTY_ASSERTION (prop-assertion->rules axiom)

    (do
      (println "unsupported axiom type            : " (. axiom getAxiomType))
      '())))

(defn ontology->schemes
  [ontology]
  (let [schemes (map axiom->schemes (.getLogicalAxioms ontology))]
    schemes))
