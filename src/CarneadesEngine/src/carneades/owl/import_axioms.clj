;; Copyright (c) 2013 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns carneades.owl.import-axioms
  (:require [carneades.engine.dublin-core :as dc]
            [carneades.engine.statement :refer [literal-complement]]
            [carneades.engine.theory :as t]
            [carneades.project.admin :as project]
            [carneades.owl.owl :as o]
            [carneades.engine.argument :as a]
            [taoensso.timbre :as timbre :refer [debug info warn error]])
  (:import [org.semanticweb.owlapi.model OWLLogicalAxiom AxiomType ClassExpressionType]))

(declare class-expression->sexpr)

;; ---------------------------
;; property expressions
;; ---------------------------

(defn property-expression->sexpr
  [prop-expr argx argy]
  (let [prop (if (.isDataPropertyExpression prop-expr)
               (.asOWLDataProperty prop-expr)
               (.asOWLObjectProperty prop-expr))]
    (list (symbol (.toStringID prop)) argx argy)))

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
  (let [vary '?y
        prop (.getProperty class-expr),
        prop-sexpr (property-expression->sexpr prop argx vary),
        range (.getFiller class-expr),
        range-sexpr (class-expression->sexpr range vary)] ;; TODO : range is not always a class description but could be a data range
    (list 'and prop-sexpr range-sexpr))) ;; TODO : maybe use exist predicate here

(defn has-value->sexpr
  [class-expr argx]
  (let [val (.getValue class-expr), ;; TODO : test
        prop (.getProperty class-expr)]
    (property-expression->sexpr prop argx val)))

(defn intersection->sexpr
  [class-expr argx]
  (let [classes (.getOperands class-expr)]
    (cons 'and (map (fn [c] (class-expression->sexpr c argx)) classes))))

(defn union->sexpr
  [class-expr argx]
  (let [classes (.getOperands class-expr)]
    (cons 'or (map (fn [c] (class-expression->sexpr c argx)) classes))))

(defn complement->sexpr
  [class-expr argx]
  (let [c (.getOperand class-expr)]
    (list 'not (class-expression->sexpr c argx))))

(defn class-expression->sexpr
  [class-expr argx]
  (condp = (.getClassExpressionType class-expr)
    ClassExpressionType/OWL_CLASS (classID->sexpr class-expr argx),
    ClassExpressionType/OBJECT_SOME_VALUES_FROM (some-values->sexpr class-expr argx),
    ClassExpressionType/OBJECT_HAS_VALUE (has-value->sexpr class-expr argx),
    ClassExpressionType/DATA_HAS_VALUE (has-value->sexpr class-expr argx),
    ClassExpressionType/OBJECT_INTERSETION_OF (intersection->sexpr class-expr argx),
    ClassExpressionType/OBJECT_UNION_OF (union->sexpr class-expr argx),
    ClassExpressionType/OBJECT_COMPLEMENT_OF (complement->sexpr class-expr argx),
    ;; else
    (do
      (println "unsupported class-expression type : " (.getClassExpressionType class-expr))
      (list 'foo argx))))

;;;; ---------------------------
;;;; class axioms
;;;; ---------------------------

(defn subclass->schemes
  [axiom]
  (let [varx '?x,
        subclass (.getSubClass axiom),
        subexpr (class-expression->sexpr subclass varx),
        superclass (.getSuperClass axiom),
        superexpr (class-expression->sexpr superclass varx)]
    (list (t/make-scheme
           :header (dc/make-metadata)
           :id (gensym "subclass-axiom")
           :conclusion superexpr
           :premises [(a/pm subexpr)])))) ;; TODO : maybe do optional contrapositioning


(defn equivalent-class->schemes
  [axiom]
  (let [varx '?x,
        classes (.getClassExpressions axiom),
        cl1 (first classes),
        cl-sexpr1 (class-expression->sexpr cl1 varx),
        cl2 (second classes),
        cl-sexpr2 (class-expression->sexpr cl2 varx),
        scheme-< (t/make-scheme
                  :header (dc/make-metadata)
                  :id (gensym "equivalent-classes-axiom")
                  :conclusion cl-sexpr1
                  :premises [(a/pm cl-sexpr2)])
        scheme-> (t/make-scheme
                  :header (dc/make-metadata)
                  :id (gensym "equivalent-classes-axiom")
                  :conclusion cl-sexpr2
                  :premises [(a/pm cl-sexpr1)])]
    (list scheme-< scheme->))) ;; TODO : do optional and some safety checks



(defn disjoint->schemes
  [axiom]
  (let [varx '?x,
        classes (.getClassExpressions axiom),
        cl1 (first classes),
        cl-sexpr1 (class-expression->sexpr cl1 varx),
        cl2 (second classes),
        cl-sexpr2 (class-expression->sexpr cl2 varx),
        scheme-< (t/make-scheme
                  :header (dc/make-metadata)
                  :id (gensym "disjoint-classes-axiom")
                  :conclusion (literal-complement cl-sexpr1)
                  :premises [(a/pm cl-sexpr2)]),
        scheme-> (t/make-scheme
                  :header (dc/make-metadata)
                  :id (gensym "disjoint-classes-axiom")
                  :conclusion (literal-complement cl-sexpr2)
                  :premises [(a/pm cl-sexpr1)])]
    (list scheme-< scheme->))) ;; TODO : do optional and some safety checks


;; ---------------------------
;; property axioms
;; ---------------------------

(defn sub-property->schemes
  [axiom]
  (let [varx '?x,
        vary '?y,
        subprop (.getSubProperty axiom),
        subprop-sexpr (property-expression->sexpr subprop varx vary),
        superprop (.getSuperProperty axiom),
        superprop-sexpr (property-expression->sexpr superprop varx vary)]
    (list (t/make-scheme
           :header (dc/make-metadata)
           :id (gensym "subproperty-axiom")
           :conclusion superprop-sexpr
           :premises [(a/pm subprop-sexpr)])))) ;; TODO : maybe do optional contrapositioning


(defn domain->schemes
  [axiom]
  (let [varx '?x,
        vary '?y,
        domain (.getDomain axiom),
        domain-sexpr (class-expression->sexpr domain varx),
        prop (.getProperty axiom),
        prop-sexpr (property-expression->sexpr prop varx vary)]
    (list (t/make-scheme
           :header (dc/make-metadata)
           :id (gensym "domain-axiom")
           :conclusion domain-sexpr ;; TODO : check if head is valid
           :premises [(a/pm prop-sexpr)])))) ;; TODO : maybe do optional contrapositioning

(defn object-property-range->schemes
  [axiom]
  (let [varx '?x,
        vary '?y,
        range (.getRange axiom),
        range-sexpr (class-expression->sexpr range vary),
        prop (.getProperty axiom),
        prop-sexpr (property-expression->sexpr prop varx vary)]
    ;; (spy range-sexpr)
    ;; (spy prop-sexpr)
    (list (t/make-scheme
           :header (dc/make-metadata)
           :id (gensym "range-axiom")
           :conclusion range-sexpr ;; TODO : check if head is valid
           :premises [(a/pm prop-sexpr)])))) ;; TODO : maybe do optional contrapositioning

(defn data-property-range->schemes
  [axiom]
  (let [varx '?x,
        vary '?y,
        range (.getRange axiom),
        range-sexpr (datatype-expression->sexpr range vary),
        prop (.getProperty axiom),
        prop-sexpr (property-expression->sexpr prop varx vary)]
    (list (t/make-scheme
           :header (dc/make-metadata)
           :id (gensym "range-axiom")
           :conclusion range-sexpr ;; TODO : check if head is valid
           :premises [(a/pm prop-sexpr)])))) ;; TODO : maybe do optional contrapositioning

(defn equivalent-property->schemes
  [axiom]
  (let [varx '?x,
        vary '?y,
        props (.getProperties axiom),
        prop1 (first props),
        prop-sexpr1 (property-expression->sexpr prop1 varx vary),
        prop2 (second props),
        prop-sexpr2 (property-expression->sexpr prop1 varx vary),
        scheme-< (t/make-scheme
                  :header (dc/make-metadata)
                  :id (gensym "equivalent-properties-axiom")
                  :conclusion prop-sexpr1
                  :premises [(a/pm prop-sexpr2)]),
        scheme-> (t/make-scheme
                  :header (dc/make-metadata)
                  :id (gensym "equivalent-properties-axiom")
                  :conclusion prop-sexpr2
                  :premises [(a/pm prop-sexpr1)])]
    (list scheme-< scheme->)))

(defn inverse-property->schemes
  [axiom]
  (let [varx '?x,
        vary '?y,
        prop1 (.getFirstProperty axiom),
        prop-sexpr1 (property-expression->sexpr prop1 varx vary),
        prop2 (.getSecondProperty axiom),
        prop-sexpr2 (property-expression->sexpr prop2 vary varx),
        scheme-< (t/make-scheme
                  :header (dc/make-metadata)
                  :id (gensym "inverse-properties-axiom")
                  :conclusion prop-sexpr1
                  :premises [(a/pm prop-sexpr2)]),
        scheme-> (t/make-scheme
                  :header (dc/make-metadata)
                  :id (gensym "inverse-properties-axiom")
                  :conclusion prop-sexpr2
                  :premises [(a/pm prop-sexpr1)])]
    (list scheme-< scheme->))) ;; TODO : check optionals

(defn transitive-property->schemes
  [axiom]
  (let [varx '?x,
        vary '?y,
        varz '?z,
        prop (.getProperty axiom),
        prop-sexpr-head (property-expression->sexpr prop varx varz),
        prop-sexpr-body1 (property-expression->sexpr prop varx vary),
        prop-sexpr-body2 (property-expression->sexpr prop vary varz)]
    (list (t/make-scheme
           :header (dc/make-metadata)
           :id (gensym "transitive-property-axiom")
           :conclusion prop-sexpr-head
           :premises [(a/pm (list 'and prop-sexpr-body1 prop-sexpr-body2))])))) ;; TODO : check optionals

(defn symmetric-property->schemes
  [axiom]
  (let [varx '?x,
        vary '?y,
        prop (.getProperty axiom),
        prop-sexpr1 (property-expression->sexpr prop varx vary),
        prop-sexpr2 (property-expression->sexpr prop vary varx)]
    (list (t/make-scheme
           :header (dc/make-metadata)
           :id (gensym "symmetric-property-axiom")
           :conclusion prop-sexpr1
           :premises [(a/pm prop-sexpr2)])))) ;; TODO : check optionals


;;;; ---------------------------
;;;; individual axioms
;;;; ---------------------------

(defn class-assertion->schemes
  [axiom]
  (let [individual (symbol (.. axiom getIndividual toStringID)),
        class-expr (.getClassExpression axiom),
        class-sexpr (class-expression->sexpr class-expr individual)]
    (list (t/make-scheme
           :header (dc/make-metadata)
           :id (gensym "class-assertion-axiom")
           :conclusion class-sexpr))))

(defn prop-assertion->schemes
  [axiom]
  (let [subject (symbol (..  axiom getSubject toStringID)),
        object (.getObject axiom), ;; TODO : test
        prop (.getProperty axiom),
        prop-sexpr (property-expression->sexpr prop subject object)]
    (list (t/make-scheme
           :header (dc/make-metadata)
           :id (gensym "property-assertion-axiom")
           :conclusion prop-sexpr))))


(defn axiom->schemes
  [axiom]
  (condp = (.getAxiomType axiom)
    ;; class axioms
    AxiomType/SUBCLASS_OF (subclass->schemes axiom),
    AxiomType/EQUIVALENT_CLASSES (equivalent-class->schemes axiom),
    AxiomType/DISJOINT_CLASSES (disjoint->schemes axiom),
    ;; property axioms
    AxiomType/SUB_OBJECT_PROPERTY (sub-property->schemes axiom),
    AxiomType/SUB_DATA_PROPERTY (sub-property->schemes axiom),
    AxiomType/OBJECT_PROPERTY_DOMAIN (domain->schemes axiom),
    AxiomType/DATA_PROPERTY_DOMAIN (domain->schemes axiom),
    AxiomType/OBJECT_PROPERTY_RANGE (object-property-range->schemes axiom),
    AxiomType/DATA_PROPERTY_RANGE (data-property-range->schemes axiom),
    AxiomType/EQUIVALENT_OBJECT_PROPERTIES (equivalent-property->schemes axiom),
    AxiomType/EQUIVALENT_DATA_PROPERTIES (equivalent-property->schemes axiom),
    AxiomType/INVERSE_OBJECT_PROPERTIES (inverse-property->schemes axiom),
    AxiomType/TRANSITIVE_OBJECT_PROPERTY (transitive-property->schemes axiom),
    AxiomType/SYMMETRIC_OBJECT_PROPERTY (symmetric-property->schemes axiom),
    ;; individual axioms
    AxiomType/CLASS_ASSERTION (class-assertion->schemes axiom),
    AxiomType/OBJECT_PROPERTY_ASSERTION (prop-assertion->schemes axiom),
    AxiomType/DATA_PROPERTY_ASSERTION (prop-assertion->schemes axiom)

    (do
      (info "unsupported axiom type            : " (.getAxiomType axiom))
      ())))

(defn ontology->schemes
  [ontology]
  (let [schemes (flatten (map axiom->schemes (.getLogicalAxioms ontology)))]
    schemes))
