;;; Copyright (c) 2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Converter from an annotated ontology to a language."}
  carneades.owl.import-language
  (:require [clojure.string :as s]
            [carneades.engine.utils :refer [safe-read-string]])
  (:import [org.semanticweb.owlapi.model IRI AxiomType]))

(def ontology-iri "http://www.carneades.github.io/ontologies/carneades-annotations")

(defn carneades-annotation?
  "Returns true if the OWL annotation is a Carneades annotation."
  [ann]
  (.startsWith (str (.getProperty ann)) (str "<" ontology-iri)))

(defn ann-property->key
  "Converts an OWL annotation property to a key suitable for a
  language definition."
  [prop]
  (keyword (s/replace (str prop) (re-pattern (str "<" ontology-iri "#(.+)>")) "$1")))

(defn ann-value->value
  "Converts an OWL annotation value to key suitable for a language definition."
  [value]
  (safe-read-string (.getLiteral value)))

(defn owl-class->symbol
  "Converts an OWL class to a Clojure symbol."
  [class]
  (symbol (s/replace (str class) #"[<>]" "")))

(defn equivalent-classes-from-annotated-subject
  "Returns all the equivalent classes for an annotated subject."
  [ontology subject]
  (let [sub (first (.getEntitiesInSignature ontology (IRI/create (str subject))))]
   (into #{(owl-class->symbol sub)} (map owl-class->symbol (.getEquivalentClasses sub ontology)))))

(defn axiom->language-element
  "Converts an OWL Annotation Assertion to a language element."
  [ontology language axiom]
  (if (= (.getAxiomType axiom) AxiomType/ANNOTATION_ASSERTION)
    (let [subject (.getSubject axiom)
          ann (.getAnnotation axiom)
          prop (.getProperty ann)
          value (.getValue ann)]
      (if (carneades-annotation? ann)
        (update-in language [(symbol (str subject))]
                   assoc (ann-property->key prop) (ann-value->value value)
                   :type (equivalent-classes-from-annotated-subject ontology subject))
        language))
    language))

(defn ontology->language
  [ontology]
  (reduce (partial axiom->language-element ontology) {} (.getAxioms ontology)))
