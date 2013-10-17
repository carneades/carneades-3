;;; Copyright (c) 2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Converter from an annotated ontology to a language."}
  carneades.owl.import-language
  (:require [clojure.string :as s]
            [clojure.pprint :refer :all]
            [carneades.engine.utils :refer [safe-read-string]]
            [carneades.engine.theory :as t]
            [clojure.tools.logging :refer [info debug spy]]
            [carneades.owl.owl :as o])
  (:import [org.semanticweb.owlapi.model IRI AxiomType EntityType]))

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

(defn owl-entity->symbol
  "Converts an OWL entity to a Clojure symbol."
  [entity]
  (symbol (s/replace (str entity) #"[<>]" "")))

(defn equivalent-classes-from-annotated-subject
  "Returns all the equivalent classes for an annotated subject."
  [ontology subject]
  (let [sub (first (.getEntitiesInSignature ontology (IRI/create (str subject))))]
    (into #{(owl-entity->symbol sub)} (map owl-entity->symbol (.getEquivalentClasses sub ontology)))))

(defn add-ann-to-language-element
  "Add the property and value of an annotation to sequence of key value."
  [element ann]
  (if (carneades-annotation? ann)
    (concat element
            [(ann-property->key (.getProperty ann))
             (ann-value->value (.getValue ann))])
    element))

(defn anns->language-elements
  "Converts OWL Annotations to a sequence of key values."
  [annotations]
  (reduce add-ann-to-language-element () annotations))

(defn add-individual
  "Adds an individual to the language, if it is annotated."
  [ontology language individual]
  (let [anns (.getAnnotations individual ontology)]
    (if (some carneades-annotation? anns)
      (let [idv (apply t/make-individual
                       :symbol (owl-entity->symbol individual)
                       (anns->language-elements anns))]
        (assoc language (:symbol idv) idv))
      language)))

(defn property-ranges->type
  "Converts the ranges of a property to a language type"
  [property ontology]
  (let [ranges (o/ranges-in-closure property ontology)]
    (condp = (count ranges)
      0 (throw (ex-info (str "Range is not defined for property " property)
                        {:property property
                         :id (.toStringID property)}))
      1 (owl-entity->symbol (first ranges))
      (map owl-entity->symbol ranges))))

(defn add-class-annotation
  "Adds a class annotation to the language."
  [ontology language annotation entity]
  (let [concept (apply t/make-concept :symbol (owl-entity->symbol entity)
                       (anns->language-elements [annotation]))]
    (assoc language (:symbol concept) concept)))

(defn add-x-property-annotation
  "Adds an object annotation to the language."
  [ontology language annotation entity]
  (let [role (apply t/make-role
                    :type (property-ranges->type entity ontology)
                    :symbol (owl-entity->symbol entity)
                    (anns->language-elements [annotation]))]
   (assoc language (:symbol role) role)))

(defn add-named-individual-annotation
  "Adds an object annotation to the language."
  [ontology language annotation entity]
  (let [individual (apply t/make-individual
                          :symbol (owl-entity->symbol entity)
                          (anns->language-elements [annotation]))]
    (assoc language (:symbol individual) individual)))

(defn add-annotation-assertion
  "Adds an annotation to the language."
  [ontology language annotation]
  (let [subject-iri (.getSubject annotation)
        entities (.getEntitiesInSignature ontology (.getSubject annotation) true)
        entity (first entities)]
    (assert (= 1 (count entities)))
    (condp = (.getEntityType entity)
      EntityType/CLASS
      (add-class-annotation ontology language annotation entity)

      EntityType/DATA_PROPERTY
      (add-x-property-annotation ontology language annotation entity)

      EntityType/OBJECT_PROPERTY
      (add-x-property-annotation ontology language annotation entity)

      EntityType/NAMED_INDIVIDUAL
      (add-named-individual-annotation ontology language annotation entity))))

(defn add-annotation-assertions
  "Adds annotations assertions to the language."
  [ontology language]
  (let [annotations (filter (every-pred #(= (.getAxiomType %) AxiomType/ANNOTATION_ASSERTION)
                                        carneades-annotation?)
                            (.getAxioms ontology))]
    (reduce (partial add-annotation-assertion ontology)
            language
            annotations)))

(defn ontology->language
  "Converts the annotated individuals and properties into a language."
  [ontology]
  (add-annotation-assertions ontology {}))
