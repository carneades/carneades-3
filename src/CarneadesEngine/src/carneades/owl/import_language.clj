;;; Copyright (c) 2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Converter from an annotated ontology to a language."}
  carneades.owl.import-language
  (:require [clojure.string :as s]
            [carneades.engine.utils :refer [safe-read-string]]
            [carneades.engine.theory :as t])
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

(defn add-annotated-individuals
  "Adds the annotated individuals contained in the ontology to the language."
  [ontology language]
  (reduce (partial add-individual ontology)
          language
          (.getIndividualsInSignature ontology)))

(defn property-ranges->type
  "Converts the ranges of a property to a language type"
  [property ontology]
  (let [ranges (.getRanges property ontology)]
    (condp = (count ranges)
      0 (throw (ex-info (str "Type is not defined for property " property)
                        {:property property}))
      1 (owl-entity->symbol (first ranges))
      (map owl-entity->symbol ranges))))

(defn add-annotated-property
  "Adds an annotated OWL property to the language.
OWL properties are mapped to roles."
  [ontology language property]
  (let [anns (.getAnnotations property ontology)]
    (if (some carneades-annotation? anns)
      (let [role (apply t/make-role
                        :type (property-ranges->type property ontology)
                        :symbol (owl-entity->symbol property)
                        (anns->language-elements anns))]
        (assoc language (:symbol role) role))
      language)))

(defn add-annotated-x-properties
  "Adds OWL data or object properties to the language, depending of
  the value of the f function."
  [ontology language f]
  (reduce (partial add-annotated-property ontology)
          language
          (f ontology)))

(defn add-annotated-data-properties
  "Adds OWL data properties to the language as roles."
  [ontology language]
  (add-annotated-x-properties ontology
                              language
                              (memfn getDataPropertiesInSignature)))

(defn add-annotated-object-properties
  "Adds OWL object properties to the language as roles."
  [ontology language]
  (add-annotated-x-properties ontology
                              language
                              (memfn getObjectPropertiesInSignature)))

(defn add-annotated-properties
  "Adds OWL properties to the language as roles."
  [ontology language]
  (->> language
       (add-annotated-data-properties ontology ,,,)
       (add-annotated-object-properties ontology ,,,)))

(defn add-annotated-class
  "Adds an OWL class to the language as a concept."
  [ontology language class]
  (let [anns (.getAnnotations class ontology)]
    (if (some carneades-annotation? anns)
      (let [concept (apply t/make-concept :symbol (owl-entity->symbol class)
                           (anns->language-elements anns))]
        (assoc language (:symbol concept) concept))
      language)))

(defn add-annotated-classes
  "Adds OWL classes to the language as concepts."
  [ontology language]
  (reduce (partial add-annotated-class ontology)
          language
          (.getClassesInSignature ontology)))

(defn ontology->language
  "Converts the annotated individuals and properties into a language."
  [ontology]
  (->> {}
       (add-annotated-individuals ontology ,,,)
       (add-annotated-properties ontology ,,,)
       (add-annotated-classes ontology ,,,)))
