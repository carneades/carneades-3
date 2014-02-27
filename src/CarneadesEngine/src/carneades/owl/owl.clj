;; Copyright (c) 2010-2013 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(ns ^{:doc "Functions to load an ontology and query it."}
  carneades.owl.owl
  (:require [clojure.java.io :as io])
  (:import org.semanticweb.HermiT.Reasoner
           org.semanticweb.owlapi.apibinding.OWLManager
           org.semanticweb.owlapi.model.IRI
           org.semanticweb.owlapi.model.OWLOntology
           org.semanticweb.owlapi.model.OWLOntologyManager
           org.semanticweb.owlapi.util.AutoIRIMapper))

(defn load-ontology
  "Loads an ontology.

The first argument is an URL or a pathname. The
second optional argument specifies the directory for resolving the
ontology's imports. If not specified, it will be either the current
directory if loading an URL or the directory of the file being
loaded."
  ([url]
     (if (.exists (io/file url))
       (load-ontology url (.getParent (io/file url)))
       (load-ontology url ".")))
  ([url importdir]
     (let [manager (OWLManager/createOWLOntologyManager)
           automapper (AutoIRIMapper. (io/file importdir) true)]
       (.addIRIMapper manager automapper)
       (.loadOntologyFromOntologyDocument manager (io/input-stream url)))))

(defn ranges-in-closure
  "Returns the ranges of the property defined in the current ontology or in its
  imports."
  [property ontology]
  (set (mapcat #(.getRanges property %) (conj (set (.getImports ontology)) ontology))))

;; (defn owl?
;;   [url]
;;   (= (:tag (xml/parse url)) :rdf:RDF))

;; (defn path->uri
;;   [path prepath]
;;   (let [file (File. path)]
;;     (if (.exists file)
;;       (.toURI file)
;;       (if (and prepath (not (url? path)))
;;         (let [fullpath (if (.endsWith prepath java.io.File/separator)
;;                          (str prepath path)
;;                          (str prepath java.io.File/separator path))]
;;          (path->uri fullpath nil))
;;         (URI. path)))))

;; (defn generate-arguments-from-owl
;;   "Generates argument from an OWL ontology"
;;   ([ontology type]
;;     (generate-arguments-from-owl ontology type '()))
;;   ([ontology type optionals]
;;     (and (:ontology ontology)
;;       (condp = type
;;           :reasoner (generate-arguments-from-reasoner (:ontology ontology) (:reasoner ontology)),
;;         :rule (generate-arguments-from-rules (map-ontology (:ontology ontology) optionals)),
;;         (throw (Exception. "Invalid type value for owl generator"))))))

;; (defn individuals
;;   "returns all individuals in the ontology as a list of (class individual)"
;;   [ontology]
;;   (mapcat (fn [class]
;;             (let [individuals (.getIndividuals class ontology)
;;                   sid (symbol (.toStringID class))]
;;               (if (not (empty? individuals))
;;                 (map (fn [individual]
;;                        (list sid (symbol (.toStringID individual))))
;;                      individuals)
;;                 ())))
;;           (.getClassesInSignature ontology)))

;; (defn create-iri [s]
;;   (IRI/create s))

;; (defn classes
;;   "Returns the classes of the ontology"
;;   ([ontology]
;;      (seq (.getClassesInSignature ontology)))
;;   ([ontology iri]
;;      (seq (filter #(instance? OWLClass %)
;;                   (.getEntitiesInSignature ontology iri)))))

;; (defn root-ontology
;;   "Returns the root ontology of the reasoner"
;;   [reasoner]
;;   (.getRootOntology reasoner))

;; (defn instances
;;   "Returns instances of class"
;;   [reasoner class]
;;   (seq (.getFlattened (.getInstances reasoner class false))))

;; (defn individuals [ontology]
;;   (seq (.getIndividualsInSignature ontology)))

;; (defn to-symbol [obj]
;;   (symbol (.toStringID obj)))

;; (defn- x-properties [ontology individual f]
;;   (reduce (fn [acc [k v]]
;;             (assoc acc (to-symbol k) (map #(to-symbol %) v)))
;;           {}
;;           (into {} (f individual ontology))))

;; (defn object-properties
;;   "Returns a hashmap of property -> vals"
;;   ([ontology]
;;      (seq (.getObjectPropertiesInSignature ontology)))
;;   ([ontology individual]
;;      (x-properties ontology individual (memfn getObjectPropertyValues ontology))))

;; (defn data-properties
;;   ([ontology]
;;      (seq (.getDataPropertiesInSignature ontology)))
;;   ([ontology individual]
;;      (x-properties ontology individual (memfn getDataPropertyValues ontology))))

;; (defn- x-properties-seq
;;   "Returns a seq of (property indivi val) from the individual"
;;   [ontology individual f]
;;   (map (fn [[k v]]
;;          (list k (to-symbol individual) v))
;;        (mapcat (fn [[k v]]
;;                  (cartesian-product [k] v))
;;                (f ontology individual))))

;; (defn object-properties-seq
;;   "Returns a seq of (property indiv val) from the individual"
;;   [ontology individual]
;;   (x-properties-seq ontology individual object-properties))

;; (defn data-properties-seq
;;   "Returns a seq of (property indiv val) from the individual"
;;   [ontology individual]
;;   (x-properties-seq ontology individual data-properties))

;; (defn parse-class-expression
;;   "Returns an Expression from the Manchester query. This can then be used to
;;    query the ontology"
;;   [ontology s]
;;   (try
;;     (let [data-factory (.. ontology getOWLOntologyManager getOWLDataFactory)
;;           manager (.getOWLOntologyManager ontology)
;;           import-closures (.getImportsClosure ontology)
;;           short-form-provider (SimpleShortFormProvider.)
;;           bidi (BidirectionalShortFormProviderAdapter. manager import-closures short-form-provider)
;;           parser (ManchesterOWLSyntaxEditorParser. data-factory s)
;;           entity-checker (ShortFormEntityChecker. bidi)]
;;       (.setDefaultOntology parser ontology)
;;       (.setOWLEntityChecker parser entity-checker)
;;       (.parseClassExpression parser))
;;     (catch ParserException e nil)))

;; (defn shorten [sym]
;;   (last (clojure.string/split (str sym) #"#")))

;; (defn instances-with-property
;;   "Returns a list of (property indiv val)"
;;   [reasoner property]
;;   (let [prop (shorten property)
;;         ontology (root-ontology reasoner)]
;;     (if-let [ex (parse-class-expression ontology (format "(%s SOME Thing)" prop))]
;;      (let [individuals (into #{} (.getFlattened (.getInstances reasoner ex true)))]
;;        (mapcat (fn [individual]
;;                  ;; TODO extract the good property with the OWL API
;;                  (let [object-properties (object-properties-seq ontology individual)
;;                        data-properties (data-properties-seq ontology individual)]
;;                    (concat (filter #(= (first %) property) object-properties)
;;                            (filter #(= (first %) property) data-properties))))
;;         individuals))
;;      ())))
