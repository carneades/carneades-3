;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.engine.owl
  (:require [clojure.xml :as xml])
  (:use clojure.contrib.def
        carneades.engine.owl.reasoner
        carneades.engine.owl.rule
        carneades.engine.rule
        clojure.contrib.combinatorics)
  (:import (java.net URI)
           (java.io File)
           (org.semanticweb.owlapi.expression ParserException ShortFormEntityChecker)
           org.coode.owlapi.manchesterowlsyntax.ManchesterOWLSyntaxEditorParser
           (org.semanticweb.owlapi.util ShortFormProvider SimpleShortFormProvider
                                        BidirectionalShortFormProviderAdapter)
           (org.semanticweb.owlapi.apibinding OWLManager)
           (org.semanticweb.owlapi.model IRI MissingImportListener OWLOntologyIRIMapper OWLClass)
           (org.semanticweb.HermiT Reasoner$ReasonerFactory)))

(defn owl?
  [url]
  (= (:tag (xml/parse url)) :rdf:RDF))

(defn- path->uri
  [path prepath]
  (let [file (new File path)]
    (if (. file exists)
      (. file toURI)
      (if prepath
        (path->uri (str prepath path) nil)
        (new URI path)))))

(defn- iri-to-file-mapper [prepath]
  (proxy [OWLOntologyIRIMapper] []
    (getDocumentIRI [ontIRI]
      ;(println "mapper in: ontURI:" ontIRI (.getScheme ontIRI))
      (if (or (nil? prepath) (= (.getScheme ontIRI) "file"))
        (do
          ;(println "mapper map: no map")
          ontIRI)
        (let [ontURI (.toURI ontIRI),
              uriPath* (last (.split (.getPath ontURI) "/")),
              uriPath (if (= (.indexOf uriPath* ".") -1)
                        (str uriPath* ".owl")
                        uriPath*),
              iri (IRI/create (path->uri uriPath prepath))]
          ;(println "mapper map:" ontIRI "->" iri)
          iri)))))

(defvar- missing-import-handler
  (proxy [MissingImportListener] []
    (importMissing [event]
      (let [uri (.getImportedOntologyURI event)]
        (println "!!! could not load ontology " uri "!!!")))))

(defn load-ontology
  ([path] (load-ontology path nil))
  ([path pre-path]
    ;(println "loading ontology" path pre-path)
    (let [manager (OWLManager/createOWLOntologyManager),
          se (. manager setSilentMissingImportsHandling true)
          mih (. manager addMissingImportListener missing-import-handler),
          iri-map (. manager addIRIMapper (iri-to-file-mapper pre-path)),          
          documentIRI (IRI/create (path->uri path pre-path)),
          ontology (. manager loadOntology documentIRI),
          reasoner (. (new Reasoner$ReasonerFactory) createReasoner ontology)]
      (. reasoner prepareReasoner)
      ;(println "ontology loaded")
      ;(println "direct imports" (count (.getDirectImports manager ontology)))
      ;(println "processing goal:" wff)
      ;      (println "consistent ontology?:" (.isConsistent reasoner))
      ;      (println "number of axioms:" (.getLogicalAxiomCount ontology))
      ;      (println "axioms:" (map (memfn toString) (.getLogicalAxioms ontology)))
      ;      (println "object-properties:" (map (memfn toString) (.getObjectPropertiesInSignature ontology true)))
      ;      (println "number of object-properties:" (count (.getObjectPropertiesInSignature ontology true)))
      ;      (println "number of data-properties:" (count (.getDataPropertiesInSignature ontology true)))
      ;      (println "individuals:" (map (memfn toString) (.getIndividualsInSignature ontology true)))
      ;      (println "number of individuals:" (count (.getIndividualsInSignature ontology true)))
      ;      (println "classes:" (map (memfn toString) (.getFlattened (.getSubClasses reasoner (. (. manager getOWLDataFactory) getOWLThing) false))))
      ;      (println "number of classes:" (count (.getFlattened (.getSubClasses reasoner (. (. manager getOWLDataFactory) getOWLThing) false))))
       {:ontology ontology, :reasoner reasoner})))

(defn generate-arguments-from-owl
  ([ontology type]
    (generate-arguments-from-owl ontology type '()))
  ([ontology type optionals]
    (and (:ontology ontology)
      (condp = type
          :reasoner (generate-arguments-from-reasoner (:ontology ontology) (:reasoner ontology)),
        :rule (generate-arguments-from-rules (map-ontology (:ontology ontology) optionals) '()),
        (throw (Exception. "Invalid type value for owl generator"))))))

(defn individuals [ontology]
  "returns all individuals in the ontology as a list of (class individual)"
  (mapcat (fn [class]
            (let [individuals (.getIndividuals class ontology)
                  sid (symbol (.toStringID class))]
              (prn (bean class))
              (if (not (empty? individuals))
                (map (fn [individual]
                       (list sid (symbol (.toStringID individual))))
                     individuals)
                ())))
          (.getClassesInSignature ontology)))

(defn create-iri [s]
  (IRI/create s))

(defn classes
  ([ontology]
     (seq (.getClassesInSignature ontology)))
  ([ontology iri]
     (seq (filter #(do (prn "obj =") (prn %) (instance? OWLClass %))
                  (.getEntitiesInSignature ontology iri)))))

(defn root-ontology [reasoner]
  (.getRootOntology reasoner))

(defn instances [reasoner class]
  "returns instances of class"
  (seq (.getFlattened (.getInstances reasoner class false))))

(defn individuals [ontology]
  (seq (.getIndividualsInSignature ontology)))

(defn to-symbol [obj]
  (symbol (.toStringID obj)))

(defn- x-properties [ontology individual f]
  (reduce (fn [acc [k v]]
            (assoc acc (to-symbol k) (map #(to-symbol %) v)))
          {}
          (into {} (f individual ontology))))

(defn object-properties
  ([ontology]
     (seq (.getObjectPropertiesInSignature ontology)))
  ([ontology individual]
     "returns a hashmap of property -> vals"
     (x-properties ontology individual (memfn getObjectPropertyValues ontology))))

(defn data-properties
  ([ontology]
     (seq (.getDataPropertiesInSignature ontology)))
  ([ontology individual]
     (x-properties ontology individual (memfn getDataPropertyValues ontology))))

(defn x-properties-seq [ontology individual f]
  "returns a seq of (property indivi val) from the individual"
  (map (fn [[k v]]
         (list k (to-symbol individual) v))
       (mapcat (fn [[k v]]
                 (cartesian-product [k] v))
               (f ontology individual))))

(defn object-properties-seq [ontology individual]
  "returns a seq of (property indiv val) from the individual"
  (x-properties-seq ontology individual object-properties))

(defn data-properties-seq [ontology individual]
  "returns a seq of (property indiv val) from the individual"
  (x-properties-seq ontology individual data-properties))

(defn parse-class-expression [ontology s]
  (try
    (let [data-factory (.. ontology getOWLOntologyManager getOWLDataFactory)
          manager (.getOWLOntologyManager ontology)
          import-closures (.getImportsClosure ontology)
          short-form-provider (SimpleShortFormProvider.)
          bidi (BidirectionalShortFormProviderAdapter. manager import-closures short-form-provider)
          parser (ManchesterOWLSyntaxEditorParser. data-factory s)
          entity-checker (ShortFormEntityChecker. bidi)]
      (.setDefaultOntology parser ontology)
      (.setOWLEntityChecker parser entity-checker)
      (.parseClassExpression parser))
    (catch ParserException e nil)))

(defn shorten [sym]
  (last (clojure.string/split (str sym) #"#")))

(defn instances-with-property [reasoner property]
  "returns a list of (property indiv val) "
  (let [prop (shorten property)
        ontology (root-ontology reasoner)]
    (prn "prop =")
    (prn prop)
    (if-let [ex (parse-class-expression ontology (format "(%s SOME Thing)" prop))]
     (let [individuals (into #{} (.getFlattened (.getInstances reasoner ex true)))]
       (prn "ex =")
       (prn ex)
       (mapcat (fn [individual]
                 ;; TODO extract the good property with the OWL API
                 (let [object-properties (object-properties-seq ontology individual)
                       data-properties (data-properties-seq ontology individual)]
                   (prn "obj props =")
                   (prn object-properties)
                   (concat (filter #(= (first %) property) object-properties)
                           (filter #(= (first %) property) data-properties))))
        individuals))
     ())))