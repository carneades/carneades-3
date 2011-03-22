;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.engine.owl
  (:use clojure.contrib.def
        clojure.contrib.trace
        carneades.engine.owl.reasoner
        carneades.engine.owl.rule
        carneades.engine.rule
        carneades.engine.utils
        clojure.contrib.combinatorics)
  (:require [clojure.xml :as xml])
  (:import (java.net URI)
           (java.io File FileNotFoundException)
           (org.semanticweb.owlapi.expression ParserException ShortFormEntityChecker)
           org.coode.owlapi.manchesterowlsyntax.ManchesterOWLSyntaxEditorParser
           (org.semanticweb.owlapi.util ShortFormProvider SimpleShortFormProvider
                                        BidirectionalShortFormProviderAdapter)
           (org.semanticweb.owlapi.apibinding OWLManager)
           org.semanticweb.owlapi.io.OWLOntologyCreationIOException
           (org.semanticweb.owlapi.model IRI MissingImportListener OWLOntologyIRIMapper OWLClass)
           (org.semanticweb.HermiT Reasoner$ReasonerFactory)))

(defn owl?
  [url]
  (= (:tag (xml/parse url)) :rdf:RDF))

(defn path->uri
  [path prepath]
  (let [file (File. path)]
    (if (.exists file)
      (.toURI file)
      (if (and prepath (not (url? path)))
        (let [fullpath (if (.endsWith prepath java.io.File/separator)
                         (str prepath path)
                         (str prepath java.io.File/separator path))]
         (path->uri fullpath nil))
        (URI. path)))))

(defn iri-to-file-mapper [path prepath]
  (proxy [OWLOntologyIRIMapper] []
    (getDocumentIRI
     [ontIRI]
     (if (= (.getScheme ontIRI) "file")
       ontIRI
       (let [uri (.toURI ontIRI)
             rawfilename (last-segment (str uri))
             filename (add-extension rawfilename "owl")
             parentdir (or (parent path) prepath)
             localfile (if parentdir
                         (create-path parentdir filename)
                         filename)]
         ;; (printf "path = %s prepath = %s parentdir = %s uri = %s\nlocalfile = %s\n"
         ;;         path prepath parentdir uri localfile)
         (if (exists? localfile)
           ;; if the file exists locally to the currently imported file we use it
           (IRI/create (path->uri localfile prepath))
           ;; else we use the URI
           (let [uripath (str uri)
                 resolved (if (nil? (extension rawfilename))
                            (add-extension uripath "owl")
                            uripath)]
             (IRI/create (path->uri resolved prepath)))))))))

(defvar- missing-import-handler
  (proxy [MissingImportListener] []
    (importMissing [event]
      (let [uri (.getImportedOntologyURI event)]
        (throw (java.io.FileNotFoundException. (format "no such file %s" (str uri))))
        ;; (println "!!! could not load ontology " uri "!!!")
        ))))

(defn load-ontology
  ([path] (load-ontology path nil))
  ([path pre-path]
     (try
       (let [manager (OWLManager/createOWLOntologyManager)
             se (. manager setSilentMissingImportsHandling true)
             mih (. manager addMissingImportListener missing-import-handler)
             iri-map (. manager addIRIMapper (iri-to-file-mapper path pre-path))
             documentIRI (IRI/create (path->uri path pre-path))
             ontology (. manager loadOntology documentIRI)
             reasoner (. (new Reasoner$ReasonerFactory) createReasoner ontology)]
         (. reasoner prepareReasoner)
         {:ontology ontology, :reasoner reasoner})
       (catch OWLOntologyCreationIOException e
         (throw (FileNotFoundException. (.getMessage e)))))))

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
     (seq (filter #(instance? OWLClass %)
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
  "returns an Expression from the Manchester query"
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
    (if-let [ex (parse-class-expression ontology (format "(%s SOME Thing)" prop))]
     (let [individuals (into #{} (.getFlattened (.getInstances reasoner ex true)))]
       (mapcat (fn [individual]
                 ;; TODO extract the good property with the OWL API
                 (let [object-properties (object-properties-seq ontology individual)
                       data-properties (data-properties-seq ontology individual)]
                   (concat (filter #(= (first %) property) object-properties)
                           (filter #(= (first %) property) data-properties))))
        individuals))
     ())))