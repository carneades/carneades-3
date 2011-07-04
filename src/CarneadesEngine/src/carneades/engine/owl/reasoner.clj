;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Functions to maanage and query an OWL reasoner"}
  ;;; TODO merge / replace with owl.clj ?
    carneades.engine.owl.reasoner
  (:require     
    [carneades.engine.argument-search :as as] ; for testing only
    )   
  (:use
    ;clojure.contrib.profile ; for testing
    clojure.contrib.def
    carneades.engine.statement
    carneades.engine.unify
    [carneades.engine.argument :as arg])
  (:import    
    (org.semanticweb.owlapi.apibinding OWLManager)        
    (java.net URI)
    (java.io File)
    (org.semanticweb.owlapi.model IRI)
    ;(org.semanticweb.owlapi.vocab OWLRDFVocabulary)
    ;(org.semanticweb.owlapi.reasoner OWLReasoner OWLReasonerFactory)
    ;(org.semanticweb.owlapi.util SimpleIRIMapper)
    ;(java.net URI)
    ;(java.io File)
    )
  )

(defvar- *debug* true)

(defn class-instances-to-responses
  [reasoner manager wff subs ontology]
  (if *debug* (println "finding instances of class" (statement-formatted wff)))
  (let [cname (str (first wff)),
        clazz (. (. manager getOWLDataFactory) getOWLClass (IRI/create cname)),
        insts (. (. reasoner getInstances clazz false) getFlattened)]
    (if *debug* (println "# of instances found:" (count insts)))
    (map (fn [i]
           (let [c (list (first wff) (symbol (. i toStringID))),
                 subs2 (unify c wff subs)]
             (as/response
               (or subs2 subs)
               (arg/argument
                 (gensym "a")
                 :pro
                 c
                 (list (arg/am (list 'valid (symbol (. (. (. ontology getOntologyID) getOntologyIRI) toString)))))
                 "HermiT"))))
      insts)))

(defn single-class-instance-to-responses
  [reasoner manager wff subs ontology]
  (if *debug* (println "finding instances of class" (statement-formatted wff)))
  (let [cname (str (first wff)),
        clazz (. (. manager getOWLDataFactory) getOWLClass (IRI/create cname)),
        iname (str (second wff)),
        ind (. (. manager getOWLDataFactory) getOWLNamedIndividual (IRI/create iname)),
        insts (map (memfn toStringID) (. (. reasoner getInstances clazz false) getFlattened))]
    (if *debug* (println "# of instances found:" (count insts)))
    (if (some #{iname} insts)
      (let []
        (if *debug* (println iname "is instance of" cname))
        (list (as/response
                subs
                (arg/argument
                  (gensym "a")
                  :pro
                  wff
                  (list (arg/am (list 'valid (symbol (. (. (. ontology getOntologyID) getOntologyIRI) toString)))))
                  "HermiT"))))
      nil)))

(defn property-instances-to-responses
  [reasoner manager wff subs ontology]
  (if *debug* (println "finding instances of property:" (statement-formatted wff)))
  (let [pname (str (first wff)),
        prop-type (cond
                    (. ontology containsDataPropertyInSignature (IRI/create pname)) :data,
                    (. ontology containsObjectPropertyInSignature (IRI/create pname) true) :object),
        prop (condp = prop-type
               :data (. (. manager getOWLDataFactory) getOWLDataProperty (IRI/create pname)),
               :object (. (. manager getOWLDataFactory) getOWLObjectProperty (IRI/create pname))),
        iname (str (second wff)),
        ind (. (. manager getOWLDataFactory) getOWLNamedIndividual (IRI/create iname)),
        insts (condp = prop-type
                :data (map (memfn getLiteral) (. reasoner getDataPropertyValues ind prop)),
                :object (map (memfn toStringID) (. (. reasoner getObjectPropertyValues ind prop) getFlattened)))]
    (if *debug* (println "# of instances found:" (count insts)))
    (map (fn [i]
           (let [c (list (first wff) (second wff) (symbol i)),
                 subs2 (unify c wff subs)]
             (as/response
               subs2
               (arg/argument
                 (gensym "a")
                 :pro
                 c
                 (list (arg/am (list 'valid (symbol (. (. (. ontology getOntologyID) getOntologyIRI) toString)))))
                 "HermiT"))))
      insts)))

(defn single-property-instance-to-responses
  [reasoner manager wff subs ontology]
  (if *debug* (println "finding instances of property" (statement-formatted wff)))
  (let [pname (str (first wff)),
        prop-type (cond
                    (. ontology containsDataPropertyInSignature (IRI/create pname)) :data,
                    (. ontology containsObjectPropertyInSignature (IRI/create pname) true) :object,
                    :else false),
        prop (condp = prop-type
               :data (. (. manager getOWLDataFactory) getOWLDataProperty (IRI/create pname)),
               :object (. (. manager getOWLDataFactory) getOWLObjectProperty (IRI/create pname)),
               false false),
        iname (str (second wff)),
        iname2 (str (nth wff 2)),
        ind (. (. manager getOWLDataFactory) getOWLNamedIndividual (IRI/create iname)),
        insts (condp = prop-type
                :data (map (memfn getLiteral) (. reasoner getDataPropertyValues ind prop)),
                :object (map (memfn toStringID) (. (. reasoner getObjectPropertyValues ind prop) getFlattened)),
                false '())]
    (if *debug* (println "# of instances found:" (count insts)))
    (if (some #{iname2} insts)
      (list (as/response
              subs
              (arg/argument
                (gensym "a")
                :pro
                wff
                (list (arg/am (list 'valid (symbol (. (. (. ontology getOntologyID) getOntologyIRI) toString)))))
                "HermiT")))
      nil)))


(defn generate-arguments-from-reasoner
  "Generates argument from a OWL reasoner"
  [ontology reasoner]
  (fn [subgoal state]    
    (let [manager (OWLManager/createOWLOntologyManager),          
          subs (:substitutions state),
          wff (apply-substitution subs (statement-wff subgoal))]
      (if *debug* (println "ontology" ontology "\nreasoner" reasoner))
      (cond
        (and
          (seq? wff)
          (= (count wff) 2)
          (variable? (second wff))) (class-instances-to-responses reasoner manager wff subs ontology),
        (and
          (seq? wff)
          (= (count wff) 2)
          (not (variable? (second wff)))) (single-class-instance-to-responses reasoner manager wff subs ontology),
        (and
          (seq? wff)
          (= (count wff) 3)
          (variable? (nth wff 2))) (property-instances-to-responses reasoner manager wff subs ontology)
        (and
          (seq? wff)
          (= (count wff) 3)
          (not (variable? (nth wff 2)))) (single-property-instance-to-responses reasoner manager wff subs ontology)))))
