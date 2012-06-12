;;; Copyright (c) 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Functions to maanage and query an OWL reasoner"}
  carneades.engine.owl.reasoner 
  ;; (:use carneades.engine.statement
  ;;         carneades.engine.unify
  ;;         carneades.engine.argument
  ;;         [carneades.engine.argument :as arg])
  ;; (:import org.semanticweb.owlapi.apibinding OWLManager
  ;;            java.net URI
  ;;            java.io File
  ;;            org.semanticweb.owlapi.model IRI)
  )

;; (defvar- *debug* false)

;; (defn class-instances-to-responses
;;   [reasoner manager wff subs ontology]
;;   (if *debug* (println "finding instances of class" (literal->str wff)))
;;   (let [cname (str (first wff)),
;;         clazz (. (. manager getOWLDataFactory) getOWLClass (IRI/create cname)),
;;         insts (. (. reasoner getInstances clazz false) getFlattened)]
;;     (if *debug* (println "# of instances found:" (count insts)))
;;     (map (fn [i]
;;            (let [c (list (first wff) (symbol (. i toStringID))),
;;                  subs2 (unify c wff subs)]
;;              (make-response
;;                (or subs2 subs)
;;                [(list 'valid (symbol (. (. (. ontology getOntologyID) getOntologyIRI) toString)))]
;;                (make-argument
;;                  :id (gensym "a")
;;                  :conclusion c
;;                  :premises  
;;                  (list (list 'valid (symbol (. (. (. ontology getOntologyID) getOntologyIRI) toString))))
;;                  :scheme "HermiT"))))
;;       insts)))

;; (defn single-class-instance-to-responses
;;   [reasoner manager wff subs ontology]
;;   (if *debug* (println "finding instances of class" (literal->str wff)))
;;   (let [cname (str (first wff)),
;;         clazz (. (. manager getOWLDataFactory) getOWLClass (IRI/create cname)),
;;         iname (str (second wff)),
;;         ind (. (. manager getOWLDataFactory) getOWLNamedIndividual (IRI/create iname)),
;;         insts (map (memfn toStringID) (. (. reasoner getInstances clazz false) getFlattened))]
;;     (if *debug* (println "# of instances found:" (count insts)))
;;     (if (some #{iname} insts)
;;       (let []
;;         (if *debug* (println iname "is instance of" cname))
;;         (list (make-response
;;                 subs
;;                 [(list 'valid (symbol (. (. (. ontology getOntologyID) getOntologyIRI) toString)))]
;;                 (make-argument
;;                   :id (gensym "a")
;;                   :conclusion wff
;;                   :premises (list (list 'valid (symbol (. (. (. ontology getOntologyID) getOntologyIRI) toString))))
;;                   :scheme "HermiT"))))
;;       nil)))

;; (defn property-instances-to-responses
;;   [reasoner manager wff subs ontology]
;;   (if *debug* (println "finding instances of property:" (literal->str wff)))
;;   (let [pname (str (first wff)),
;;         prop-type (cond
;;                     (. ontology containsDataPropertyInSignature (IRI/create pname)) :data,
;;                     (. ontology containsObjectPropertyInSignature (IRI/create pname) true) :object),
;;         prop (condp = prop-type
;;                :data (. (. manager getOWLDataFactory) getOWLDataProperty (IRI/create pname)),
;;                :object (. (. manager getOWLDataFactory) getOWLObjectProperty (IRI/create pname))),
;;         iname (str (second wff)),
;;         ind (. (. manager getOWLDataFactory) getOWLNamedIndividual (IRI/create iname)),
;;         insts (condp = prop-type
;;                 :data (map (memfn getLiteral) (. reasoner getDataPropertyValues ind prop)),
;;                 :object (map (memfn toStringID) (. (. reasoner getObjectPropertyValues ind prop) getFlattened)))]
;;     (if *debug* (println "# of instances found:" (count insts)))
;;     (map (fn [i]
;;            (let [c (list (first wff) (second wff) (symbol i)),
;;                  subs2 (unify c wff subs)]
;;              (make-response
;;                subs2
;;                [(list 'valid (symbol (. (. (. ontology getOntologyID) getOntologyIRI) toString)))]
;;                (make-argument
;;                  :id (gensym "a")
;;                  :conclusion c
;;                  :premises (list (list 'valid (symbol (. (. (. ontology getOntologyID) getOntologyIRI) toString))))
;;                  :scheme "HermiT"))))
;;       insts)))

;; (defn single-property-instance-to-responses
;;   [reasoner manager wff subs ontology]
;;   (if *debug* (println "finding instances of property" (literal->str wff)))
;;   (let [pname (str (first wff)),
;;         prop-type (cond
;;                     (. ontology containsDataPropertyInSignature (IRI/create pname)) :data,
;;                     (. ontology containsObjectPropertyInSignature (IRI/create pname) true) :object),
;;         prop (condp = prop-type
;;                :data (. (. manager getOWLDataFactory) getOWLDataProperty (IRI/create pname)),
;;                :object (. (. manager getOWLDataFactory) getOWLObjectProperty (IRI/create pname))),
;;         iname (str (second wff)),
;;         iname2 (str (nth wff 2)),
;;         ind (. (. manager getOWLDataFactory) getOWLNamedIndividual (IRI/create iname)),
;;         insts (condp = prop-type
;;                 :data (map (memfn getLiteral) (. reasoner getDataPropertyValues ind prop)),
;;                 :object (map (memfn toStringID) (. (. reasoner getObjectPropertyValues ind prop) getFlattened)))]
;;     (if *debug* (println "# of instances found:" (count insts)))
;;     (if (some #{iname2} insts)
;;       (list (make-response
;;               subs
;;               [(list 'valid (symbol (. (. (. ontology getOntologyID) getOntologyIRI))))]
;;               (arg/argument
;;                 (gensym "a")
;;                 :pro
;;                 wff
;;                 (list (arg/pm (list 'valid (symbol (. (. (. ontology getOntologyID) getOntologyIRI) toString)))))
;;                 "HermiT")))
;;       nil)))


;; (defn generate-arguments-from-reasoner
;;   "Generates argument from a OWL reasoner"
;;   [ontology reasoner]
;;   (fn [subgoal state]    
;;     (let [manager (OWLManager/createOWLOntologyManager),          
;;           subs (:substitutions state),
;;           wff (apply-substitutions subs (statement-wff subgoal))]
;;       (if *debug* (println "ontology" ontology "\nreasoner" reasoner))
;;       (cond
;;         (and
;;           (seq? wff)
;;           (= (count wff) 2)
;;           (variable? (second wff))) (class-instances-to-responses reasoner manager wff subs ontology),
;;         (and
;;           (seq? wff)
;;           (= (count wff) 2)
;;           (not (variable? (second wff)))) (single-class-instance-to-responses reasoner manager wff subs ontology),
;;         (and
;;           (seq? wff)
;;           (= (count wff) 3)
;;           (variable? (nth wff 2))) (property-instances-to-responses reasoner manager wff subs ontology)
;;         (and
;;           (seq? wff)
;;           (= (count wff) 3)
;;           (not (variable? (nth wff 2)))) (single-property-instance-to-responses reasoner manager wff subs ontology)))))
