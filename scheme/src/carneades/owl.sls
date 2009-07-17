#!r6rs

(library
 (carneades owl)
 
 (export import-owl)
 
 (import (rnrs)
         (carneades rule)
         (carneades system)
         (only (carneades lib srfi strings) string-trim)
         (carneades lib xml ssax ssax-code)
         (carneades lib xml ssax access-remote)
         (carneades lib xml sxml sxpath))
 
 (define namespaces '((owl . "http://www.w3.org/2002/07/owl#")
                      (xsd . "http://www.w3.org/2001/XMLSchema#")
                      (owl2xml . "http://www.w3.org/2006/12/owl2-xml#")
                      (rdfs . "http://www.w3.org/2000/01/rdf-schema#")
                      (rdf . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")))
 
 (define ontology-path "rdf:RDF")
 
 (define (import-owl path)
   (let* ((doc (ssax:dtd-xml->sxml (open-input-resource path) '()))
          (owl-body ((sxpath ontology-path namespaces) doc)))
     (ontology->rules owl-body)))
 
 #|
    An OWL document consists of:
    - optional ontology heders (generally at most one)
    - class axioms
    - property axioms
    - facts about individuals

    !!! we wil only concentrate on class and property axioms !!!
|#
 
 ; -------------------------
 ; Classes
 ; -------------------------
 
 ; class descriptions
 
 (define (get-owl-classes sxml)
   ((sxpath "owl:Class" namespaces) sxml))
 
 (define (get-class-enumerations sxml)
   ((sxpath "owl:oneOF" namespaces) sxml))
 
 (define (get-class-property-restrictions sxml)
   ((sxpath "owl:Restriction" namespaces) sxml))
 
 (define (get-class-intersections sxml)
   ((sxpath "owl:intersectionOf" namespaces) sxml))
 
 (define (get-class-unions sxml)
   ((sxpath "owl:unionOf" namespaces) sxml))
 
 (define (get-class-complements sxml)
   ((sxpath "owl:complementOf" namespaces) sxml))
 
 ; class axioms
 
 (define (get-subclasses sxml)
   ((sxpath "rdfs:subClassOf" namespaces) sxml))
 
 (define (get-equivalent-classes sxml)
   ((sxpath "owl:equivalentClass" namespaces) sxml))
 
 (define (get-disjoint-classes sxml)
   ((sxpath "owl:disjointWith" namespaces) sxml))
 
 
 ; -------------------------
 ; Properties
 ; -------------------------
 
 (define (get-object-properties sxml)
   ((sxpath "owl:ObjectProperty" namespaces) sxml))
 
 (define (get-data-properties sxml)
   ((sxpath "owl:DatatypeProperty" namespaces) sxml))
 
 (define (get-subproperties sxml)
   ((sxpath "owl:subPropertyOf" namespaces) sxml))
 
 (define (get-domain sxml)
   ((sxpath "rdfs:domain" namespaces) sxml))
 
 (define (get-range sxml)
   ((sxpath "rdfs:range" namespaces) sxml))
 
 (define (get-equivalent-properties sxml)
   ((sxpath "owl:equivalentProperty" namespaces) sxml))
 
 (define (get-inverse-properties sxml)
   ((sxpath "owl:inverseOf" namespaces) sxml))
 
 (define (get-functional-properties sxml)
   ((sxpath "owl:FunctionalProperty" namespaces) sxml))
 
 (define (get-inverse-functional-properties sxml)
   ((sxpath "owl:InverseFunctionalProperty" namespaces) sxml))
 
 (define (get-transitive-properties sxml)
   ((sxpath "owl:TransitiveProperty" namespaces) sxml))
 
 (define (get-symmetric-properties sxml)
   ((sxpath "owl:SymmetricProperty" namespaces) sxml))
 
 ; -------------------------
 ; Ontology -> Rule Convertion
 ; -------------------------
 
 (define (ontology->rules ontology)
   (let* ((class-axioms (get-owl-classes ontology))
          (object-properties (get-object-properties ontology))
          (data-properties (get-data-properties ontology)))
#|     (display "class-axioms: ")
     (display class-axioms)
     (newline)
     (newline)
     (display "object-properties: ")
     (display object-properties)
     (newline)
     (newline)
     (display "data-properties: ")
     (display data-properties)
     (newline)
     (newline) |#
     (add-rules empty-rulebase (map class-axiom->rule class-axioms))))
 
 (define (class-axiom->rule axiom)
   (let* ((rdf-id ((sxpath "@rdf:ID/text()" namespaces) axiom))
          (rdf-about ((sxpath "@rdf:about/text()" namespaces) axiom))
          (axiom-name (cond ((not (null? rdf-id)) (car rdf-id))
                            ((not (null? rdf-about)) (string-trim (car rdf-about) #\#))
                            (else (begin (display axiom)
                                         (newline)
                                         (raise "no class name found")))))
          (rule-name (gensym (string-append axiom-name "-rule")))
          (rule-head (list (string->symbol axiom-name) '?x))
          (subclasses (get-subclasses axiom))
          (equivalents (get-equivalent-classes axiom))
          (disjoints (get-disjoint-classes axiom))
          (clauses (axioms->clauses subclasses equivalents disjoints)))
     (make-rule rule-name 
                #f
                (make-rule-head rule-head)
                (make-rule-body clauses))))
 
 (define (axioms->clauses subclasses equivalents disjoints)
   '(foo ?x))
   

 
 ; -------------------------
 ; Test Code
 ; -------------------------
 
  
 (define sxml '(*TOP* (doc (^ (foo "foo"))
                           (title "Hello world")
                           (title "foo bar"))))
 
 (define doc ((sxpath "doc" '()) sxml))
 
 (define titles ((sxpath "title" '()) doc))
 
 (define hund (ssax:dtd-xml->sxml (open-input-resource "C:\\Users\\stb\\Desktop\\Hundeformular.owl") '()))
 
 (define classes ((sxpath "rdf:RDF/owl:Class" namespaces) hund))
 
 (define class1 (car classes))
 
 (define rb (import-owl "C:\\Users\\stb\\Desktop\\Hundeformular.owl"))
 
 )
