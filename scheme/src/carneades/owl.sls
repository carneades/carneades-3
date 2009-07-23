;;; Carneades Argumentation Library and Tools.
;;; Copyright (C) 2008 Thomas F. Gordon, Fraunhofer FOKUS, Berlin
;;; 
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License version 3 (LGPL-3)
;;; as published by the Free Software Foundation.
;;; 
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for details.
;;; 
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

#!r6rs

(library
 (carneades owl)
 
 (export import-owl)
 
 (import (rnrs)
         (carneades rule)
         (carneades system)
         (carneades dnf)
         (only (carneades lib srfi strings) string-trim string-suffix?)
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
    - optional ontology headers (generally at most one)
    - class axioms
    - property axioms
    - facts about individuals

    !!! we wil only concentrate on class and property axioms !!!
|#
 
#|
    OWL class axioms
    - subclasses          - total support
    - equivalent classes  - partial support as far as rules only support conjunctions of literals in the head
    - disjoint classes    - partial support (s.a.); maybe different semantic
    - complete classes    - !!! no support !!! will propably be implemented later

    OWL class descriptions
    - identifier            - total support
    - enumerations          - !!! no support !!! no equation theory in carneades
    - property restrictions - partial support
      - allValuesFrom  - !!! no support !!!
      - someValuesFrom - total support
      - hasValue       - total support
      - maxCardinality - !!! no support !!!
      - minCardinality - !!! no support !!!
      - Cardinality    - !!! no support !!!
    - intersections         - total support
    - unions                - total support
    - complements           - total support
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
 
 (define (get-domains sxml)
   ((sxpath "rdfs:domain" namespaces) sxml))
 
 (define (get-ranges sxml)
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
 ; RDF
 ; -------------------------
 
 (define (get-types sxml)
   ((sxpath "rdf:type" namespaces) sxml))
 
 
 ; -------------------------
 ; Ontology -> Rule Convertion
 ; -------------------------
 
 (define (ontology->rules ontology)
   (let* ((class-axioms (get-owl-classes ontology))
          (object-properties (get-object-properties ontology))
          (data-properties (get-data-properties ontology))
          (transitive-properties (get-transitive-properties ontology))
          (symmetric-properties (get-symmetric-properties ontology))
          (object-property-rules (filter (lambda (l) (not (null? l))) (flatmap property-axiom->rules object-properties)))
          (data-property-rules (filter (lambda (l) (not (null? l))) (flatmap property-axiom->rules data-properties)))
          (transitive-property-rules (filter (lambda (l) (not (null? l)))
                                             (flatmap transitive-property-axiom->rules transitive-properties)))
          (symmetric-property-rules (filter (lambda (l) (not (null? l)))
                                            (flatmap symmetric-property-axiom->rules symmetric-properties)))
          (class-rules (filter (lambda (l) (not (null? l))) (flatmap class-axiom->rules class-axioms))))
     (add-rules empty-rulebase (append class-rules object-property-rules data-property-rules))))
 
 (define (class-axiom->rules axiom)
   (let* ((rdf-id ((sxpath "@rdf:ID/text()" namespaces) axiom))
          (rdf-about ((sxpath "@rdf:about/text()" namespaces) axiom))
          (axiom-name (cond ((not (null? rdf-id)) (car rdf-id))
                            ((not (null? rdf-about)) (string-trim (car rdf-about) #\#))
                            (else (begin (display axiom)
                                         (newline)
                                         (error "class-axiom->rule" "no class name found" axiom)))))
          (subclasses (get-subclasses axiom))
          (equivalents (get-equivalent-classes axiom))
          (disjoints (get-disjoint-classes axiom))
          (subclass-rules (map (lambda (s) (subclass-axiom->rule axiom-name s)) subclasses))
          (equivalent-rules (flatmap (lambda (s) (equivalent-axiom->rules axiom-name s)) equivalents))
          (disjoint-rules (flatmap (lambda (s) (disjoint-axiom->rules axiom-name s)) disjoints)))
     (append subclass-rules equivalent-rules disjoint-rules)))
 
 (define (subclass-axiom->rule axiom-name sxml)
   (let* ((rdf-resource ((sxpath "@rdf:resource/text()" namespaces) sxml))
          (owl-classes (get-owl-classes sxml))
          (restrictions (get-class-property-restrictions sxml))
          (class-argument (cond ((not (null? rdf-resource))
                                 (list (string->symbol (string-trim (car rdf-resource) #\#)) (string->symbol (string-append "?" axiom-name))))
                            ((not (null? owl-classes)) (class-description->formula axiom-name (car owl-classes)))
                            ((not (null? restrictions)) (class-restriction->formula axiom-name (car restrictions)))
                            (else (error "subclass-axiom->rule" "no class description found" sxml)))))
     (make-rule (string->symbol (string-append "subclass-" axiom-name "-rule"))
                #f
                (make-rule-head class-argument)
                (make-rule-body (list (string->symbol axiom-name) (string->symbol (string-append "?" axiom-name)))))))

 
 (define (equivalent-axiom->rules axiom-name sxml)
   (let* ((rdf-resource ((sxpath "@rdf:resource/text()" namespaces) sxml))
          (owl-classes (get-owl-classes sxml))
          (restrictions (get-class-property-restrictions sxml))
          (class-argument (cond ((not (null? rdf-resource))
                                 (list (string->symbol (string-trim (car rdf-resource) #\#)) (string->symbol (string-append "?" axiom-name))))
                            ((not (null? owl-classes)) (class-description->formula axiom-name (car owl-classes)))
                            ((not (null? restrictions)) (class-restriction->formula axiom-name (car restrictions)))
                            (else (error "equivalent-axiom->rules" "no class description found" sxml))))
          (rule-< (make-rule (string->symbol (string-append "equivalent-" axiom-name "-<-rule"))
                             #f
                             (make-rule-head class-argument)
                             (make-rule-body (list (string->symbol axiom-name) (string->symbol (string-append "?" axiom-name))))))
          (rule-> (make-rule (string->symbol (string-append "equivalent-" axiom-name "->-rule"))
                      #f
                      (make-rule-head (list (string->symbol axiom-name) (string->symbol (string-append "?" axiom-name))))
                      (make-rule-body class-argument))))

     (if (lconjunction? class-argument)
         (list rule-< rule->)
         (list rule->)))) 
 
 (define (disjoint-axiom->rules axiom-name sxml)
   (let* ((rdf-resource ((sxpath "@rdf:resource/text()" namespaces) sxml))
          (owl-classes (get-owl-classes sxml))
          (restrictions (get-class-property-restrictions sxml))
          (class-argument (cond ((not (null? rdf-resource))
                                 (list (string->symbol (string-trim (car rdf-resource) #\#)) (string->symbol (string-append "?" axiom-name))))
                            ((not (null? owl-classes)) (class-description->formula axiom-name (car owl-classes)))
                            ((not (null? restrictions)) (class-restriction->formula axiom-name (car restrictions)))
                            (else (error "disjoint-axiom->rules" "no class description found" sxml))))
          (rule-< (make-rule (string->symbol (string-append "disjoint-" axiom-name "-<-rule"))
                      #f
                      (make-rule-head (list 'not class-argument))
                      (make-rule-body (list (string->symbol axiom-name) (string->symbol (string-append "?" axiom-name))))))
          (rule-> (make-rule (string->symbol (string-append "disjoint-" axiom-name "->-rule"))
                      #f
                      (make-rule-head (list 'not (list (string->symbol axiom-name) (string->symbol (string-append "?" axiom-name)))))
                      (make-rule-body class-argument))))
     (if (literal? class-argument)
         (list rule-< rule->)
         (list rule->))))
 
 (define (property-axiom->rules axiom)
   (let* ((rdf-id ((sxpath "@rdf:ID/text()" namespaces) axiom))
          (rdf-about ((sxpath "@rdf:about/text()" namespaces) axiom))         
          (property-name (cond ((not (null? rdf-id)) (car rdf-id))
                               ((not (null? rdf-about)) (string-trim (car rdf-about) #\#))
                               (else (begin (display axiom)
                                            (newline)
                                            (error "property-axiom->rule" "no property name found" axiom)))))
          (subproperties (get-subproperties axiom))
          (domains (get-domains axiom))
          (ranges (get-ranges axiom))
          (equiv-props (get-equivalent-properties axiom))
          (inverse-props (get-inverse-properties axiom))
          (functional-props (get-functional-properties axiom))
          (inverse-functional-props (get-inverse-functional-properties axiom))
          (prop-types (get-types axiom))
          (type-rules (filter (lambda (x) (not (null? x))) (map (lambda (t) (prop-type->rule property-name t)) prop-types)))
          )
     (append (list (make-rule (string->symbol (string-append property-name "-rule"))
                              #f
                              (make-rule-head (string->symbol property-name))
                              '()))
             type-rules)))
 
 (define (prop-type->rule property-name prop-type)
   (let* ((transitive? (let ((rdf-resource ((sxpath "@rdf:resource/text()" namespaces) prop-type)))
                         (and (not (null? rdf-resource))
                              (string-suffix? "TransitiveProperty" (car rdf-resource)))))
          (symmetric? (let ((rdf-resource ((sxpath "@rdf:resource/text()" namespaces) prop-type)))
                        (and (not (null? rdf-resource))
                             (string-suffix? "SymmetricProperty" (car rdf-resource)))))
          (functional? (let ((rdf-resource ((sxpath "@rdf:resource/text()" namespaces) prop-type)))
                         (and (not (null? rdf-resource))
                              (string-suffix? "FunctionalProperty" (car rdf-resource)))))
          (inverse-functional? (let ((rdf-resource ((sxpath "@rdf:resource/text()" namespaces) prop-type)))
                                 (and (not (null? rdf-resource))
                                      (string-suffix? "InverseFunctionalProperty" (car rdf-resource))))))
     (cond (transitive? (make-rule (string->symbol (string-append property-name "-transitive-rule"))
                                   #f
                                   (make-rule-head (list (string->symbol property-name)'?x '?z))
                                   (make-rule-body (list 'and
                                                         (list (string->symbol property-name)'?x '?y)
                                                         (list (string->symbol property-name)'?y '?z)))))
           (symmetric? (make-rule (string->symbol (string-append property-name "-symmetric-rule"))
                                     #f
                                     (make-rule-head (list (string->symbol property-name)'?y '?x))
                                     (make-rule-body (list (string->symbol property-name)'?x '?y))))
           (else '()))))
 
 (define (transitive-property-axiom->rules axiom)
   (let* ((rdf-id ((sxpath "@rdf:ID/text()" namespaces) axiom))
          (rdf-about ((sxpath "@rdf:about/text()" namespaces) axiom))
          ;(rdf-type ((sxpath "@rdf:type/text()" namespaces) axiom))          
          (property-name (cond ((not (null? rdf-id)) (car rdf-id))
                               ((not (null? rdf-about)) (string-trim (car rdf-about) #\#))
                               (else (begin (display axiom)
                                            (newline)
                                            (error "transitive-property-axiom->rule" "no property name found" axiom)))))
          (property-rules (property-axiom->rules axiom))
          (transitive-rule (make-rule (string->symbol (string-append property-name "-transitive-rule"))
                                      #f
                                      (make-rule-head (list (string->symbol property-name)'?x '?z))
                                      (make-rule-body (list 'and
                                                            (list (string->symbol property-name)'?x '?y)
                                                            (list (string->symbol property-name)'?y '?z))))))
     (append property-rules (list transitive-rule))))
 
 (define (symmetric-property-axiom->rules axiom)
   (let* ((rdf-id ((sxpath "@rdf:ID/text()" namespaces) axiom))
          (rdf-about ((sxpath "@rdf:about/text()" namespaces) axiom))
          ;(rdf-type ((sxpath "@rdf:type/text()" namespaces) axiom))          
          (property-name (cond ((not (null? rdf-id)) (car rdf-id))
                               ((not (null? rdf-about)) (string-trim (car rdf-about) #\#))
                               (else (begin (display axiom)
                                            (newline)
                                            (error "symmetric-property-axiom->rule" "no property name found" axiom)))))
          (property-rules (property-axiom->rules axiom))
          (symmetric-rule (make-rule (string->symbol (string-append property-name "-symmetric-rule"))
                                     #f
                                     (make-rule-head (list (string->symbol property-name)'?y '?x))
                                     (make-rule-body (list (string->symbol property-name)'?x '?y)))))
     (append property-rules (list symmetric-rule))))
 

 (define (class-description->formula axiom-name class-desc)  
   (let* ((rdf-id ((sxpath "@rdf:ID/text()" namespaces) class-desc))
          (rdf-about ((sxpath "@rdf:about/text()" namespaces) class-desc))
          ;(rdf-resource ((sxpath "@rdf:resource/text()" namespaces) class-desc))
          ;(rdf-type ((sxpath "@rdf:type/text()" namespaces) class-desc))          
          (class-name (cond ((not (null? rdf-id)) (car rdf-id))
                            ((not (null? rdf-about)) (string-trim (car rdf-about) #\#))
                            ;((not (null? rdf-resource)) (string-trim (car rdf-resource) #\#))
                            (else #f)))
          (enumerations (get-class-enumerations class-desc))          
          (intersections (get-class-intersections class-desc))
          (unions (get-class-unions class-desc))
          (complements (get-class-complements class-desc)))     
     (cond (class-name (list (string->symbol class-name) (string->symbol (string-append "?" axiom-name))))
           ((not (null? enumerations)) (enumeration->formula axiom-name (car enumerations)))           
           ((not (null? intersections)) (intersection->formula axiom-name (car intersections)))
           ((not (null? unions)) (union->formula axiom-name (car unions)))
           ((not (null? complements)) (complement->formula axiom-name (car complements)))
           (else (error "class-description->formula" "no valid class description found" class-desc)))))
 
 (define (class-restriction->formula axiom-name class-restr)
   (let* ((prop (string-trim (car ((sxpath "owl:onProperty/@rdf:resource/text()" namespaces) class-restr)) #\#))
          (someValues ((sxpath "owl:someValuesFrom" namespaces) class-restr))
          (hasValue ((sxpath "owl:hasValue" namespaces) class-restr)))
     
     (cond ((not (null? someValues)) (some-values-restriction->rule axiom-name prop (car someValues)))
           ((not (null? hasValue)) (has-value-restriction->rule axiom-name prop (car hasValue)))
           (else (error "class-restriction->formula" "owl:someValuesFrom or owl:hasValue expected! other restrictions are not supported!" class-restr)))))
 
 (define (some-values-restriction->rule axiom-name prop restriction)
   (let* ((rdf-resource ((sxpath "@rdf:resource/text()" namespaces) restriction))
          (owl-classes (get-owl-classes restriction))
          (restrictions (get-class-property-restrictions restriction))
          (class-argument (cond ((not (null? rdf-resource)) 
                                 (list (string->symbol (string-trim (car rdf-resource) #\#)) (string->symbol (string-append "?" prop))))
                                ((not (null? owl-classes)) (class-description->formula prop (car owl-classes)))
                                ((not (null? restrictions)) (class-restriction->formula prop (car restrictions)))
                                (else (error "some-values-restriction->rule" "no class description found" restriction)))))
     (list 'and 
            (list (string->symbol prop)
                  (string->symbol (string-append "?" axiom-name))
                  (string->symbol (string-append "?" prop)))
            class-argument)))
 
 (define (has-value-restriction->rule axiom-name prop restriction)
   (let* ((rdf-resource ((sxpath "@rdf:resource/text()" namespaces) restriction))
          (data-value ((sxpath "/text()" '()) restriction))
          (value (cond ((not (null? rdf-resource)) (string->symbol (string-trim (car rdf-resource) #\#)))
                       ((not (null? data-value)) (string->symbol (car data-value))))))          
     (list (string->symbol prop) (string->symbol (string-append "?" axiom-name)) value)))
     
 
 (define (enumeration->formula axiom-name enum)
   (error "class-description->formula" "no numerations supported" enum))
 
 (define (intersection->formula axiom-name intersect) 
   (let* ((owl-classes (get-owl-classes intersect))
          (restrictions (get-class-property-restrictions intersect))
          (class-arguments (append (map (lambda (c) (class-description->formula axiom-name c)) owl-classes)
                                   (map (lambda (c) (class-restriction->formula axiom-name c)) restrictions))))
     (cons 'and class-arguments)))
 
 (define (union->formula axiom-name union)
   (let* ((owl-classes (get-owl-classes union))
          (restrictions (get-class-property-restrictions union))
          (class-arguments (append (map (lambda (c) (class-description->formula axiom-name c)) owl-classes)
                                   (map (lambda (c) (class-restriction->formula axiom-name c)) restrictions))))
     (cons 'or class-arguments)))
 
 (define (complement->formula axiom-name comp)
   (let* ((owl-classes (get-owl-classes comp))
          (restrictions (get-class-property-restrictions comp))
          (class-arguments (append (map (lambda (c) (class-description->formula axiom-name c)) owl-classes)
                                   (map (lambda (c) (class-restriction->formula axiom-name c)) restrictions))))
   (cons 'not class-arguments)))
 
 ; -------------------------
 ; UTILS
 ; -------------------------
 
 (define (flatmap f l) (apply append (map f l)))
   

 
 ; -------------------------
 ; Test Code
 ; -------------------------
 
  
 #;(define sxml '(*TOP* (doc (^ (foo "foo"))
                           (title "Hello world")
                           (title "foo bar"))))
 
 ;(define doc ((sxpath "doc" '()) sxml))
 
 ;(define titles ((sxpath "title" '()) doc))
 
 (define hund (ssax:dtd-xml->sxml (open-input-resource "C:\\Users\\stb\\Desktop\\Hundeformular.owl") namespaces))
 
 ;(define classes ((sxpath "rdf:RDF/owl:Class" namespaces) hund))
 
 ;(define class1 (car classes))
 
 (define rb (import-owl "C:\\Users\\stb\\Desktop\\Hundeformular.owl"))
 
 )
