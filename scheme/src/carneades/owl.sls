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


#|
    An OWL document consists of:
    - optional ontology headers (generally at most one) - !!! no support !!!
    - class axioms                                      - partial support
    - property axioms                                   - partial support
    - facts about individuals                           - !!! no support !!! will propably be implemented later
    
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

#|
    OWL property axioms
    - subproperty                 - total support
    - domain                      - total support
    - range                       - total support
    - equivalent property         - total support
    - inverse property            - total support
    - functional property         - !!! no support !!!
    - inverse functional property - !!! no support !!!
    - symmetric property          - total support
    - transitive property         - total support
|#

#!r6rs

(library
 (carneades owl)
 
 (export owl-import)
 
 (import (rnrs)
         (carneades rule)
         (carneades system)
         (carneades dnf)
         (only (carneades lib srfi strings) string-trim string-suffix? string-prefix? string-replace)
         (carneades lib xml ssax ssax-code)
         (carneades lib xml ssax access-remote)
         (carneades lib xml sxml sxpath))
 
 (define *debug* #t)
 
 (define namespaces '((owl . "http://www.w3.org/2002/07/owl#")
                      (xsd . "http://www.w3.org/2001/XMLSchema#")
                      (owl2xml . "http://www.w3.org/2006/12/owl2-xml#")
                      (rdfs . "http://www.w3.org/2000/01/rdf-schema#")
                      (rdf . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")))
 
 (define prefixes '("http://www.w3.org/2002/07/owl#"
                    "http://www.w3.org/2001/XMLSchema#"
                    "http://www.w3.org/2006/12/owl2-xml#"
                    "http://www.w3.org/2000/01/rdf-schema#"
                    "http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
 
 
 (define ontology-path "rdf:RDF")
   
   (define (owl-import path)
     (let* ((doc (ssax:dtd-xml->sxml (open-input-resource path) '()))
            (owl-body ((sxpath ontology-path namespaces) doc)))
       (ontology->rules owl-body)))
   
   
   
   ; -------------------------
   ; class parser
   ; -------------------------
   
   (define classes '())
   
   (define (add-class c)
     (set! classes (append classes (list (apply-namespaces c namespaces)))))
   
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
   ; property parser
   ; -------------------------
   
   (define properties '())
   
   (define (add-property p)
     (set! properties (append properties (list (apply-namespaces p namespaces)))))
   
   (define (get-object-properties sxml)
     ((sxpath "owl:ObjectProperty" namespaces) sxml))
   
   (define (get-data-properties sxml)
     ((sxpath "owl:DatatypeProperty" namespaces) sxml))
   
   (define (get-subproperties sxml)
     ((sxpath "rdfs:subPropertyOf" namespaces) sxml))
   
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
   ; individual parser
   ; -------------------------
   
   (define (get-descriptions sxml)
     ((sxpath "rdf:Description" namespaces) sxml))
   
   (define (get-class-members sxml)
     (filter (lambda (c) (not (null? c)))
             (map (lambda (c) 
                    (let ((member ((sxpath c namespaces) sxml)))
                      (if (null? member)
                          '()
                          (cons c member))))
                    classes)))
   
   (define (get-property-values sxml)
     (filter (lambda (p) (not (null? p)))
             (map (lambda (c) 
                    (let ((member ((sxpath c namespaces) sxml)))
                      (if (null? member)
                          '()
                          (cons c member))))
                  properties)))
   
   ; -------------------------
   ; rdf parser
   ; -------------------------
   
   (define (get-types sxml)
     ((sxpath "rdf:type" namespaces) sxml))
   
   
   ; -------------------------
   ; Ontology -> Rule Convertion
   ; -------------------------
   
   (define (ontology->rules ontology)     
     (let* ((xml-base ((sxpath "@xml:base/text()" namespaces) ontology))
            (namespaces-changed (if (not (null? xml-base))
                                    (set! namespaces (cons (cons 'base (string-append (car xml-base) "#")) namespaces))
                                    namespaces))
            (class-axioms (get-owl-classes ontology))
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
            (class-rules (filter (lambda (l) (not (null? l))) (flatmap class-axiom->rules class-axioms)))            
            (descriptions (get-descriptions ontology))
            (class-members (get-class-members ontology))
            (description-rules (flatmap description->rules descriptions))
            (class-member-rules (flatmap class-member->rules class-members)))
       (add-rules empty-rulebase (append class-rules
                                         object-property-rules
                                         data-property-rules
                                         transitive-property-rules
                                         symmetric-property-rules))))
   
   ; ----------------------
   ; class-conversion
   ; ----------------------
     
   (define (class-axiom->rules axiom)
     (let* ((rdf-id ((sxpath "@rdf:ID/text()" namespaces) axiom))
            (rdf-about ((sxpath "@rdf:about/text()" namespaces) axiom))
            (axiom-name (cond ((not (null? rdf-id)) (car rdf-id))
                              ((not (null? rdf-about)) (text->name rdf-about))
                              (else (begin (display axiom)
                                           (newline)
                                           (error "class-axiom->rule" "no class name found" axiom)))))
            (subclasses (get-subclasses axiom))
            (equivalents (get-equivalent-classes axiom))
            (disjoints (get-disjoint-classes axiom))
            (subclass-rules (map (lambda (s) (subclass-axiom->rule axiom-name s)) subclasses))
            (equivalent-rules (flatmap (lambda (s) (equivalent-axiom->rules axiom-name s)) equivalents))
            (disjoint-rules (flatmap (lambda (s) (disjoint-axiom->rules axiom-name s)) disjoints)))
       (add-class axiom-name)
       (append subclass-rules equivalent-rules disjoint-rules)))
   
   (define (subclass-axiom->rule axiom-name sxml)
     (let* ((rdf-resource ((sxpath "@rdf:resource/text()" namespaces) sxml))
            (owl-classes (get-owl-classes sxml))
            (restrictions (get-class-property-restrictions sxml))
            (class-argument (cond ((not (null? rdf-resource))
                                   (list (string->symbol (text->name rdf-resource))
                                         (string->symbol (string-append "?" (apply-namespaces axiom-name namespaces)))))
                                  ((not (null? owl-classes)) (class-description->formula axiom-name (car owl-classes)))
                                  ((not (null? restrictions)) (class-restriction->formula axiom-name (car restrictions)))
                                  (else (error "subclass-axiom->rule" "no class description found" sxml)))))
       (make-rule (string->symbol (string-append "subclass-" axiom-name "-rule"))
                  #f
                  (make-rule-head class-argument)
                  (make-rule-body (list (string->symbol axiom-name) (string->symbol (string-append "?" (apply-namespaces axiom-name namespaces))))))))
   
   
   (define (equivalent-axiom->rules axiom-name sxml)
     (let* ((rdf-resource ((sxpath "@rdf:resource/text()" namespaces) sxml))
            (owl-classes (get-owl-classes sxml))
            (restrictions (get-class-property-restrictions sxml))
            (class-argument (cond ((not (null? rdf-resource))
                                   (list (string->symbol (text->name rdf-resource))
                                         (string->symbol (string-append "?" (apply-namespaces axiom-name namespaces)))))
                                  ((not (null? owl-classes)) (class-description->formula axiom-name (car owl-classes)))
                                  ((not (null? restrictions)) (class-restriction->formula axiom-name (car restrictions)))
                                  (else (error "equivalent-axiom->rules" "no class description found" sxml))))
            (rule-< (make-rule (string->symbol (string-append "equivalent-" axiom-name "-<-rule"))
                               #f
                               (make-rule-head class-argument)
                               (make-rule-body (list (string->symbol axiom-name) 
                                                     (string->symbol (string-append "?" (apply-namespaces axiom-name namespaces)))))))
            (rule-> (make-rule (string->symbol (string-append "equivalent-" axiom-name "->-rule"))
                               #f
                               (make-rule-head (list (string->symbol axiom-name)
                                                     (string->symbol (string-append "?" (apply-namespaces axiom-name namespaces)))))
                               (make-rule-body class-argument))))
       
       (if (lconjunction? class-argument)
           (list rule-< rule->)
           (list rule->)))) 
   
   (define (disjoint-axiom->rules axiom-name sxml)
     (let* ((rdf-resource ((sxpath "@rdf:resource/text()" namespaces) sxml))
            (owl-classes (get-owl-classes sxml))
            (restrictions (get-class-property-restrictions sxml))
            (class-argument (cond ((not (null? rdf-resource))
                                   (list (string->symbol (text->name rdf-resource))
                                         (string->symbol (string-append "?" (apply-namespaces axiom-name namespaces)))))
                                  ((not (null? owl-classes)) (class-description->formula axiom-name (car owl-classes)))
                                  ((not (null? restrictions)) (class-restriction->formula axiom-name (car restrictions)))
                                  (else (error "disjoint-axiom->rules" "no class description found" sxml))))
            (rule-< (make-rule (string->symbol (string-append "disjoint-" axiom-name "-<-rule"))
                               #f
                               (make-rule-head (list 'not class-argument))
                               (make-rule-body (list (string->symbol axiom-name) 
                                                     (string->symbol (string-append "?" (apply-namespaces axiom-name namespaces)))))))
            (rule-> (make-rule (string->symbol (string-append "disjoint-" axiom-name "->-rule"))
                               #f
                               (make-rule-head (list 'not (list (string->symbol axiom-name) 
                                                                (string->symbol (string-append "?" (apply-namespaces axiom-name namespaces))))))
                               (make-rule-body class-argument))))
       (if (literal? class-argument)
           (list rule-< rule->)
           (list rule->))))
   
   (define (class-description->formula axiom-name class-desc)  
     (let* ((rdf-id ((sxpath "@rdf:ID/text()" namespaces) class-desc))
            (rdf-about ((sxpath "@rdf:about/text()" namespaces) class-desc))
            ;(rdf-resource ((sxpath "@rdf:resource/text()" namespaces) class-desc))
            ;(rdf-type ((sxpath "@rdf:type/text()" namespaces) class-desc))          
            (class-name (cond ((not (null? rdf-id)) (car rdf-id))
                              ((not (null? rdf-about)) (text->name rdf-about))
                              ;((not (null? rdf-resource)) (string-trim (car rdf-resource) #\#))
                              (else #f)))
            (enumerations (get-class-enumerations class-desc))          
            (intersections (get-class-intersections class-desc))
            (unions (get-class-unions class-desc))
            (complements (get-class-complements class-desc)))     
       (cond (class-name (list (string->symbol class-name) (string->symbol (string-append "?" (apply-namespaces axiom-name namespaces)))))
             ((not (null? enumerations)) (enumeration->formula axiom-name (car enumerations)))           
             ((not (null? intersections)) (intersection->formula axiom-name (car intersections)))
             ((not (null? unions)) (union->formula axiom-name (car unions)))
             ((not (null? complements)) (complement->formula axiom-name (car complements)))
             (else (error "class-description->formula" "no valid class description found" class-desc)))))
   
   (define (class-restriction->formula axiom-name class-restr)
     (let* ((prop (text->name ((sxpath "owl:onProperty/@rdf:resource/text()" namespaces) class-restr)))
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
                                   (list (string->symbol (text->name rdf-resource))
                                         (string->symbol (string-append "?" (apply-namespaces prop namespaces)))))
                                  ((not (null? owl-classes)) (class-description->formula prop (car owl-classes)))
                                  ((not (null? restrictions)) (class-restriction->formula prop (car restrictions)))
                                  (else (error "some-values-restriction->rule" "no class description found" restriction)))))
       (list 'and 
             (list (string->symbol prop)
                   (string->symbol (string-append "?" (apply-namespaces axiom-name namespaces)))
                   (string->symbol (string-append "?" (apply-namespaces prop namespaces))))
             class-argument)))
   
   (define (has-value-restriction->rule axiom-name prop restriction)
     (let* ((rdf-resource ((sxpath "@rdf:resource/text()" namespaces) restriction))
            (data-value ((sxpath "/text()" '()) restriction))
            (value (cond ((not (null? rdf-resource)) (string->symbol (text->name rdf-resource)))
                         ((not (null? data-value)) (string->symbol (car data-value))))))          
       (list (string->symbol prop) (string->symbol (string-append "?" (apply-namespaces axiom-name namespaces))) value)))
   
   
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
     (let* ((rdf-resource ((sxpath "@rdf:resource/text()" namespaces) comp))
            (owl-classes (get-owl-classes comp))
            (restrictions (get-class-property-restrictions comp))
            (class-arguments (cond ((not (null? rdf-resource)) (list (string->symbol (text->name rdf-resource))
                                                                     (string->symbol (string-append "?" (apply-namespaces axiom-name namespaces)))))
                                   ((not (null? owl-classes)) (class-description->formula axiom-name (car owl-classes)))
                                   ((not (null? restrictions)) (class-restriction->formula axiom-name (car restrictions)))
                                   (else (error "complement->formula" "no class description found" comp)))))
       (list 'not class-arguments)))
   
   ; ----------------------
   ; property-conversion
   ; ----------------------
   
   (define (property-axiom->rules axiom)
     (let* ((rdf-id ((sxpath "@rdf:ID/text()" namespaces) axiom))
            (rdf-about ((sxpath "@rdf:about/text()" namespaces) axiom))         
            (property-name (cond ((not (null? rdf-id)) (car rdf-id))
                                 ((not (null? rdf-about)) (text->name rdf-about))
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
            (subproperty-rules (map (lambda (s) (subproperty->rule property-name s)) subproperties))
            (domain-rules (map (lambda (d) (domain->rule property-name d)) domains))
            (range-rules (map (lambda (r) (range->rule property-name r)) ranges))
            (equiv-rules (flatmap (lambda (e) (equivalent-property->rules property-name e)) equiv-props))
            (inverse-rules (flatmap (lambda (i) (inverse-property->rules property-name i)) inverse-props))          
            (type-rules (filter (lambda (x) (not (null? x))) (map (lambda (t) (prop-type->rule property-name t)) prop-types)))            
            )
       (add-property property-name)
       (append subproperty-rules domain-rules range-rules equiv-rules inverse-rules type-rules)))
   
   (define (subproperty->rule property-name sub-prop)
     (let* ((rdf-resource (text->name ((sxpath "@rdf:resource/text()" namespaces) sub-prop)))
            (rule-head (list (string->symbol rdf-resource)
                             (string->symbol (string-append "?" (apply-namespaces property-name namespaces) "-1"))
                             (string->symbol (string-append "?" (apply-namespaces property-name namespaces) "-2"))))
            (rule-body (list (string->symbol property-name)
                             (string->symbol (string-append "?" (apply-namespaces property-name namespaces) "-1"))
                             (string->symbol (string-append "?" (apply-namespaces property-name namespaces) "-2")))))
       (make-rule (string->symbol (string-append property-name "-subproperty-rule"))
                  #f
                  (make-rule-head rule-head)
                  (make-rule-body rule-body))))
   
   (define (domain->rule property-name domain)
     (let* ((rdf-resource ((sxpath "@rdf:resource/text()" namespaces) domain))
            (var1 (string->symbol (string-append "?" (apply-namespaces property-name namespaces) "-1")))
            (var2 (string->symbol (string-append "?" (apply-namespaces property-name namespaces) "-2")))
            (owl-classes (get-owl-classes domain))
            (restrictions (get-class-property-restrictions domain))
            (rule-head (cond ((not (null? rdf-resource)) (list (string->symbol (text->name rdf-resource))
                                                               var1))
                             ((not (null? owl-classes)) (class-description->formula (string-append property-name "-1") (car owl-classes)))
                             ((not (null? restrictions)) (class-restriction->formula (string-append property-name "-1") (car restrictions)))
                             (else (error "domain->rule" "no class description found" domain))))          
            (rule-body (list (string->symbol property-name) var1 var2)))
       (make-rule (string->symbol (string-append property-name "-domain-rule"))
                  #f
                  (make-rule-head rule-head)
                  (make-rule-body rule-body))))
   
   
   (define (range->rule property-name range)
     (let* ((rdf-resource ((sxpath "@rdf:resource/text()" namespaces) range))
            (var1 (string->symbol (string-append "?" (apply-namespaces property-name namespaces) "-1")))
            (var2 (string->symbol (string-append "?" (apply-namespaces property-name namespaces) "-2")))
            (owl-classes (get-owl-classes range))
            (restrictions (get-class-property-restrictions range))
            (rule-head (cond ((not (null? rdf-resource)) (list (string->symbol (text->name rdf-resource))
                                                               var2))
                             ((not (null? owl-classes)) (class-description->formula (string-append property-name "-2") (car owl-classes)))
                             ((not (null? restrictions)) (class-restriction->formula (string-append property-name "-2") (car restrictions)))
                             (else (error "range->rule" "no class description found" range))))          
            (rule-body (list (string->symbol property-name) var1 var2)))
       (make-rule (string->symbol (string-append property-name "-range-rule"))
                  #f
                  (make-rule-head rule-head)
                  (make-rule-body rule-body))))
   
   (define (equivalent-property->rules property-name equiv-prop)
     (let* ((rdf-resource (string->symbol (text->name ((sxpath "@rdf:resource/text()" namespaces) equiv-prop))))
            (var1 (string->symbol (string-append "?" (apply-namespaces property-name namespaces) "-1")))
            (var2 (string->symbol (string-append "?" (apply-namespaces property-name namespaces) "-2")))
            (prop (string->symbol property-name))
            (stmt1 (list prop var1 var2))
            (stmt2 (list rdf-resource var1 var2)))
       (list (make-rule (string->symbol (string-append property-name "-equiv->-rule"))
                        #f
                        (make-rule-head stmt1)
                        (make-rule-head stmt2))
             (make-rule (string->symbol (string-append property-name "-equiv-<-rule"))
                        #f
                        (make-rule-head stmt2)
                        (make-rule-head stmt1)))))
   
   (define (inverse-property->rules property-name inverse-prop)
     (let* ((rdf-resource (string->symbol (text->name ((sxpath "@rdf:resource/text()" namespaces) inverse-prop))))
            (var1 (string->symbol (string-append "?" (apply-namespaces property-name namespaces) "-1")))
            (var2 (string->symbol (string-append "?" (apply-namespaces property-name namespaces) "-2")))
            (prop (string->symbol property-name))
            (stmt1 (list prop var1 var2))
            (stmt2 (list rdf-resource var2 var1)))
       (list (make-rule (string->symbol (string-append property-name "-inverse->-rule"))
                        #f
                        (make-rule-head stmt1)
                        (make-rule-head stmt2))
             (make-rule (string->symbol (string-append property-name "-inverse-<-rule"))
                        #f
                        (make-rule-head stmt2)
                        (make-rule-head stmt1)))))
   
   (define (prop-type->rule property-name prop-type)
     (let* ((rdf-resource ((sxpath "@rdf:resource/text()" namespaces) prop-type))
            (var1 (string->symbol (string-append "?" (apply-namespaces property-name namespaces) "-1")))
            (var2 (string->symbol (string-append "?" (apply-namespaces property-name namespaces) "-2")))
            (var3 (string->symbol (string-append "?" (apply-namespaces property-name namespaces) "-3")))
            (transitive? (and (not (null? rdf-resource))
                              (string=? (resolve-namespaces "owl:TransitiveProperty" namespaces) (car rdf-resource))))
            (symmetric? (and (not (null? rdf-resource))
                             (string=? (resolve-namespaces "owl:SymmetricProperty" namespaces) (car rdf-resource))))
            (functional? (and (not (null? rdf-resource))
                              (string=? (resolve-namespaces "owl:FunctionalProperty" namespaces) (car rdf-resource))))
            (inverse-functional? (and (not (null? rdf-resource))
                                      (string=? (resolve-namespaces "owl:InverseFunctionalProperty" namespaces) (car rdf-resource)))))
       (cond (transitive? (make-rule (string->symbol (string-append property-name "-transitive-rule"))
                                     #f
                                     (make-rule-head (list (string->symbol property-name)var1 var3))
                                     (make-rule-body (list 'and
                                                           (list (string->symbol property-name)var1 var2)
                                                           (list (string->symbol property-name)var2 var3)))))
             (symmetric? (make-rule (string->symbol (string-append property-name "-symmetric-rule"))
                                    #f
                                    (make-rule-head (list (string->symbol property-name)var2 var1))
                                    (make-rule-body (list (string->symbol property-name)var1 var2))))
             (else (if *debug*
                       (begin (display "prop-type->rule: only symmetric and transitive types supported")
                              (newline)
                              (display "found: ")
                              (display prop-type)
                              (newline)))
                   '()))))
   
   (define (transitive-property-axiom->rules axiom)
     (let* ((rdf-id ((sxpath "@rdf:ID/text()" namespaces) axiom))
            (rdf-about ((sxpath "@rdf:about/text()" namespaces) axiom))
            ;(rdf-type ((sxpath "@rdf:type/text()" namespaces) axiom))          
            (property-name (cond ((not (null? rdf-id)) (car rdf-id))
                                 ((not (null? rdf-about)) (text->name rdf-about))
                                 (else (begin (display axiom)
                                              (newline)
                                              (error "transitive-property-axiom->rule" "no property name found" axiom)))))
            (var1 (string->symbol (string-append "?" (apply-namespaces property-name namespaces) "-1")))
            (var2 (string->symbol (string-append "?" (apply-namespaces property-name namespaces) "-2")))
            (var3 (string->symbol (string-append "?" (apply-namespaces property-name namespaces) "-3")))
            (property-rules (property-axiom->rules axiom))
            (transitive-rule (make-rule (string->symbol (string-append property-name "-transitive-rule"))
                                        #f
                                        (make-rule-head (list (string->symbol property-name)var1 var3))
                                        (make-rule-body (list 'and
                                                              (list (string->symbol property-name)var1 var2)
                                                              (list (string->symbol property-name)var2 var3))))))
       (append property-rules (list transitive-rule))))
   
   (define (symmetric-property-axiom->rules axiom)
     (let* ((rdf-id ((sxpath "@rdf:ID/text()" namespaces) axiom))
            (rdf-about ((sxpath "@rdf:about/text()" namespaces) axiom))
            ;(rdf-type ((sxpath "@rdf:type/text()" namespaces) axiom))          
            (property-name (cond ((not (null? rdf-id)) (car rdf-id))
                                 ((not (null? rdf-about)) (text->name rdf-about))
                                 (else (begin (display axiom)
                                              (newline)
                                              (error "symmetric-property-axiom->rules" "no property name found" axiom)))))
            (var1 (string->symbol (string-append "?" (apply-namespaces property-name namespaces) "-1")))
            (var2 (string->symbol (string-append "?" (apply-namespaces property-name namespaces) "-2")))
            (property-rules (property-axiom->rules axiom))
            (symmetric-rule (make-rule (string->symbol (string-append property-name "-symmetric-rule"))
                                       #f
                                       (make-rule-head (list (string->symbol property-name)var2 var1))
                                       (make-rule-body (list (string->symbol property-name)var1 var2)))))
       (append property-rules (list symmetric-rule))))
   
   
   ; ----------------------
   ; individual-conversion
   ; ----------------------
   
   (define (description->rules desc)
     (let* ((rdf-id ((sxpath "@rdf:ID/text()" namespaces) desc))
            (rdf-about((sxpath "@rdf:about/text()" namespaces) desc))
            (individual-name (cond ((not (null? rdf-id)) (car rdf-id))
                                    ((not (null? rdf-about)) (text->name rdf-about))
                                    (else (error "description->rules" "no individual-name found" desc))))
            (types (get-types desc))
            (prop-values (get-property-values desc))
            (type-rules (map member-type->rule types))
            (prop-value-rules (map property-value->rule prop-values)))
;       (newline)
;       (display "description (")
;       (display individual-name)
;       (display "): ")
;       (newline)
;       (display "types: ")
;       (display types)
;       (newline)
;       (display "prop-values: ")
;       (display prop-values)
;       (newline)
       '()))
   
   (define (class-member->rules class-member)
     (let* ((class-name (resolve-namespaces (car class-member) namespaces))
            (member (cdr class-member))
            (rdf-id ((sxpath "@rdf:ID/text()" namespaces) member))
            (rdf-about((sxpath "@rdf:about/text()" namespaces) member))
            (individual-name (cond ((not (null? rdf-id)) (car rdf-id))
                                    ((not (null? rdf-about)) (text->name rdf-about))
                                    (else (symbol->string (gensym "individual-")))))
            (types (get-types member))
            (prop-values (get-property-values member))
            (type-rules (map member-type->rule types))
            (prop-value-rules (map property-value->rule prop-values)))
;       (newline)
;       (display "class-member (")
;       (display individual-name)
;       (display "): ")
;       (newline)
;       (display "class: ")
;       (display class-name)
;       (newline)
;       (display "types: ")
;       (display types)
;       (newline)
;       (display "prop-values: ")
;       (display prop-values)
;       (newline)
       '()))
   
   (define (member-type->rule type)
     '())
   
   (define (property-value->rule property-value)
     (let ((property-name (resolve-namespaces (car property-value) namespaces))
           (prop-value (cdr property-value)))
;       (display "property-value")
;       (newline)
;       (display "property: ")
;       (display property-name)
;       (newline)
;       (display "value: ")
;       (display prop-value)
;       (newline)
       '()))
   
   
   ; -------------------------
   ; UTILS
   ; -------------------------
   
   (define (flatmap f l) (apply append (map f l)))
   
   (define (one-of-prefixes? p s)
     (if (null? p)
         #f
         (or (string-prefix? (car p) s)
             (one-of-prefixes? (cdr p) s))))
   
   (define (resolve-namespaces s ns)
     (fold-left resolve-namespace s ns))
   
   ; "owl:foo" -> "http://www.w3.org/2002/07/owl#foo"
   (define (resolve-namespace s ns)
     (let ((prefix (string-append (symbol->string (car ns)) ":"))
           (uri (cdr ns)))
       (if (string-prefix? prefix s)
           (string-replace s uri 0 (string-length prefix))
           s)))
   
   (define (apply-namespaces s ns)
     (fold-left apply-namespace s ns))
   
   ; "http://www.w3.org/2002/07/owl#foo" -> "owl:foo"
   (define (apply-namespace s ns)
     (let ((prefix (string-append (symbol->string (car ns)) ":"))
           (uri (cdr ns)))
       (if (string-prefix? uri s)
           (string-replace s prefix 0 (string-length uri))
           s)))
   
   (define (text->name txt)
     (if (one-of-prefixes? prefixes (car txt))
         (string-trim (car txt) #\#)
         (resolve-namespaces (string-append "base:" (string-trim (car txt) #\#)) namespaces)))
   
   
   ; -------------------------
   ; Test Code
   ; -------------------------
   
   
   #;(define sxml '(*TOP* (doc (^ (foo "foo"))
                               (title "Hello world")
                               (title "foo bar"))))
   
   ;(define doc ((sxpath "doc" '()) sxml))
   
   ;(define titles ((sxpath "title" '()) doc))
   
   ;(define hund (ssax:dtd-xml->sxml (open-input-resource "C:\\Users\\stb\\Desktop\\Hundeformular.owl") namespaces))
   
   ;(define classes ((sxpath "rdf:RDF/owl:Class" namespaces) hund))
   
   ;(define class1 (car classes))
   
   ;(define rb (owl-import "C:\\Users\\stb\\Desktop\\Hundeformular.owl"))
   
   )
 