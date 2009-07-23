#!r6rs

(library
 (carneades owl)
 
 (export import-owl)
 
 (import (rnrs)
         (carneades rule)
         (carneades system)
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
     (display "class-rules: ")
     (display class-rules)
     (newline)
     (display "object-property-rules: ")
     (display object-property-rules)
     (newline)
     (display "data-property-rules: ")
     (display data-property-rules)
     (newline)
     (add-rules empty-rulebase (append class-rules object-property-rules data-property-rules))))
 
 (define (class-axiom->rules axiom)
   (let* ((rdf-id ((sxpath "@rdf:ID/text()" namespaces) axiom))
          (rdf-about ((sxpath "@rdf:about/text()" namespaces) axiom))
          (axiom-name (cond ((not (null? rdf-id)) (car rdf-id))
                            ((not (null? rdf-about)) (string-trim (car rdf-about) #\#))
                            (else (begin (display axiom)
                                         (newline)
                                         (raise "no class name found")))))
          (subclasses (get-subclasses axiom))
          (equivalents (get-equivalent-classes axiom))
          (disjoints (get-disjoint-classes axiom))
          (subclass-rules (map (lambda (s) (subclass-axiom->rule axiom-name s)) subclasses))
          (equivalent-rules (flatmap (lambda (s) (equivalent-axiom->rules axiom-name s)) equivalents))
          (disjoint-rules (flatmap (lambda (s) (disjoint-axiom->rules axiom-name s)) disjoints)))
     (append subclass-rules equivalent-rules disjoint-rules)))
 
 (define (subclass-axiom->rule axiom-name sxml)
   (let ((superclass '(superclass ?x)))
     (make-rule (string->symbol (string-append "subclass-" axiom-name "-rule"))
                #f
                (make-rule-head superclass)
                (make-rule-body (list (string->symbol axiom-name) '?x)))))

 
 (define (equivalent-axiom->rules axiom-name sxml)
   (let ((equiv-class '(equi-class ?x)))
     (list (make-rule (string->symbol (string-append "equivalent-" axiom-name "-<-rule"))
                      #f
                      (make-rule-head equiv-class)
                      (make-rule-body (list (string->symbol axiom-name) '?x)))
           (make-rule (string->symbol (string-append "equivalent-" axiom-name "->-rule"))
                      #f
                      (make-rule-head (list (string->symbol axiom-name) '?x))
                      (make-rule-body equiv-class)))))
 
 (define (disjoint-axiom->rules axiom-name sxml)
   (let ((disjoint-class '(disjoint ?x)))
     (list (make-rule (string->symbol (string-append "disjoint-" axiom-name "-<-rule"))
                      #f
                      (make-rule-head (list 'not disjoint-class))
                      (make-rule-body (list (string->symbol axiom-name) '?x)))
           (make-rule (string->symbol (string-append "disjoint-" axiom-name "-<-rule"))
                      #f
                      (make-rule-head (list 'not (list (string->symbol axiom-name) '?x)))
                      (make-rule-body disjoint-class)))))
 
 (define (property-axiom->rules axiom)
   (let* ((rdf-id ((sxpath "@rdf:ID/text()" namespaces) axiom))
          (rdf-about ((sxpath "@rdf:about/text()" namespaces) axiom))         
          (property-name (cond ((not (null? rdf-id)) (car rdf-id))
                               ((not (null? rdf-about)) (string-trim (car rdf-about) #\#))
                               (else (begin (display axiom)
                                            (newline)
                                            (raise "no class name found")))))
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
                                            (raise "no class name found")))))
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
                                            (raise "no class name found")))))
          (property-rules (property-axiom->rules axiom))
          (symmetric-rule (make-rule (string->symbol (string-append property-name "-symmetric-rule"))
                                     #f
                                     (make-rule-head (list (string->symbol property-name)'?y '?x))
                                     (make-rule-body (list (string->symbol property-name)'?x '?y)))))
     (append property-rules (list symmetric-rule))))
 

 (define (class-description->formula class-desc)
   (let* ((rdf-id ((sxpath "@rdf:ID/text()" namespaces) class-desc))
          (rdf-about ((sxpath "@rdf:about/text()" namespaces) class-desc))
          ;(rdf-type ((sxpath "@rdf:type/text()" namespaces) class-desc))          
          (class-name (cond ((not (null? rdf-id)) (car rdf-id))
                               ((not (null? rdf-about)) (string-trim (car rdf-about) #\#))
                               (else #f)))
          (unions (get-class-unions class-desc))
          (intersections (get-class-intersections class-desc)))
     '()))
 
 ; -------------------------
 ; UTILS
 ; -------------------------
 
 (define (flatmap f l) (apply append (map f l)))
   

 
 ; -------------------------
 ; Test Code
 ; -------------------------
 
  
 (define sxml '(*TOP* (doc (^ (foo "foo"))
                           (title "Hello world")
                           (title "foo bar"))))
 
 (define doc ((sxpath "doc" '()) sxml))
 
 (define titles ((sxpath "title" '()) doc))
 
 (define hund (ssax:dtd-xml->sxml (open-input-resource "C:\\Users\\Silvi\\Desktop\\OWL\\Hundeformular.owl") '()))
 
 (define classes ((sxpath "rdf:RDF/owl:Class" namespaces) hund))
 
 (define class1 (car classes))
 
 (define rb (import-owl "C:\\Users\\Silvi\\Desktop\\OWL\\Hundeformular.owl"))
 
 )
