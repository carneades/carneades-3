#!r6rs

(library
 
 (carneades lkif2 lkif2-export)
 
 (export)
 
 (import (rnrs)         
         (carneades lkif2 lkif2-base)
         
         (carneades lkif2 lkif2-import) ; only for testing
         
         (prefix (carneades argument) argument:)
         (carneades rule)
         (prefix (carneades table) table:)
         (carneades unify)
         (carneades system)
         (carneades lib xml sxml serializer))
 
 
 ; --------------------------------
 ; misc functions
 
 ; generates a new ID
 (define (new-id prefix)
   (symbol->string (gensym prefix)))
 
 ; functor? is needed to seperate Expressions in LKIF from Atoms.
 ; Expressions seen as s-expressions look the same compared to
 ; atoms seen as s-expressions. Therefor, if the predicate is a
 ; functor (in the sense of functor?) the s-expression is a LKIF-
 ; Expression.
 ;
 ; TODO:
 ;        - expand the list of functors
 ;        - is there a simpler way to define functor? ?
 (define (functor? f)
   (or (eq? f '+)
       (eq? f '-)
       (eq? f '*)
       (eq? f '/)
       (eq? f 'list)
       (eq? f 'cons)))
 
 
 ; --------------------------------
 ; export function
 
 ; lkif-export: struct:lkif-data file-path -> void
 (define (lkif-export export-data path)
   (let ((sxml-obj (lkif-data->sxml export-data)))
     (apply srl:sxml->xml (cons sxml-obj path))))
 
 
 ; --------------------------------
 ; Conversion functions
 
 
 ; element->sxml: element-name element-value -> sxml
 (define (element->sxml name value)
   (list name value))
 
 ; elements->attributes: (list-of sxml) -> sxml
 (define (elements->attributes l)
   (cons '^ l))
 
 ; lkif-data->sxml: struct:lkif-data -> sxml
 (define (lkif-data->sxml data)
   (let ((sxml (list (lkif-data->sources data) 
                     (lkif-data->theory data) 
                     (lkif-data->argument-graphs data))))
     (cons 'lkif sxml))) 
 
 ; lkif-data->sources: struct:lkif-data -> sxml
 (define (lkif-data->sources data)
   (let ((sources (lkif-data-sources data)))
     (cons 'sources (map source->sxml sources))))
 
 ; source->sxml: struct:source -> sxml
 (define (source->sxml source)
   (list 'source
         (elements->attributes (list (element->sxml 'uri (source-uri source))
                                     (element->sxml 'element (source-element source))))))
 
 ; lkif-data->theory: struct:lkif-data -> sxml
 (define (lkif-data->theory data)
   (let ((axioms (context->axioms (lkif-data-context data)))
         (rules (rulebase->rules (lkif-data-rulebase data))))
     (list 'theory axioms rules)))
 
 ; context->axioms: context -> sxml
 (define (context->axioms context)
   (let ((axioms (table:keys (argument:context-status context))))
     (cons 'axioms (map axiom->sxml axioms))))
 
 ; axiom->sxml: axiom -> sxml
 (define (axiom->sxml a)
   (list 'axiom
         (elements->attributes (list (element->sxml 'id (new-id "a"))))
         (wff->sxml a)))
 
 ; wff->sxml: wff -> sxml
 (define (wff->sxml f)
   (cond ((string? f) (list 's f))
         ((symbol? f) (list 's f))
         ((pair? f) (case (car f)
                      ((not and or if iff) (list (car f) (map wff->sxml (cdr f))))
                      ((assuming) (let ((s (wff->sxml (cadr f))))
                                    (list (car s)
                                          (elements->attributes (list (element->sxml 'assumable "true")))
                                          (cadr s))))
                      ((unless) (append (list 'not
                                            (elements->attributes (list (element->sxml 'exception "true"))))
                                      (map wff->sxml (cdr f))))
                      (else ; the wff is an atom
                       (append (list 's
                                   (elements->attributes (list (element->sxml 'pred (symbol->string (car f))))))
                             (map text/term->sxml (cdr f))))))
         (else (display "Error: unknown wff - ")
               (display f)
               (newline)
               '())))
 
 ; text/term->sxml: any -> sxml
 ;
 ; text/term->sxml converts the arguments of an predicate
 ; to the according sxml-object.
 ; An argument can be a text or a term.
 ; Texts are left as they are.
 ; For terms there will be no difference between constants and
 ; individuals as there is no correspondence for individuals
 ; in carneades and even in the lkif-import the additional
 ; information for individuals is lost. So every symbol will
 ; be a constant
 ; TODO:
 ;       - can constants be strings?
 ;       - and if, is a string a text or a constant?
 (define (text/term->sxml t)
   (cond ((string? t) t)
         ((variable? t) (let ((s (symbol->string t)))
                          (element->sxml 'v (substring s 1 (string-length s)))))
         ((symbol? t) (list 'c t))
         ((pair? t) (let ((p (car t)))
                      (cond ((functor? p) (append (element->sxml 'expr
                                                      (elements->attributes (list (element->sxml 'functor (symbol->string p)))))
                                                (map text/term->sxml (cdr t)))) ; maybe write a term->sxml function
                            (else
                             (append (list 's
                                         (elements->attributes (list (element->sxml 'pred (symbol->string p)))))
                                   (map text/term->sxml (cdr t)))))))
         (else (display "Error: unknown text/term - ")
               (display t)
               (newline)
               '())))
 
 ; rulebase->rules: rulebase -> sxml
 (define (rulebase->rules rb)
   (cons 'rules (map rule->sxml (rulebase-rules rb))))
 
 ; rule->sxml: struct:rule -> sxml
 ; TODO: - if list-length = 1 in body than don't use 'or / 'and
 ;       - what about conjunction in head?
 (define (rule->sxml r)
   (list 'rule
         (elements->attributes (list (element->sxml 'id (symbol->string (rule-id r)))
                                     (element->sxml 'strict (if (rule-strict r)
                                                                "true"
                                                                "false"))))
         (cons 'head (map wff->sxml (rule-head r)))
         (cons 'body (cons 'or (map (lambda (l) (cons 'and (map wff->sxml l))) (rule-body r))))))
                                     
     
 ; lkif-data->argument-graphs: struct:lkif-data -> sxml
 (define (lkif-data->argument-graphs data)
   data)
 
 
 ; ----------------------------------
 ; Testing Code
 
 (define import-data (lkif-import "C:\\test2.xml")) 
 
 (define s (lkif-data->sources import-data)) 
 
 (define c (lkif-data-context import-data))
 
 (define a (context->axioms c))
 
 (define r (rulebase->rules (lkif-data-rulebase import-data)))
 
 )
     