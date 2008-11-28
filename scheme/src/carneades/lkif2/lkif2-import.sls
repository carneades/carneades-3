#!r6rs

(library
 
 (carneades lkif2 lkif2-import)
 
 (export lkif-import
         )
 
 (import (rnrs)
         (carneades lkif2 lkif2-base)
         (carneades rule)
         (prefix (carneades argument) argument:)
         (prefix (carneades statement) statement:)
         (carneades lib match)
         (prefix (carneades table) table:)
         (carneades lib xml sxml xpath-context_xlink))
 
 
 
 
 
 
 ;---------
 ; records for internal use
 
 ; theory in lkif2
 (define-record-type theory
   (fields id
           imports
           axioms
           rules))
 
 ; argument-graph in lkif2
 (define-record-type lkif-argument-graph
   (fields id
           title
           main-issue-id
           statements
           arguments))
 

 ; --------------------------------
 ; Type checkers 
 ; check for object-types as defined in the lkif2 grammar
 ; (only keyword is checked; length and argument-types are ignored)
 
 (define (element-type? element type)
   (and (pair? element)
        (eq? (car element) type)))
 
 (define (element-checker* type)
   (lambda (e) (element-type? e type)))
 
 (define-syntax element-types?
   (syntax-rules ()
     ((_ e t) (element-type? e t))
     ((_ e t1 ...) (or (element-type? e t1)
                       ...))))
 
 (define-syntax element-checker
   (syntax-rules ()
;     ((_ t) (element-checker* t))
     ((_ t1 ...) (lambda (e) (element-types? e t1 ...)))))
 
 
 ; --------------------------------
 ; Import functions 
 
 
 ; lkif-import: file-path -> lkif-data
 (define (lkif-import path)
   (let ((lkif-body (get-lkif (get-document path))))
     (let ((sources (read-sources lkif-body))
           (theory (read-theory lkif-body))
           (arg-graphs (read-argument-graphs lkif-body)))
       (let ((context (lkif-axioms->context (theory-axioms theory)))
             (rb (lkif-rules->rulebase (theory-rules theory)))
             (stages (map argument-graph->stage arg-graphs)))
         (make-lkif-data sources context rb stages)))))
 
 
 ; --------------------------------
 ; Selectors
 ; functions to extract the content from its context
 
 ; get-element: lkif-element proc any -> any
 (define (get-element element type)
   (or (find (element-checker type) element)
       '()))
 
 ; get-elements: lkif-element any -> (list-of any)
 (define (get-elements element type)
   (filter (element-checker type) element))
 
 ; get-attributes: lkif-element -> (list-of lkif-attributes)
 (define (get-attributes element)
   (get-element element '^))
 
 ; get-attribute: lkif-element any -> any
 (define (get-attribute element type)
   (get-element (get-attributes element) type))
           
 ; get-document: file-path -> sxml-document
 (define (get-document path)
   (sxml:document path '()))
 
 ; get-lkif: sxml-document -> lkif-body
 (define (get-lkif doc)
   (match doc
     (('*TOP* ('*PI* . p) ... ('lkif . b)) b)))
 
 ; get-attribute-value: any any -> any
 ; returns the value of an attribute if it is any,
 ; else returns default
 (define (get-attribute-value attribute default)
   (if (pair? attribute)
       (cadr attribute)
       default))
 
 ; get-statement: xsd:ID table -> struct:statement
 (define (get-statement sid tbl)
   (table:lookup tbl sid (lambda ()
                           (display "Error: Statement-ID not found: ")
                           (display sid)
                           (newline)
                           #f)))
 
 ; get-conclusion-statement: lkif-conclusion table -> struct:statement
 (define (get-conclusion-statement c tbl)
   (let ((sid (get-attribute-value (get-attribute c 'statement) "")))
     (get-statement sid tbl)))
 
  
 ; get-text/term: atom -> (list-of text/term)
 (define (get-text/term* a)
   (let* ((s (cdr a)))
     (if (and (pair? s)
              (pair? (car s))
              (eq? (caar s) '^))
         (cdr s)
         s)))
 
 
 ; --------------------------------
 ; Constructors 
 
 
; source-to-record: lkif-source -> struct:source
 (define (source-to-record s)
   (let ((uri (get-attribute-value (get-attribute s 'uri) "")) 
         (element (get-attribute-value (get-attribute s 'element) "")))
     (make-source element uri)))
 
 ; theory-to-record: lkif-theory -> struct:theory
 (define (theory-to-record t)
   (let ((id (get-attribute-value (get-attribute t 'id) ""))
         (imports (get-elements (get-element t 'imports) 'import))
         (axioms (get-elements (get-element t 'axioms) 'axiom))
         (rules (get-elements (get-element t 'rules) 'rule)))
     (make-theory id imports axioms rules)))
 
 ; argument-graph-to-record: lkif-argument-graph -> struct:argument-graph
 (define (argument-graph-to-record ag)
   (let ((id (get-attribute-value (get-attribute ag 'id) ""))
         (title (get-attribute-value (get-attribute ag 'title) ""))
         (main-issue-id (get-attribute-value (get-attribute ag 'main-issue) ""))
         (statements (map statement-to-record (get-elements (get-element ag 'statements) 'statement)))
         (arguments (get-elements (get-element ag 'arguments) 'argument)))
     (make-lkif-argument-graph id title main-issue-id statements arguments)))
 
 ; lkif-argument-graph->stage: lkif-argument-graph -> struct:stage
 (define (lkif-argument-graph->stage ag)
   (call-with-values (lambda () (lkif-argument-graph->argument-graph/context ag))
                     (lambda (a c) (make-stage a c))))
 
 ; statement-to-record: lkif-statement -> struct:statement
 (define (statement-to-record  s)
   (let ((id (get-attribute-value (get-attribute s 'id) ""))
         (value (get-attribute-value (get-attribute s 'value) "unknown"))
         (assumption (get-attribute-value (get-attribute s 'assumption) "false"))
         (standard (get-attribute-value (get-attribute s 'standard) "BA"))
         (atom (get-element s 's)))
     (make-statement id value assumption standard atom))) 
  
 ; premise-to-record: lkif-premise tbl -> struct:premise
 (define (premise-to-record p tbl)
   (let ((polarity (get-attribute-value (get-attribute p 'polarity) "positive"))
         (exception (get-attribute-value (get-attribute p 'exception) "false"))
         (role (get-attribute-value (get-attribute p 'role) ""))
         (sid (get-attribute-value (get-attribute p 'statement) "")))
     (let ((s (statement->sexpr (get-statement sid tbl)))
           (p (string=? polarity "positive")))
       (if (string=? exception "true")
           (argument:make-exception s p role)
           (if (and (pair? s)
                    (eq? (car s) 'assuming))
               (argument:make-assumption (cadr s) p role)
               (argument:make-ordinary-premise s p role))))))
 
 ; argument-to-record: lkif-argument table -> struct:argument
 (define (argument-to-record a tbl)
   (let ((id (string->symbol (get-attribute-value (get-attribute a 'id) "")))
         (title (string->symbol (get-attribute-value (get-attribute a 'title) "")))
         (direction (string->symbol (get-attribute-value (get-attribute a 'direction) "pro")))
         (scheme (get-attribute-value (get-attribute a 'scheme) ""))
         (weight (string->symbol (get-attribute-value (get-attribute a 'weight) "0.5")))
         (conclusion (let ((c (statement->sexpr (get-conclusion-statement (cdr (get-element a 'conclusion)) tbl))))
                       (if (and (pair? c)
                                (eq? (car c) 'assuming))
                           (cadr c)
                           c)))
         (premises (map (lambda (x) (premise-to-record x tbl)) (get-elements (get-element a 'premises) 'premise))))
     (argument:make-argument id direction conclusion premises scheme)))
 
 


 
 ; --------------------------------
 ; Reader functions 

 
 ; read-sources: lkif-body -> (list-of struct:source)
 (define (read-sources doc-lkif)
   (map source-to-record (get-elements (get-element doc-lkif 'sources) 'source)))
 
 ; read-theory: lkif-body -> struct:theory
 (define (read-theory doc-lkif)
   (theory-to-record (get-element doc-lkif 'theory)))
 
 ; read-argument-graphs: lkif-body -> (list-of struct:argument-graphs) 
 (define (read-argument-graphs doc-lkif)
   (map argument-graph-to-record (get-elements (get-element doc-lkif 'argument-graphs) 'argument-graph)))
 
 
 
 ; --------------------------------
 ; Conversion functions 
 
 
 ; wffs- and term-conversion
 
 (define (lkif-wff->sexpr w)
   (cond ((element-type? w 's) (lkif-atom->sexpr w))
         ((element-type? w 'or) (lkif-or->sexpr w))
         ((element-type? w 'and) (lkif-and->sexpr w))
         ((element-type? w 'not) (lkif-not->sexpr w))
         ((element-type? w 'if) (lkif-if->sexpr w))
         ((element-type? w 'iff) (lkif-iff->sexpr w))
         ((element-type? w 'all) (lkif-all->sexpr w))
         ((element-type? w 'exists) (lkif-exists->sexpr w))
         (else (display "Error: unknown wff - ")
               (display w)
               (newline)
               '())))
 
 (define (lkif-term->sexpr t)
   (cond ((element-type? t 'v) (lkif-variable->sexpr t))
         ((element-type? t 'i) (lkif-individual->sexpr t))
         ((element-type? t 'c) (lkif-constant->sexpr t))
         ((element-type? t 'expr) (lkif-expression->sexpr t))
         ((element-type? t 's) (lkif-atom->sexpr t))
         (else (display "Error: unknown term - ")
               (display t)
               (newline)
               '())))
 
 (define (text/term->format-string t)
   (if (string? t)
       t
       "~a"))
 
 (define (text/term*->format-string tt)
   (fold-left (lambda (s t)
                (string-append s (text/term->format-string t)))
              ""
              tt))       
 
 (define (atom->format-string a)
   (let ((tt (get-text/term* a)))
     (text/term*->format-string tt)))
 
 (define (lkif-atom->sexpr a)
   (let* ((pred (get-attribute-value (get-attribute a 'pred) #f))
          (assumable (get-attribute-value (get-attribute a 'assumable) "none"))
          (text/term* (get-text/term* a)) 
          (term? (element-checker 'v 'i 'c 'expr 's))
          (format-string (text/term*->format-string text/term*)))
     (call-with-values (lambda () (partition term? text/term*))
                       (lambda (terms text)
                         (if (string=? assumable "true")
                             (if pred
                                 (list 'assuming (statement:make-fatom format-string
                                                             (cons (string->symbol pred) (map lkif-term->sexpr terms))))
                                 (list 'assuming (car text)))
                             (if pred
                                 (statement:make-fatom format-string
                                             (cons (string->symbol pred) (map lkif-term->sexpr terms)))
                                 (car text)))))))
 
 (define (lkif-or->sexpr o)
   (let ((assumable (get-attribute-value (get-attribute o 'assumable) #f)))
     (if assumable
         (if (string=? assumable "true")
             (list 'assuming (cons 'or (map lkif-wff->sexpr (cddr o))))
             (cons 'or (map lkif-wff->sexpr (cddr o))))
         (cons 'or (map lkif-wff->sexpr (cdr o))))))
 
 (define (lkif-and->sexpr a)
   (let ((assumable (get-attribute-value (get-attribute a 'assumable) #f)))
     (if assumable
         (if (string=? assumable "true")
             (list 'assuming (cons 'and (map lkif-wff->sexpr (cddr a))))
             (cons 'and (map lkif-wff->sexpr (cddr a))))
         (cons 'and (map lkif-wff->sexpr (cdr a))))))
 
 (define (lkif-not->sexpr n)
   (let ((exception (get-attribute-value (get-attribute n 'exception) "false"))
         (assumable (get-attribute-value (get-attribute n 'assumable) "false")))
     (if (string=? exception "true")
         (list 'unless (lkif-wff->sexpr (car (list-tail n (- (length n) 1)))))
         (if (string=? assumable "true")
             (list 'assuming (list 'not (lkif-wff->sexpr (car (list-tail n (- (length n) 1))))))
             (list 'not (lkif-wff->sexpr (car (list-tail n (- (length n) 1)))))))))
 
 (define (lkif-if->sexpr i)
   (let ((assumable (get-attribute-value (get-attribute i 'assumable) "false")))
         (if (string=? assumable "true")
             (list 'assuming (cons 'if (map lkif-wff->sexpr (list-tail i (- (length i) 2)))))
             (cons 'if (map lkif-wff->sexpr (list-tail i (- (length i) 2)))))))
 
 (define (lkif-iff->sexpr i)
   (let ((assumable (get-attribute-value (get-attribute i 'assumable) "false")))
         (if (string=? assumable "true")
             (list 'assuming (cons 'iff (map lkif-wff->sexpr (list-tail i (- (length i) 2)))))
             (cons 'iff (map lkif-wff->sexpr (list-tail i (- (length i) 2)))))))
 
 (define (lkif-all->sexpr a)
   (display "Error: all quantifier are not supported yet - ")
   (display a)
   '())
 
 (define (lkif-exists->sexpr e)
   (display "Error: existence quantifier are not supported yet - ")
   (display e)
   '())
 
 (define (lkif-variable->sexpr v)
   (string->symbol (string-append "?" (cadr v))))
 
 (define (lkif-individual->sexpr i)
   (string->symbol (get-attribute-value (get-attribute i 'value) "i")))
 
 (define (lkif-constant->sexpr c)
   (let ((c-value (cadr c))
         (c-number (string->number (cadr c))))
     (cond (c-number c-number)
           ((string=? c-value "true") #t)
           ((string=? c-value "false") #f)
           (else (string->symbol c-value)))))
 
 (define (lkif-expression->sexpr e)
   (let ((functor (get-attribute-value (get-attribute e 'functor) "f")))
     (cons (string->symbol functor) (map lkif-term->sexpr (cddr e)))))
 
 
 ; rule conversion
 
 (define (lkif-rule->rule r)
   (let ((id (get-attribute-value (get-attribute r 'id) "r"))
         (strict (get-attribute-value (get-attribute r 'strict) "none"))
         (head (get-element r 'head))
         (body (get-element r 'body)))
     (if (string=? strict "true")
         (make-rule (string->symbol id) 
                    #t
                    (make-rule-head (cons 'and (map lkif-wff->sexpr (cdr head))))
                    (let ((to-body (if (null? body)
                                       '()
                                       (map lkif-wff->sexpr (cdr body)))))
                      (cond ((= (length to-body) 0) '())
                            ((= (length to-body) 1) (make-rule-body (car to-body)))
                            (else (make-rule-body (cons 'or to-body))))))
         (make-rule (string->symbol id) 
                    #f
                    (make-rule-head (cons 'and (map lkif-wff->sexpr (cdr head))))
                    (let ((to-body (if (null? body)
                                       '()
                                       (map lkif-wff->sexpr (cdr body)))))
                      (cond ((= (length to-body) 0) '())
                            ((= (length to-body) 1) (make-rule-body (car to-body)))
                            (else (make-rule-body (cons 'or to-body)))))))))

 
 (define (lkif-rules->rulebase rules)
   (add-rules empty-rulebase (map lkif-rule->rule rules)))
 
 
 ; axiom conversion
 
 (define (lkif-axiom->sexpr a)
   (lkif-wff->sexpr (caddr a)))
 
 (define (lkif-axioms->context a)
   (argument:accept argument:default-context (map lkif-axiom->sexpr a)))
 
 
 ; statement conversion
 
 ; insert-statement: table struct:statement -> table
 (define (insert-statement tbl s)
    (table:insert tbl (statement-id s) s))
 
 ; statements->table: (list-of struct:statement) -> table
 (define (statements->table s)
   (fold-left insert-statement (table:make-table) s))
 
 ; statement->sexpr: struct:statement -> any
 (define (statement->sexpr s)
   (lkif-atom->sexpr (statement-atom s)))
 
 ; statements->context: (list-of struct:statement) -> context
 (define (statements->context statements)
   (let ((scontext (fold-left (lambda (c s)
                                (let ((sexpr (statement->sexpr s)))
                                  (let ((st (if (and (pair? sexpr)
                                                     (eq? (car sexpr) 'assuming))
                                                (cadr sexpr)
                                                sexpr)))
                                (if (string=? (statement-assumption s) "true")
                                    (cond 
                                      ((string=? (statement-value s) "unknown") (argument:state c (list st)))
                                      ((string=? (statement-value s) "true") (argument:accept c (list st)))
                                      ((string=? (statement-value s) "false") (argument:reject c (list st))))
                                    (cond 
                                      ((string=? (statement-value s) "unknown") (argument:question c (list st)))
                                      ((string=? (statement-value s) "true") (argument:accept c (list st)))
                                      ((string=? (statement-value s) "false") (argument:reject c (list st))))))))
                              argument:default-context
                              statements)))
     (fold-left (lambda (c s)
                  (let ((sexpr (statement->sexpr s)))
                    (let ((st (if (and (pair? sexpr)
                                       (eq? (car sexpr) 'assuming))
                                  (cadr sexpr)
                                  sexpr)))
                      (argument:assign-standard
                       c
                       (string->symbol (string-downcase (statement-standard s)))
                       (list st)))))
                scontext
                statements)))
     
 
 ; argument-graph-conversion
 
 ; lkif-argument-graph->argument-graph: struct:lkif-argument-graph -> (struct:argument-graph context)
 (define (lkif-argument-graph->argument-graph/context ag)
   (let* ((statements (lkif-argument-graph-statements ag))
          (tbl (statements->table statements))
          (arguments (map (lambda (x) (argument-to-record x tbl)) (lkif-argument-graph-arguments ag)))
          (ag1 (argument:make-argument-graph (string->symbol (lkif-argument-graph-id ag))
                                             (lkif-argument-graph-title ag)
                                             (if (string=? (lkif-argument-graph-main-issue-id ag) "")
                                               #f
                                               (statement->sexpr (get-statement (lkif-argument-graph-main-issue-id ag) tbl)))
                                             (map (lambda (s)
                                                    (let* ((sexpr (statement->sexpr s)))
                                                      (if (and (pair? sexpr)
                                                               (eq? (car sexpr) 'assuming))
                                                          (cadr sexpr)
                                                          sexpr)))
                                                  statements))))
     (values (argument:assert-arguments ag1 arguments)
             (statements->context statements))))
 
 ; argument-graph->stage: struct:lkif-argument-graph -> struct:stage
 (define (argument-graph->stage ag)
   (call-with-values (lambda () (lkif-argument-graph->argument-graph/context ag))
                     (lambda (a c)
                       (make-stage a c))))
         
 
 


 )