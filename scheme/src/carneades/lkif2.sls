#!r6rs

(library
 
 (carneades lkif2)
 
 (export)
 
 (import (rnrs)
         (prefix (carneades argument) carneades:)
         (carneades lib match)
         (carneades lib xml sxml xpath-context_xlink))
 
 ; --------------------------------
 ; Record definitions
 
 (define-record-type source
   (fields element
           uri))
 
 (define-record-type theory
   (fields id
           imports
           axioms
           rules))
 
 (define-record-type argument-graph
   (fields id
           main-issue
           issues
           statements
           arguments))
 
 ; --------------------------------
 ; Type checkers
 
 (define (object-sources? object)
   (and (pair? object)
        (>= (length object) 2)
        (eq? (car object) 'sources)))
 
 (define (object-theory? object)
   (and (pair? object)
        (>= (length object) 2)
        (eq? (car object) 'theory)))
 
 (define (object-argument-graphs? object)
   (and (pair? object)
        (>= (length object) 2)
        (eq? (car object) 'argument-graphs)))
 
 (define (object-source? object)
   (and (pair? object)
        (= (length object) 2)
        (eq? (car object) 'source)))
 
 (define (object-imports? object)
   (and (pair? object)
        (>= (length object) 2)
        (eq? (car object) 'imports)))
 
 (define (object-axioms? object)
   (and (pair? object)
        (>= (length object) 2)
        (eq? (car object) 'axioms))) 
 
 (define (object-rules? object)
   (and (pair? object)
        (>= (length object) 2)
        (eq? (car object) 'rules))) 
 
 (define (object-id? object)
   (and (pair? object)
        (= (length object) 2)
        (eq? (car object) 'id)))
 
 (define (object-main-issue? object)
   (and (pair? object)
        (= (length object) 2)
        (eq? (car object) 'main-issue)))
 
 (define (object-issues? object)
   (and (pair? object)
        (>= (length object) 2)
        (eq? (car object) 'isuues)))
 
 (define (object-statements? object)
   (and (pair? object)
        (>= (length object) 1)
        (eq? (car object) 'statements)))
 
 (define (object-arguments? object)
   (and (pair? object)
        (>= (length object) 1)
        (eq? (car object) 'arguments)))
 
 ; --------------------------------
 ; Import functions 
 
 (define (import path)
   (sxml:document path '()))
 
 (define (get-lkif doc)
   (match doc
     (('*TOP* ('*PI* . p) ... ('lkif . b)) b)))
 
 ; --------------------------------
 ; Selectors
 
 (define (get-object-sources doc-lkif)
   (let ((s (filter object-sources? doc-lkif)))
     (if (null? s)
         '()
         (cdar s))))
 
 (define (get-object-theory doc-lkif)
   (let ((t (filter object-theory? doc-lkif)))
     (if (null? t)
         '()
         (cdar t))))
 
 (define (get-object-argument-graphs doc-lkif)
   (let ((a (filter object-argument-graphs? doc-lkif)))
     (if (null? a)
         '()
         (cdar a))))
 
 
 (define (get-theory-imports t)
   (let ((i (filter object-imports? t)))
     (if (null? i)
         '()
         (cdar i))))
 
 (define (get-theory-axioms t)
   (let ((a (filter object-axioms? t)))
     (if (null? a)
         '()
         (cdar a))))
 
 (define (get-theory-rules t)
   (let ((r (filter object-rules? t)))
     (if (null? r)
         '()
         (cdar r))))
 
 (define (get-argument-graph-id ag)
   (let ((i (filter object-id? ag)))
     (if (null? i)
         "none"
         (cadr i))))
 
  (define (get-argument-graph-main-issue ag)
   (let ((m (filter object-main-issue? ag)))
     (if (null? m)
         "none"
         (cdar m))))
  
  (define (get-argument-graph-issues ag)
   (let ((i (filter object-issues? ag)))
     (if (null? i)
         '()
         (cdar i))))
  
  (define (get-argument-graph-statements ag)
   (cdar (filter object-statements? ag)))
    
  (define (get-argument-graph-arguments ag)
   (cdar (filter object-arguments? ag)))

 
 ; --------------------------------
 ; Constructors 
 
 (define (source-to-record s)
   (let ((uri (cadr (cadadr s))) (element (cadar (cddadr s))))
     (make-source uri element)))
 
 (define (theory-to-record t)
   (let ((id (car (cdadar t))) (imports (get-theory-imports t)) (axioms (get-theory-axioms t)) (rules (get-theory-rules t)))
     (make-theory id imports axioms rules)))
 
 (define (argument-graph-to-record ag)
   (let ((id (get-argument-graph-id ag))
         (main-issue (get-argument-graph-main-issue ag))
         (issues (get-argument-graph-issues ag))
         (statements (get-argument-graph-statements ag))
         (arguments (get-argument-graph-arguments ag)))
     (make-argument-graph id main-issue issues statements arguments)))
 
 ; --------------------------------
 ; Reader functions 
 
 (define (read-sources doc-lkif)
   (let ((s (get-object-sources doc-lkif)))
     (map source-to-record s)))
 
 (define (read-theory doc-lkif)
   (let ((t (get-object-theory doc-lkif)))
     (theory-to-record t)))
 
 (define (read-argument-graphs doc-lkif)
   (let ((ag (get-object-argument-graphs doc-lkif)))
     (map argument-graph-to-record ag)))
  
 )