#!r6rs

(library (statement)
         ;(module statement mzscheme
         
         ;(require (lib "67.ss" "srfi"))  ; compare
         ;(require (lib "match.ss"))
         
         
         (export make-text text? text-id text-statement text-src text-summary
                 statement-equal? statement-compare statement-positive?
                 statement-negative? statement-complement statement-atom term->sxml
                 statement->sxml index-by-statement index-by-id summarize)
         
         (import (rnrs base)
                 (rnrs io simple)
                 (rnrs io ports)
                 (rnrs records syntactic (6))
                 ;(mzlib match)
                 ;(carneades match)
                 (rnrs hashtables (6))
                 (srfi/67 compare)
                 )
         
         #;(provide make-text text? text-id text-statement text-src text-summary
                    statement-equal? statement-compare statement-positive?
                    statement-negative? statement-complement statement-atom term->sxml
                    statement->sxml index-by-statement index-by-id summarize)
         
         ; <atom> := <datum>
         ; <statement> := <atom> | (not <atom>)
         
         
         (define-record-type text
           (fields
            id       ; symbol
            exp      ; statement | null
            summary  ; string, quoting or paraphrasing the source text
            src      ; string, a URL pointing to the original text
            ))
         
         (define (text-statement txt)
             (if (null? (text-exp txt))
                 (text-id txt)
                 (text-exp txt)))
         
         (define statement-equal? equal?)
         

         ; statement-compare: statement statement -> {-1,0,1}
         (define statement-compare default-compare)
         
         
         (define (statement-positive? s1)
           (if (pair? s1)
               (if (eqv? (car s1) 'not)
                   (#f)  ; s1 = (not s2)
                   (#t)) ; s1 = (s2)
               (#t)))    ; s1 = s2
         
         (define (statement-negative? s1)
           (if (pair? s1)
               (if (eqv? (car s1) 'not)
                   (#t)  ; s1 = (not s2)
                   (#f)) ; s1 = (s2)
               (#f)))    ; s1 = s2
         
         (define (statement-complement s1)
           (if (pair? s1)
               (if (eqv? (car s1) 'not)
                   (cdr s1)   
                   (cons 'not s1))
               (cons 'not s1)))
         
         #| statement-atom: statement -> statement
            The atom of the statement, i.e. strips the negation operator
            if there is one. |#
         (define (statement-atom s1)
           (if (pair? s1)
               (if (eqv? (car s1) 'not)
                   (cdr s1)
                   (s1))
               (s1)))
         
         (define (term->sxml t)
           (cond ((symbol? t) (string-append (symbol->string t) " "))
                 ((list? t) (statement->sxml t))
                 (else t)))
         
         (define (statement->sxml s)
           (cond ((symbol? s) (list 's (symbol->string s)))
                 ((list? s)
                  (if (eq? (car s) 'not)
                      (cons 'not (list (statement->sxml (cadr s))))
                      (cons 's (map term->sxml s))))
                 (else s)))
         
         #| index-by-statement: (list-of text) -> (hashtable-of datum text)
            assumption: the statement of each text in the list is unique.  |#
         (define (index-by-statement texts)
             (let ((tbl (make-hashtable 'equal)))
               (for-each (lambda (txt) 
                           (if (text-statement txt)
                               (hashtable-set! tbl (text-statement txt) txt)))
                         texts)
               tbl))
         
         #| index-by-id: (list-of text) -> (hashtable-of symbol text)
            assumption: the id of each text in the list is unique. |#
         (define (index-by-id texts)
             (let ((tbl (make-hashtable)))
               (for-each (lambda (txt) 
                           (hashtable-set! tbl (text-id txt) txt))
                         texts)
               tbl))
         
         #| summarize: statement -> string
            default string representation of statements,
            for use in reports and diagrams.  The default just displays the
            statement as an s-expression. |#
         (define (summarize statement)
           (let ((write-statement-to-port
                  (lambda (p f)
                    (write statement p)
                    (f))))
             (call-with-values open-string-output-port write-statement-to-port)))
         
         
) ; end of library statement