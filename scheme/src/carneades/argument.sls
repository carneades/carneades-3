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
 (carneades argument)
 
 (export make-ordinary-premise make-exception make-assumption   
         make-premise premise? ordinary-premise? exception? assumption?
         premise-atom premise-polarity premise-role premise-statement 
         pr am ex premise=? negative-premise? positive-premise? 
         make-argument argument? argument-id argument-direction argument-weight
         argument-conclusion argument-premises argument-scheme default-weight pro con
         define-argument argument->datum datum->argument add-premise status?
         proof-standard? state question accept reject assign-standard assert-substitutions 
         arguments pro-arguments con-arguments schemes-applied status proof-standard prior
         decided? accepted? rejected? questioned? stated? issue? make-argument-graph empty-argument-graph 
         argument-graph? argument-graph-id argument-graph-title argument-graph-main-issue
         argument-graph-nodes argument-graph-arguments argument-graph-substitutions assert-argument assert-arguments 
         questions facts statements accepted-statements rejected-statements 
         stated-statements relevant-statements list-arguments issues relevant?   
         satisfies? acceptable? holds? applicable? in? out? get-argument
         list->argument-graph instantiate-argument-graph update-statement)
 
 (import (rnrs)
         (rnrs records syntactic)
         ; (rnrs lists)
         (carneades base)
         (only (carneades system) gensym)
         (carneades statement)
         (carneades lib match)
         (prefix (carneades table) table:)
         (prefix (only (carneades lib srfi lists) lset-union any every) list:)
         (prefix (carneades lib srfi compare) compare:)
         (carneades unify)
         )
 
 (define *debug* #t)
 
 (define-record-type premise 
   (fields atom      ; an atomic statement
           polarity  ; boolean, #t => positive premise | #f => negative premise
           role))    ; string, the role of the premise in the argumentation scheme applied, empty string if none
 
 (define-record-type ordinary-premise (parent premise))
 (define-record-type assumption (parent premise))
 (define-record-type exception (parent premise))
 
 (define (positive-premise? p) (premise-polarity p))
 (define (negative-premise? p) (not (premise-polarity p)))
 
 (define (premise-statement p)
   (if (positive-premise? p)
       (premise-atom p)
       (statement-complement (premise-atom p))))
 
 ; abbreviations for constructing premises with empty roles
 
 ; pr: statement -> ordinary-premise
 (define (pr s) 
   (make-ordinary-premise (statement-atom s)
                          (statement-positive? s)
                          "")) 
 
 ; am: statement -> assumption
 (define (am s) 
   (make-assumption (statement-atom s)
                    (statement-positive? s)
                    ""))
 
 ; ex: statement -> exception
 (define (ex s) 
   (make-exception (statement-atom s)
                   (statement-positive? s)
                   ""))
 
 (define (premise=? p1 p2)
   (and (or (and (exception? p1) 
                 (exception? p2))
            (and (assumption? p1)
                 (assumption? p2))
            (and (ordinary-premise? p1)
                 (ordinary-premise? p2))
            (and (negative-premise? p1)
                 (negative-premise? p2)))
        (statement=? (premise-atom p1)
                     (premise-atom p2))))
 
 (define (direction? sym) (member sym '(pro con)))
 
 (define-record-type argument 
   (fields id           ; symbol
           applicable   ; boolean
           weight       ; 0.0 ... 1.0
           direction    ; 'pro | 'con
           conclusion   ; statement
           premises     ; premise list
           scheme))     ; a string describing or naming the scheme applied
 
 ; Implicit premises, i.e. enthymemes, may be revealed using the  add-premise function.
 
 (define default-weight 0.5)
 
 (define (pro id conclusion premises)
   (make-argument id #f default-weight 'pro conclusion premises ""))
 
 (define (con id conclusion premises)
   (make-argument id #f default-weight 'con conclusion premises ""))
 
 (define-syntax define-argument ; TODO: revise to use the argument->datum format 
   (syntax-rules (pro con and)
     ((define-argument id (pro conclusion premise ...))
      (define id (pro (quote id) conclusion (list premise ...))))
     ((define-argument id (con conclusion premise ...))
      (define id (con (quote id) conclusion (list premise ...))))))
 
 (define (premise->datum premise)
   (let* ((role (premise-role premise))
          (premise-type (cond ((exception? premise) 'exception)
                              ((assumption? premise) 'assumption)
                              (else 'premise)))
          (premise-sexp (cons premise-type (list (premise-statement premise)))))
     (if (equal? role "")
         (append premise-sexp (list role)) 
         premise-sexp)))
 
 ; argument->datum: argument -> datum
 ; represents an argument as a s-expression
 (define (argument->datum arg)
   `(argument (,(string->symbol "@") ; workaround PLT Scheme bug
               (id ,(argument-id arg))
               (direction ,(argument-direction arg))
               (scheme ,(argument-scheme arg)))
              ,@(map premise->datum (argument-premises arg))
              (conclusion ,(argument-conclusion arg)))) 
 
 ; datum->argument: datum -> argument 
 ; Converts an s-expression of the form generated by argument->datum
 ; back into an argument
 (define (datum->argument sexp)
   (define (get-attr attributes key default)
     (let* ((attrs (if (list? attributes) (cdr attributes) null))
            (entry (if (not (null? attrs)) (assq key (cdr attributes)))))
       (if entry (cadr entry) default)))
   
   (define (datum->premise sexp)
     (match sexp
       (('exception s) (ex s))
       (('assumption s) (am s))
       (('premise s) (pr s))
       (_ (pr sexp))))
   
   (define (get-conclusion l)
     (let ((sexp (filter (lambda (sexp) (eq? (car sexp) 'conclusion))
                         l)))
       (if (null? sexp) 
           (error "expression has no conclusion" sexp)
           (cadar sexp))))
   
   (define (get-premises l)
     (map (lambda (sexp) (datum->premise sexp))
          (filter 
           (lambda (sexp) (member (car sexp) '(premise exception assumption)))
           l)))
   
   
   (match sexp
     (('argument attributes . l)
      (make-argument (get-attr attributes 'id (gensym 'a))
                     #f
                     default-weight
                     (get-attr attributes 'direction 'pro)
                     (get-conclusion l)
                     (get-premises l)
                     (get-attr attributes 'scheme "")))
     (_ (error "expression does not represent an argument" sexp))))
 
 ; add-premise: premise argument -> argument
 (define (add-premise p arg)
   (make-argument (argument-id arg)
                  #f
                  default-weight
                  (argument-direction arg)
                  (argument-conclusion arg)
                  (cons p (argument-premises arg))
                  (argument-scheme arg)))
 
 (define (status? sym) (member sym '(unstated stated questioned accepted rejected))) 
 
 (define (proof-standard? sym)
   (member sym '(se        ; scintilla of the evidence
                 ba        ; best argument (was "preponderance of the evidence")
                 dv        ; dialectical validity
                 pe        ; preponderance of the evidence
                 cce       ; clear and convincing evidence
                 brd       ; beyond a reasonable doubt
                 )))   
 
 (define default-proof-standard 'dv)
 
 (define-record-type node 
   (fields statement              ; statement
           status                 ; status
           standard               ; proof standard
           in                     ; boolean
           complement-in          ; boolean
           acceptable             ; boolean
           complement-acceptable  ; boolean
           premise-of             ; (list-of symbol), where each symbol is an argument id
           conclusion-of          ; (list-of symbol), where each symbol is an argument id
           ))
 
 ; builds a new node from a statement with no pro- or con-arguments
 (define (statement->node s)
   (make-node s
              'stated
              default-proof-standard
              #f
              #f
              #f
              #f
              null
              null))
 
 ; builds a node-table from a list of statements
 (define (statements->nodes s)
   (fold-left (lambda (t s)
                (table:insert t s (statement->node s)))
              (table:make-table statement-hash statement=? null)
              s))              
 
 (define-record-type argument-graph
   (fields id             ; symbol
           title          ; string
           main-issue     ; statement | #f
           nodes          ; table: statement -> node
           arguments      ; table: argument-id -> argument 
           substitutions) ; term -> term
   (protocol
    (lambda (new)
      (case-lambda 
        ((id title main-issue nodes arguments subs)
         (new id title main-issue nodes arguments subs))
        ((id title main-issue)
         (new id title main-issue (table:make-table statement-hash statement=? null) (table:make-eq-table null) identity))
        ((id title main-issue statements)
         (new id title main-issue (statements->nodes statements) (table:make-eq-table null) identity))
        (() (new (gensym) "" #f (table:make-table statement-hash statement=? null) (table:make-eq-table null) identity))))))
 
 (define empty-argument-graph (make-argument-graph))
 
 ; status: argument-graph statement -> status
 (define (status ag s) 
   (let ((n (table:lookup (argument-graph-nodes ag) ((argument-graph-substitutions ag) (statement-atom)) #f)))
     (if (not n)
         'unstated
         (let ((v (node-status n)))
           (if (statement-positive? s)
               v
               (case v 
                 ((stated) 'stated)
                 ((questioned) 'questioned)
                 ((accepted) 'rejected)
                 ((rejected) 'accepted)
                 (else 'unstated)))))))
 
 ; proof-standard: argument-graph statement -> standard
 (define (proof-standard ag s) 
   (let ((n (table:lookup (argument-graph-nodes ag) (statement-atom s) #f)))
     (if (not n)
         default-proof-standard
         (node-standard n))))
 
 ; prior: argument argument -> boolean
 (define (prior a1 a2) 
   (> (argument-weight  a1)
      (argument-weight  a2)))
 
 ; put-node: argument-graph node -> argument-graph 
 (define (put-node ag n)
   (make-argument-graph (argument-graph-id ag)
                        (argument-graph-title ag)
                        (argument-graph-main-issue ag)
                        (table:insert (argument-graph-nodes ag) (node-statement n) n) 
                        (argument-graph-arguments ag)
                        (argument-graph-substitutions ag)))
 
 ; get-node: argument-graph statement -> node | #f
 (define (get-node ag s)
   (table:lookup (argument-graph-nodes ag) (statement-atom s) #f))
 
 ; get-nodes: argument-graph -> (list-of node)
 (define (get-nodes ag) (table:objects (argument-graph-nodes ag)))
 
 ; update-statement: argument-graph statement -> argument-graph
 ; updates the nodes for the given statement. If the "in" status of the statement
 ; of the node, or the complement of the statement of the node, is different  
 ; from the old-in and old-complement in flags, than the change
 ; is propogated forwards by udating the arguments in which the statement 
 ; is used as a premise, which, recursively, updates the conclusions of these arguments.
 (define (update-statement ag1 s)
   (if *debug* (printf "updating statement: ~s~%" s))
   (let* ((n1 (table:lookup (argument-graph-nodes ag1) (statement-atom s) #f)))
     (if (not n1)
         (begin (if *debug* (printf "node not found: ~s~%" s))
                ag1)
         (let* ((n2 (make-node (statement-atom s)
                               (node-status n1)
                               (node-standard n1)
                               (check-if-in ag1 (statement-atom s))
                               (check-if-in ag1 (statement-complement s))
                               (check-acceptability ag1 (statement-atom s))
                               (check-acceptability ag1 (statement-complement (statement-atom s)))
                               (node-premise-of n1)
                               (node-conclusion-of n1)))
                (ag2 (make-argument-graph (argument-graph-id ag1)
                                          (argument-graph-title ag1)
                                          (argument-graph-main-issue ag1)
                                          (table:insert (argument-graph-nodes ag1)
                                                        (statement-atom s)
                                                        n2)
                                          (argument-graph-arguments ag1)
                                          (argument-graph-substitutions ag1))))
           (if (and (eq? (node-in n1) (node-in n2))
                    (eq? (node-complement-in n1) (node-complement-in n2)))
               ; then the status of the statement hasn't changed and there's no need to propogate further
               ag2
               ; else propogate the update to the arguments in which the statement is a premise
               (fold-right (lambda (arg-id ag)
                             (let ((arg (get-argument ag arg-id)))
                               (if arg
                                   (update-argument ag arg)
                                   ag)))
                           ag2
                           (node-premise-of n1))))))) 
 
 ; update-statements: argument-graph (list-of statement) -> argument-graph
 (define (update-statements ag statements)
   (fold-right (lambda (s ag) (update-statement ag s)) ag statements))
 
 ; update-argument: argument-graph argument -> argument-graph
 ; updates the applicability of an argument in an argument graph and propogates 
 ; this change by updating the argument graph for the conclusion
 ; of the argument.  
 (define (update-argument ag arg)
   (if *debug* (printf "updating argument: ~s~%" (argument-id arg)))
   (let* ((old-applicability (argument-applicable arg))
          (new-applicability (all-premises-hold? ag arg))
          (ag2 (make-argument-graph (argument-graph-id ag)
                                            (argument-graph-title ag)
                                            (argument-graph-main-issue ag)
                                            (argument-graph-nodes ag)
                                            (table:insert (argument-graph-arguments ag)
                                                          (argument-id arg)
                                                          (make-argument (argument-id arg)
                                                                         new-applicability
                                                                         (argument-weight arg)
                                                                         (argument-direction arg)
                                                                         (argument-conclusion arg)
                                                                         (argument-premises arg)
                                                                         (argument-scheme arg)))
                                            (argument-graph-substitutions ag))))
     (if (eq? old-applicability new-applicability)
         ag2
         ; update the applicability of the new argument and propogate the change
         ; by updating the conclusion of the argument
         (update-statement ag2 (argument-conclusion arg)))))
 
  
 ; update-arguments: argument-graph (list-of argument) -> argument-graph
 (define (update-arguments ag args)
   (fold-right (lambda (arg ag) (update-argument ag arg)) ag args))
 
 ; change-status argument-graph statement status -> argument-graph
 ; change the status of the atom of the given statement to the given status.
 (define (change-status ag s status)
   (let ((n (table:lookup (argument-graph-nodes ag) 
                          (statement-atom s) 
                          (statement->node (statement-atom s)))))
     (make-argument-graph (argument-graph-id ag)
                          (argument-graph-title ag)
                          (argument-graph-main-issue ag)
                          (table:insert (argument-graph-nodes ag)
                                        (statement-atom s)
                                        (make-node (statement-atom s)
                                                   status ; the attributed changed
                                                   (node-standard n)
                                                   (node-in n)
                                                   (node-complement-in n)
                                                   (node-acceptable n)
                                                   (node-complement-acceptable n)
                                                   (node-premise-of n)
                                                   (node-conclusion-of n)))
                          (argument-graph-arguments ag)
                          (argument-graph-substitutions ag))))
 
 ; state: argument-graph (list-of statement) -> argument-graph
 ; Changes, non-destructively, the status of each statement in the list to 
 ; stated in the argument graph.  Statements in the list which do not
 ; have a node in the argument graph are ignored. 
 (define (state ag statements)
   (update-statements (fold-right (lambda (s ag) (change-status ag s 'stated))
                                  ag 
                                  statements)
                      statements))
   
 
 ; question: argument-graph (list-of statement) -> argument-graph
 (define (question ag statements)
   (update-statements (fold-right (lambda (s ag) (change-status ag s 'questioned))
                                  ag 
                                  statements)
                      statements))
 
 ; accept: argument-graph (list-of statement) -> argument-graph
 (define (accept ag statements)
   (update-statements (fold-right (lambda (s ag) 
                                    (change-status ag 
                                                   (statement-atom s) 
                                                   (if (statement-positive? s) 
                                                       'accepted 
                                                       'rejected)))
                                  ag 
                                  statements)
                      statements))
 
 ; reject: argument-graph (list-of statement) -> argument-graph
 (define (reject ag statements)
   (update-statements (fold-right (lambda (s ag) 
                                    (change-status ag 
                                                   (statement-atom s)
                                                   (if (statement-positive? s) 
                                                       'rejected
                                                       'accepted)))
                                  ag 
                                  statements)
                      statements))
 
 ; assign-standard: argument-graph  proof-standard (list-of statement) -> argument-graph
 (define (assign-standard ag ps statements)
   (update-statements (fold-right (lambda (s ag) 
                                    (let ((n (table:lookup (argument-graph-nodes ag) 
                                                           (statement-atom s) 
                                                           (statement->node (statement-atom s)))))
                                      (make-argument-graph (argument-graph-id ag)
                                                           (argument-graph-title ag)
                                                           (argument-graph-main-issue ag)
                                                           (table:insert (argument-graph-nodes ag)
                                                                         (statement-atom s)
                                                                         (make-node (statement-atom s)
                                                                                    (node-status n)
                                                                                    ps ; the new proof standard
                                                                                    (node-in n)
                                                                                    (node-complement-in n)
                                                                                    (node-acceptable n)
                                                                                    (node-complement-acceptable n)
                                                                                    (node-premise-of n)
                                                                                    (node-conclusion-of n)))
                                                           (argument-graph-arguments ag)
                                                           (argument-graph-substitutions ag)))
                                    ag
                                    statements))
                      statements))
     
 ; assert-substitutions: argument-graph substitutions -> argument-graph
 ; constructs an argument graph by replacing the substitutions with the ones provided
 (define (assert-substitutions ag newsubs)
   (make-argument-graph (argument-graph-id ag)
                        (argument-graph-title ag)
                        (argument-graph-main-issue ag)
                        (argument-graph-nodes ag)
                        (argument-graph-arguments ag)
                        newsubs))
 
 ; get-argument: argument-graph symbol -> argument | #f
 (define (get-argument ag id)
   (table:lookup (argument-graph-arguments ag) id #f))
 
 ; list-arguments: argument-graph -> (list-of argument)
 (define (list-arguments ag) (table:objects (argument-graph-arguments ag)))
 
 ; instantiate-argument-graph: argument-graph substitutions -> argument-graph
 ; replace any variables in the statements of the arguments in the 
 ; argument graph with their values in the substitutions
 (define (instantiate-argument-graph ag subs)
   (let ((iag (list->argument-graph 
               (map (lambda (arg) (subs (argument->datum arg)))
                    (list-arguments ag)))))
     (make-argument-graph (argument-graph-id ag)
                          (argument-graph-title ag)
                          (argument-graph-main-issue ag)
                          (argument-graph-nodes iag)
                          (argument-graph-arguments iag))))
 
 ; add-string (list-of string) -> (list-of string)
 ; Add the string, s, the list l, if it is not already a member of l
 (define (add-string s l)
   (if (memq s l) l (cons s l)))
 
 ; ids->arguments: argument-graph (list-of symbol) -> (list-of argument)
 (define (ids->arguments ag ids)
   (filter (lambda (arg) (not (eq? arg #f)))
           (map (lambda (arg-id) (get-argument ag arg-id))
                ids)))
   
 ; arguments: argument-graph statement -> (list-of argument)
 ; all arguments pro and con some statement in an argument graph
 (define (arguments ag s)
   (let ((n (get-node ag s)))
     (if (not (node? n))
         null
         (ids->arguments ag (node-conclusion-of n)))))
 
  ; pro-arguments: argument-graph statement  -> (list-of argument)
 (define (pro-arguments ag s)
   (filter (lambda (arg) (eq? (argument-direction arg) 'pro))
                (arguments ag s)))
 
 ; con-arguments: argument-graph statement -> (list-of argument)
 (define (con-arguments ag s)
   (filter (lambda (arg) (eq? (argument-direction arg) 'con))
                (arguments ag s)))
 
 ; schemes-applied: argument-graph statement -> (list-of symbol)
 (define (schemes-applied ag s)
   (let ((n (get-node ag s)))
     (if (not (node? n))
         null
         (map argument-scheme (ids->arguments ag (node-conclusion-of n))))))
 
 ; accepted?: argument-graph statement -> boolean
 #;(define (accepted? ag s)
   (if (statement-positive? s)
       (let* ((s1 ((argument-graph-substitutions ag) s))
              (n (table:lookup (argument-graph-nodes ag) s1 #f)))
         (and n (eq? (node-status n) 'accepted)))
       (rejected? ag (statement-atom s))))
         
 (define (accepted? ag s)
   (if (statement-positive? s)
       (let ((s1 ((argument-graph-substitutions ag) s)))
         (and (find (lambda (s2)
                      (statement=? s1 s2))
                    (map (argument-graph-substitutions ag) 
                         (accepted-statements ag)))
              #t))
       (rejected? ag (statement-atom s))))
 
 ; rejected?: argument-graph statement -> boolean
 #;(define (rejected? ag s)
   (if (statement-positive? s)
       (let* ((s1 ((argument-graph-substitutions ag) s))
              (n (table:lookup (argument-graph-nodes ag) s1 #f)))
         (and n (eq? (node-status n) 'rejected)))
       (accepted? ag (statement-atom s))))
   
 (define (rejected? ag s)
   (if (statement-positive? s)
       (let ((s1 ((argument-graph-substitutions ag) s)))
         (and (find (lambda (s2) (statement=? s1 s2))
                    (map (argument-graph-substitutions ag) 
                         (rejected-statements ag)))
              #t))
       (accepted? ag (statement-atom s))))
 
 ; decided?: argument-graph statement -> boolean
 (define (decided? ag s) (or (accepted? ag s) (rejected? ag s)))
 
 ; questioned?: argument-graph statement -> boolean
 (define (questioned? ag s)
   (let ((n (table:lookup (argument-graph-nodes ag) (statement-atom s) #f)))
     (and n (eq? 'questioned (node-status n)))))
 
 ; stated?: argument-graph statement -> boolean
 (define (stated? ag s)
    (let ((n (table:lookup (argument-graph-nodes ag) (statement-atom s) #f)))
     (and n (eq? 'stated (node-status n)))))
 
 ; issue?: argument-graph statement -> boolean
 ; An statement is an issue if it is undecided in the argument graph
 ; An acceptable statement is still an issue, due to nonmonotonicity:
 ; Additional arguments may make the statement unacceptable again.
 (define (issue? ag s) (not (decided? ag s)))
 
 ; questions: argument-graph -> (list-of statement)
 (define (questions ag)
   (table:filter-keys (lambda (pair) 
                        (eq? (node-status (cdr pair)) 'questioned)) 
                      (argument-graph-nodes ag)))
 
 ; accepted-statements: argument-graph -> (list-of statement)
 (define (accepted-statements ag)
   (table:filter-keys (lambda (pair) 
                        (eq? (node-status (cdr pair)) 'accepted)) 
                      (argument-graph-nodes ag)))
 
 ; rejected-statements: argument-graph -> (list-of statement)
 (define (rejected-statements ag)
   (table:filter-keys (lambda (pair) 
                        (eq? (node-status (cdr pair)) 'rejected)) 
                      (argument-graph-nodes ag)))
 
 ; facts: argument-graph -> (list-of statement)
 ; The "facts" are the accepted statements and the complements 
 ; of the rejected statements
 (define (facts ag)
   (append (accepted-statements ag)
           (map (lambda (s) `(not ,s))
                (rejected-statements ag))))
 
 ; stated-statements: argument-graph -> (list-of statement)
 (define (stated-statements ag)
   (table:filter-keys (lambda (pair) 
                        (eq? (node-status (cdr pair)) 'stated)) 
                      (argument-graph-nodes ag)))
 
 ; issues: argument-graph argument-graph -> (list-of statement)
 (define (issues ag)
   (filter (lambda (s) (issue? ag s)) (statements ag)))
 
 ; all-premises: argument-graph statement -> (list-of premise)
 ; Returns the set of all the premises of all arguments pro or con the 
 ; statement s in the argument graph ag. The set of premises is represented as a list.         
 (define (all-premises ag s)
   (fold-right (lambda (arg l) 
                 (list:lset-union premise=? (argument-premises arg) l))
               null
               (arguments ag s)))
 
 ; depends-on?: argument-graph statement statement -> boolean
 ; s1 depends on s2 in ag if s1 equals s2 or, recursively, some premise 
 ; of some argument pro or con s1 depends on s2 in ag. 
 (define (depends-on? ag s1 s2)
   (or (statement=? s1 s2)
       (list:any (lambda (p) (depends-on? ag (premise-atom p) s2))
                 (all-premises ag s1))))
 
 ; cycle-free?: argument argument-graph -> boolean
 ; (cycle-free arg ag) checks whether an argument arg will not introduce a cycle into
 ; the argument graph ag.  An argument will not introduce a cycle if none of its
 ; premises depend on its conclusion in ag.
 (define (cycle-free? arg ag)
   (not (list:any (lambda (p) (depends-on? ag 
                                           (premise-atom p) 
                                           (argument-conclusion arg)))
                  (argument-premises arg))))
 
 ; relevant?: argument-graph statement statement -> boolean
 ;  A sentence s is relevant for proving a 
 ;  goal sentence g in ag if g depends on s in ag 
 (define (relevant? ag s g) (depends-on? ag g s))
 
 ; statements: argument-graph -> (list-of statement)
 ; returns the list of all statements in the argument graph
 (define (statements ag)
   (table:keys (argument-graph-nodes ag)))
 
 ; relevant-statements: argument-graph statement -> (list-of statement)
 ; (relevant-statements ag g) finds the statements in ag which are
 ; relevant for proving g. Since only nodes in the graph are 
 ; considered, g itself will be a member of the resulting list
 ; only if it has a node in the argument graph.
 
 (define (relevant-statements ag g)
   (filter (lambda (s) (relevant? ag s g))
           (statements ag))) 
 
 ; assert-argument: argument-graph argument -> argument-graph
 ; Add the argument, arg, to the argument graph, ag,
 ; if doing so would not introduce a cycle. If some argument with
 ; the same id exists in the argument graph, then the existing argument is replaced by the
 ; new argument. A node for the conclusion of the argument is added to the node table of the 
 ; argument graph if one does not yet exist. It is the responsiblity of the
 ; protocol to question the conclusion of the argument, if this is wanted.
 
 (define (assert-argument ag arg)
   (cond ((not (cycle-free? arg ag))
          (error "assert-argument: cyclic argument." arg))
         (else (let* ((n (or (get-node ag (argument-conclusion arg))
                             (statement->node (argument-conclusion arg))))
                      ; update the node for the conclusion of the argument
                      (ag1 (put-node ag (make-node (node-statement n)
                                                   (node-status n)
                                                   (node-standard n)
                                                   (node-in n)
                                                   (node-complement-in n)
                                                   (node-acceptable n) ; updated below
                                                   (node-complement-acceptable n) ; updated below
                                                   (node-premise-of n)
                                                   (add-string (argument-id arg) 
                                                               (node-conclusion-of n)))))
                      ; update or create nodes for each of the premises of the argument
                      (ag2 (fold-right (lambda (p ag1)
                                         (let ((n (or (get-node ag1 (premise-atom p))
                                                      (statement->node (premise-atom p)))))
                                           
                                           (put-node ag1 
                                                     (make-node (node-statement n)
                                                                (node-status n)
                                                                (node-standard n)
                                                                (node-in n)
                                                                (node-complement-in n)
                                                                (node-acceptable n) ; updated below
                                                                (node-complement-acceptable n) ; updated below
                                                                (add-string (argument-id arg)
                                                                            (node-premise-of n))
                                                                (node-conclusion-of n)))))
                                       ag1
                                       (argument-premises arg))))
                 (update-argument ag2 arg)))))
 
 ; assert-arguments: argument-graph (list-of argument) -> argument-graph
 ; asserts a list of arguments
 (define (assert-arguments ag args) 
   (fold-right (lambda (arg ag) (assert-argument ag arg))
               ag 
               args))

 ; list->argument-graph: (list-of datum) -> argument-graph
 ; converts a list of expressions representing arguments into an argument graph
 (define (list->argument-graph l)
   (assert-arguments empty-argument-graph (map datum->argument l)))
 
 ; satisfies?: argument-graph proof-standard 
 ;            (list-of argument) (list-of argument) -> boolean
 (define (satisfies? ag ps pro-args con-args)
   (case ps
     ((se) (scintilla? ag pro-args con-args)) 
     ((ba) (best-argument? ag pro-args con-args))
     ((dv) (dialectically-valid? ag pro-args con-args))
     ((pe) (preponderance-of-the-evidence? ag pro-args con-args))
     ((cce) (clear-and-convincing-evidence? ag pro-args con-args))
     ((brd) (beyond-reasonable-doubt? ag pro-args con-args))
     (else (error "satisfies: unknown proof standard." ps))))
 
 ; check-acceptability: argument-graph statement -> boolean
 (define (check-acceptability ag s)
   (let ((ps (proof-standard ag s)))
     (if (statement-negative? s)
         (satisfies? ag ps 
                     (con-arguments ag (statement-atom s)) 
                     (pro-arguments ag (statement-atom s)))
         (satisfies? ag ps 
                     (pro-arguments ag s) 
                     (con-arguments ag s)))))
 
 ; acceptable?: argument-graph statement -> boolean
 (define (acceptable? ag s)
   (let ((n (table:lookup (argument-graph-nodes ag) s #f)))
     (if (not n)
         #f  
         (if (statement-positive? s)
             (node-acceptable n)
             (node-complement-acceptable n)))))
 
 ; applicable?: argument-graph argument -> boolean
 ; Assumes that the applicability of the argument has been 
 ; previously computed or updated and stored in the applicable
 ; field of the argument.
 (define (applicable? ag arg) (argument-applicable arg))
 
 ; in?: argument-graph statement -> boolean 
 ; looks up the cached "in" status of the statement in the agreement graph
 (define (in? ag s)
   (let* ((s2  ((argument-graph-substitutions ag)(statement-atom s)))
          (n (table:lookup (argument-graph-nodes ag) s #f)))
     (if (and *debug* (not n)) (printf "in?: no node for ~a~%" s))
     (and n
          (if (statement-positive? s)
              (node-in n)
              (node-complement-in n)))))
 
 ; check-if-in: argument-graph statement -> boolean
 ; computes whether the statement is "in" in the argument graph
 (define (check-if-in ag s)
   (let* ((s2  ((argument-graph-substitutions ag)(statement-atom s)))
          (n (table:lookup (argument-graph-nodes ag) s2 #f)))
     (if (and *debug* (not n)) (printf "check-if-in: no node for ~a~%" s))
     (and n
          (if (statement-positive? s2)
              (or (eq? (node-status n) 'accepted)
                  (and (not (eq? (node-status n) 'rejected))
                       (check-acceptability ag s)))
              (or (eq? (node-status n) 'rejected)
                  (and (not (eq? (node-status n) 'accepted))
                       (check-acceptability ag s)))))))
   
 ; out?: argument-graph statement -> boolean
 (define (out? ag s) (not (in? ag s)))
 
 ; holds?: argument-graph premise -> boolean
 (define (holds? ag p) 
   (let ((n (table:lookup (argument-graph-nodes ag) ((argument-graph-substitutions ag) (premise-atom p)) #f)))
     (if (not n) #f
         (cond ((ordinary-premise? p)
                (case (node-status n)
                  ((accepted) (positive-premise? p))
                  ((rejected) (negative-premise? p))
                  ((questioned stated) (in? ag (premise-statement p)))
                  ((unstated) #f)))
               ((assumption? p)
                (case (node-status n)
                  ((stated) #t) ; whether the premise is positive or negative 
                  ((unstated) #f)
                  ((accepted) (positive-premise? p))
                  ((rejected) (negative-premise? p))
                  ((questioned) (in? ag (premise-statement p)))))
               ((exception? p)
                (case (node-status n)
                  ((accepted) (negative-premise? p))
                  ((rejected) (positive-premise? p))
                  ((stated questioned) (out? ag (premise-statement p)))
                  ((unstated) #f)))
               (else (error "holds: not a premise." p))))))

 
 ; all-premises-hold?: argument-graph argument -> boolean
 (define (all-premises-hold? ag arg)
   (list:every (lambda (p) (holds? ag p)) (argument-premises arg)))
 
 ; scintilla?: argument-graph (list-of argument) (list-of argument) -> boolean
 (define (scintilla? ag pro-args con-args)
   (list:any (lambda (arg) (all-premises-hold? ag arg)) 
             pro-args))
 
 ; dialectically-valid?: argument-graph (list-of argument) (list-of argument) -> boolean
 (define (dialectically-valid? ag pro-args con-args)
   (and (scintilla? ag pro-args con-args)
        (not (list:any (lambda (arg) (all-premises-hold? ag arg))
                       con-args))))
 
 ; best-argument?: argument-graph (list-of argument) (list-of argument) -> boolean
 (define (best-argument? ag pro-args con-args)
   (let* ((pro (filter (lambda (arg) (all-premises-hold? ag arg))
                       pro-args))
          (con (filter (lambda (arg) (all-premises-hold? ag arg))
                       con-args))
          (best-pro (if (null? pro)
                        0.0 
                        (apply max (map argument-weight pro))))
          (best-con (if (null? con)
                        0.0
                        (apply max (map argument-weight con)))))
     (> best-pro best-con)))
 
 ; preponderance-of-the-evidence?: argument-graph (list-of argument) (list-of argument) -> boolean
 (define preponderance-of-the-evidence? best-argument?) 
 
 ; clear-and-convincing-evidence?: argument-graph (list-of argument) (list-of argument) -> boolean
 (define (clear-and-convincing-evidence? ag pro-args con-args)
   (let* ((pro (filter (lambda (arg) (all-premises-hold? ag arg))
                       pro-args))
          (con (filter (lambda (arg) (all-premises-hold? ag arg))
                       con-args))
          (best-pro (if (null? pro)
                        0.0 
                        (apply max (map argument-weight pro))))
          (best-con (if (null? con)
                        0.0
                        (apply max (map argument-weight con))))
          (alpha 0.5)
          (beta 0.3))
     (and (> best-pro best-con)  ; i.e. preponderance of the evidence test is met
          (> best-pro alpha)
          (> (- best-pro best-con) beta))))
 
 
 ; beyond-reasonable-doubt?: argument-graph (list-of argument) (list-of argument) -> boolean
 (define (beyond-reasonable-doubt? ag pro-args con-args)
   (let* ((pro (filter (lambda (arg) (all-premises-hold? ag arg))
                       pro-args))
          (con (filter (lambda (arg) (all-premises-hold? ag arg))
                       con-args))
          (best-pro (if (null? pro)
                        0.0 
                        (apply max (map argument-weight pro))))
          (best-con (if (null? con)
                        0.0
                        (apply max (map argument-weight con))))
          (alpha 0.5)
          (beta 0.3)
          (gamma 0.2))
     (and  
      ; clear and convincing evidence test is also met
      (> best-pro best-con) 
      (> best-pro alpha)
      (> (- best-pro best-con) beta)
      ; and the strongest con argument weighs less than the gamma threshold
      (< best-con gamma))))

 ) ; module argument
