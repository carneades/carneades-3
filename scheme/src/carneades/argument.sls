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
         premise? ordinary-premise? exception? assumption?
         premise-atom premise-polarity premise-role premise-statement 
         pr am ex premise=? negative-premise? positive-premise? 
         make-argument argument? argument-id argument-direction argument-weight
         argument-conclusion argument-premises argument-scheme default-weight pro con
         define-argument argument->datum datum->argument argument-variables instantiate-argument add-premise status?
         proof-standard? state question accept reject assign-standard 
         statements arguments pro-arguments con-arguments schemes-applied status proof-standard prior
         decided? accepted? rejected? questioned? stated? issue? make-argument-graph empty-argument-graph 
         argument-graph? argument-graph-id argument-graph-title argument-graph-main-issue
         argument-graph-nodes argument-graph-arguments assert-argument assert-arguments 
         relevant? satisfies? acceptable? holds? applicable? in? out? get-argument
         list->argument-graph)
 
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
         (prefix (carneades set) set:)
         )
 
 (define *debug* #f)
 
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
           scheme)      ; a string describing or naming the scheme applied
   (protocol
    (lambda (new)
      (case-lambda 
        ((id applicable weight direction conclusion premises scheme)
         (new id applicable weight direction conclusion premises scheme))
        ((id direction conclusion premises scheme)
         (new id #f default-weight direction conclusion premises scheme))
        ((id direction conclusion premises)
         (new id #f default-weight direction conclusion premises ""))))))
 
 ; Implicit premises, i.e. enthymemes, may be revealed using the  add-premise function.
 
 (define default-weight 0.5)
 
 (define (pro id conclusion premises)
   (make-argument id #f default-weight 'pro conclusion premises ""))
 
 (define (con id conclusion premises)
   (make-argument id #f default-weight 'con conclusion premises ""))
 
 ; assign-applicability: argument boolean -> argument
 (define (assign-applicability arg new-applicability)
   (make-argument (argument-id arg)
                  new-applicability
                  (argument-weight arg)
                  (argument-direction arg)
                  (argument-conclusion arg)
                  (argument-premises arg)
                  (argument-scheme arg)))
 
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
 
 ; argument-variables: argument -> (list-of symbol)
 (define (argument-variables arg)
   (let ((cv (variables (argument-conclusion arg)))
         (pv (apply append (map (lambda (p) (variables (premise-atom p)))
                      (argument-premises arg)))))
     (set:set->list ((set:list->set eq?) (append cv pv)))))
 
 ; instantiate-argument: argument substitutions -> argument
 (define (instantiate-argument arg subs)
   (make-argument (argument-id arg)
                      (argument-applicable arg)
                      (argument-weight arg)
                      (argument-direction arg)
                      (subs (argument-conclusion arg))
                      (map (lambda (p) 
                             (cond ((ordinary-premise? p)
                                    (make-ordinary-premise (subs (premise-atom p))
                                                           (premise-polarity p)
                                                           (premise-role p)))
                                   ((exception? p)
                                    (make-exception (subs (premise-atom p))
                                                    (premise-polarity p)
                                                    (premise-role p)))
                                   ((assumption? p)
                                    (make-assumption (subs (premise-atom p))
                                                     (premise-polarity p)
                                                     (premise-role p)))))
                           (argument-premises arg))
                      (argument-scheme arg)))
 
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
              null
              null))
 
 ; builds a node-table from a list of statements
 ; statements->nodes: (list-of statement) -> node table
 (define (statements->nodes s)
   (fold-left (lambda (t s)
                (let ((bucket (table:lookup t 
                                            (statement-predicate s)
                                            (table:make-table statement-hash statement=? null)))) 
                  (table:insert t 
                                (statement-predicate s) 
                                (table:insert bucket s (statement->node s)))))
              (table:make-eq-table null)
              s))              
 
 (define-record-type argument-graph
   (fields id             ; symbol
           title          ; string
           main-issue     ; statement | #f
           nodes          ; table: symbol -> statement -> node
           arguments)     ; table: argument-id -> argument 
   (protocol
    (lambda (new)
      (case-lambda 
        (() (new (gensym) "" #f (table:make-eq-table null) (table:make-eq-table null)))
        ((id title main-issue)
         (new id title main-issue (table:make-eq-table null) (table:make-eq-table null)))
        ((id title main-issue statements)
         (new id title main-issue (statements->nodes statements) (table:make-eq-table null)))
        ((id title main-issue nodes arguments)
         (new id title main-issue nodes arguments)) ))))
 
 (define empty-argument-graph (make-argument-graph))
 
 ; status: argument-graph statement -> status
 (define (status ag s) 
   (let ((n (get-node ag s)))
     (let ((v (node-status n)))
       (if (statement-positive? s)
           v
           (case v 
             ((stated) 'stated)
             ((questioned) 'questioned)
             ((accepted) 'rejected)
             ((rejected) 'accepted)
             (else 'unstated))))))
 
 ; proof-standard: argument-graph statement -> standard
 (define (proof-standard ag s) 
   (node-standard (get-node ag s)))
 
 ; prior: argument argument -> boolean
 (define (prior a1 a2) 
   (> (argument-weight  a1)
      (argument-weight  a2)))
 
 ; get-node-bucket: argument-graph symbol -> table
 (define (get-node-bucket ag predicate)
   (table:lookup (argument-graph-nodes ag) 
                 predicate
                 (table:make-table statement-hash statement=? null)))
                 
 ; get-node: argument-graph statement -> node
 (define (get-node ag s)
   (table:lookup (get-node-bucket ag (statement-predicate (statement-atom s))) 
                 (statement-atom s) 
                 (statement->node s)))
 
 ; add-node-to-argument-graph: argument-graph node -> argument-graph 
 ; add a node to the nodes table of an argument graph and replace 
 ; the nodes table of the argument graph with this new table
 (define (add-node-to-argument-graph ag n)
   (make-argument-graph (argument-graph-id ag)
                        (argument-graph-title ag)
                        (argument-graph-main-issue ag)
                        (table:insert (argument-graph-nodes ag)
                                      (statement-predicate (node-statement n))
                                      (table:insert (get-node-bucket ag (statement-predicate (node-statement n)))
                                                    (node-statement n) 
                                                    n)) 
                        (argument-graph-arguments ag)))
  
 ; update-statement: argument-graph statement (or symbol #f) -> argument-graph
 ; updates the nodes for the given statement by changing the status of the 
 ; statment to new-status and recomputing the acceptability of the statement
 ; and its complement. If the "in" status of the statement
 ; of the node, or the complement of the statement, is affected, the change
 ; is propogated forwards by updating the arguments in which the statement 
 ; is used as a premise, which, recursively, updates the conclusions of these arguments.
 (define (update-statement ag1 s new-status)
   (if *debug* (printf "updating statement: ~s with new status ~s~%" s new-status))
   (let* ((n1 (get-node ag1 s)) 
          (old-status (node-status n1))
          (n2 (make-node (statement-atom s)
                         (or new-status old-status)
                         (node-standard n1)
                         (check-acceptability ag1 (statement-atom s))
                         (check-acceptability ag1 (statement-complement (statement-atom s)))
                         (node-premise-of n1)
                         (node-conclusion-of n1)))
          (ag2 (add-node-to-argument-graph ag1 n2))) 
 
     (if (and (eq? (node-in? ag1 n1 #t) (node-in? ag2 n2 #t))
              (eq? (node-in? ag1 n1 #f) (node-in? ag2 n2 #f)))
         ; then the "in" status of the statement hasn't changed and there's no need to propogate further
         ag2
         ; else propogate the update to the arguments in which the statement is a premise
         (fold-right (lambda (arg-id ag)
                       (let ((arg (get-argument ag arg-id)))
                         (if arg
                             (update-argument ag arg)
                             ag)))
                     ag2
                     (node-premise-of n1))))) 
 
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
                                                  (assign-applicability arg 
                                                                        new-applicability)))))
     
     (if (eq? old-applicability new-applicability)
         ag2
         ; update the applicability of the new argument and propogate the change
         ; by updating the conclusion of the argument
         (update-statement ag2 (argument-conclusion arg) #f))))
 
 ; state: argument-graph (list-of statement) -> argument-graph
 ; Changes, non-destructively, the status of each statement in the list to 
 ; stated in the argument graph.  Statements in the list which do not
 ; have a node in the argument graph are ignored. 
 (define (state ag statements)
   (fold-right (lambda (s ag) (update-statement ag s 'stated))
               ag 
               statements))
   
 
 ; question: argument-graph (list-of statement) -> argument-graph
 (define (question ag statements)
   (fold-right (lambda (s ag) (update-statement ag s 'questioned))
               ag 
               statements))
 
 ; accept: argument-graph (list-of statement) -> argument-graph
 (define (accept ag statements)
   (fold-right (lambda (s ag) 
                 (update-statement ag 
                                   s
                                   (if (statement-positive? s) 
                                       'accepted 
                                       'rejected)))
               ag 
               statements))
 
 ; reject: argument-graph (list-of statement) -> argument-graph
 (define (reject ag statements)
   (fold-right (lambda (s ag) 
                 (update-statement ag 
                                   s
                                   (if (statement-positive? s) 
                                       'rejected
                                       'accepted)))
               ag 
               statements))
 
 ; assign-standard: argument-graph  proof-standard (list-of statement) -> argument-graph
 (define (assign-standard ag ps statements)
   (fold-right (lambda (s ag) 
                 (let ((n (get-node ag s)))
                   (update-statement (add-node-to-argument-graph ag (make-node (statement-atom s)
                                                                               (node-status n)
                                                                               ps ; the new proof standard
                                                                               (node-acceptable n)
                                                                               (node-complement-acceptable n)
                                                                               (node-premise-of n)
                                                                               (node-conclusion-of n)))
                                     s
                                     (node-status n))))
               ag
               statements))
 
 ; get-argument: argument-graph symbol -> argument | #f
 (define (get-argument ag id)
   (table:lookup (argument-graph-arguments ag) id #f))
 
 ; add-string (list-of string) -> (list-of string)
 ; Add the string, s, the list l, if it is not already a member of l
 (define (add-string s l)
   (if (memq s l) l (cons s l)))
 
 ; ids->arguments: argument-graph (list-of symbol) -> (list-of argument)
 (define (ids->arguments ag ids)
   (filter (lambda (arg) (not (eq? arg #f)))
           (map (lambda (arg-id) (get-argument ag arg-id))
                ids)))
   
 ; arguments: argument-graph [statement] -> (list-of argument)
 ; all arguments pro and con some statement in an argument graph,
 ; or all arguments in the argument graph, if no statement is provided
 (define arguments
   (case-lambda ((ag s) ; argument-graph statement
                 (let ((n (get-node ag s)))
                   (if (not (node? n))
                       null
                       (ids->arguments ag (node-conclusion-of n)))))
                ((ag)  ; argument-graph
                 (table:objects (argument-graph-arguments ag)))))
 
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
 (define (accepted? ag s)
   (let ((n (get-node ag s)))
     (if (statement-positive? s)
         (eq? (node-status n) 'accepted)
         (eq? (node-status n) 'rejected))))

 ; rejected?: argument-graph statement -> boolean
 (define (rejected? ag s)
   (let ((n (get-node ag s)))
     (if (statement-positive? s)
         (eq? (node-status n) 'rejected)
         (eq? (node-status n) 'accepted))))
     
 ; decided?: argument-graph statement -> boolean
 (define (decided? ag s) (or (accepted? ag s) (rejected? ag s)))
 
 ; questioned?: argument-graph statement -> boolean
 (define (questioned? ag s)
   (let ((n (get-node ag s)))
     (eq? 'questioned (node-status n))))
 
 ; stated?: argument-graph statement -> boolean
 (define (stated? ag s)
    (let ((n (get-node ag s)))
     (eq? 'stated (node-status n))))
 
 ; issue?: argument-graph statement -> boolean
 ; An statement is an issue if it is undecided in the argument graph
 ; An acceptable statement is still an issue, due to nonmonotonicity:
 ; Additional arguments may make the statement unacceptable again.
 (define (issue? ag s) (not (decided? ag s)))

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
 
 ; statements: argument-graph [symbol] -> (list-of statement)
 ; returns the list of all statements in the argument graph
 (define statements 
   (case-lambda ((ag) ; retrieve all statements in the argument graph
                 (apply append (map (lambda (predicate)
                                      (table:keys (get-node-bucket ag predicate)))
                                    (table:keys (argument-graph-nodes ag)))))
                ((ag predicate) ; retrieve all statements with this predicate
                 (table:keys (get-node-bucket ag predicate)))))
 
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
 ; The "applicable" field of the argument is changed to #f before it is updated to assure
 ; the the acceptabiity of its conclusion is checked if the argument is in fact applicable.
 
 (define (assert-argument ag arg)
   (cond ((not (cycle-free? arg ag))
          ; just ignore the argument if it would introduce a cycle
          (if *debug*
              (printf "This argument would cause a cycle:~%~a~%" (argument->datum arg)))
          ag)
         (else (let* ((n (or (get-node ag (argument-conclusion arg))
                             (statement->node (argument-conclusion arg))))
                      ; update the node for the conclusion of the argument
                      (ag1 (add-node-to-argument-graph ag 
                                                       (make-node (node-statement n)
                                                                  (node-status n)
                                                                  (node-standard n)
                                                                  (node-acceptable n) ; updated below
                                                                  (node-complement-acceptable n) ; updated below
                                                                  (node-premise-of n)
                                                                  (add-string (argument-id arg) 
                                                                              (node-conclusion-of n)))))
                      ; update or create nodes for each of the premises of the argument
                      (ag2 (fold-right (lambda (p ag1)
                                         (let ((n (or (get-node ag1 (premise-atom p))
                                                      (statement->node (premise-atom p)))))
                                           
                                           (add-node-to-argument-graph ag1 
                                                                       (make-node (node-statement n)
                                                                                  (node-status n)
                                                                                  (node-standard n)
                                                                                  (node-acceptable n) ; updated below
                                                                                  (node-complement-acceptable n) ; updated below
                                                                                  (add-string (argument-id arg)
                                                                                              (node-premise-of n))
                                                                                  (node-conclusion-of n)))))
                                       ag1
                                       (argument-premises arg))))
                 (update-argument ag2 (if (not (argument-applicable arg)) ; safety check
                                          arg 
                                          (assign-applicability arg #f)))))))
 
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
   (let ((n (get-node ag s))) 
     (if (statement-positive? s)
         (node-acceptable n)
         (node-complement-acceptable n))))
 
 ; applicable?: argument-graph argument -> boolean
 ; Assumes that the applicability of the argument has been 
 ; previously computed or updated and stored in the applicable
 ; field of the argument.
 (define (applicable? ag arg) (argument-applicable arg))
 
 ; node-in? argument-graph node boolean -> boolean
 (define (node-in? ag n positive)
   (if positive
       (or (eq? (node-status n) 'accepted)
           (node-acceptable n))
       (or (eq? (node-status n) 'rejected)
           (node-complement-acceptable n))))
   
 ; in?: argument-graph statement -> boolean 
 ; looks up the cached "in" status of the statement in the agreement graph
 (define (in? ag s)
   (let ((n (get-node ag s)))
     (node-in? ag n (statement-positive? s))))
 
 ; out?: argument-graph statement -> boolean
 (define (out? ag s) (not (in? ag s)))
 
 ; holds?: argument-graph premise -> boolean
 (define (holds? ag p) 
   (let ((n (get-node ag (premise-atom p))))
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
           (else (error "holds: not a premise." p)))))

 
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
