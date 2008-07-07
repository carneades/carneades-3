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
         make-argument argument? argument-id argument-direction 
         argument-conclusion argument-premises argument-scheme pro con
         define-argument argument->datum datum->argument add-premise status? proof-standard? 
         complementary-proof-standard make-context default-context
         context? question accept reject assign-standard schemes-applied 
         status proof-standard prior decided? accepted? rejected?
         questioned? stated? issue? empty-argument-graph argument-graph? assert* assert 
         update questions facts statements accepted-statements rejected-statements 
         stated-statements relevant-statements list-arguments issues relevant?   
         satisfies? acceptable? holds? all-premises-hold? in? out? 
         node? node-statement node-pro node-con get-node get-argument
         list->argument-graph instantiate-argument-graph)
 
 (import (except (rnrs) assert)
         (rnrs records syntactic)
         (rnrs lists)
         (carneades gensym)
         (carneades statement)
         (carneades lib match)
         (prefix (carneades table) table:)
         (prefix (only (carneades lib srfi lists) lset-union any every) list:)
         (prefix (srfi/67 compare) compare:)
         )
 
 
 (define null '())
 
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
        (statement-equal? (premise-atom p1)
                          (premise-atom p2))))
 
 (define (direction? sym) (member sym '(pro con)))
 
 (define-record-type argument 
   (fields id           ; symbol
           direction    ; 'pro | 'con
           conclusion   ; statement
           premises     ; premise list
           scheme))     ; a string describing or naming the scheme applied
 
 ; Implicit premises, i.e. enthymemes, may be revealed using the  add-premise function.
 
 (define (pro id conclusion premises)
   (make-argument id 'pro conclusion premises ""))
 
 (define (con id conclusion premises)
   (make-argument id 'con conclusion premises ""))
 
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
                     (get-attr attributes 'direction 'pro)
                     (get-conclusion l)
                     (get-premises l)
                     (get-attr attributes 'scheme "")))
     (_ (error "expression does not represent an argument" sexp))))
 
 ; add-premise: premise argument -> argument
 (define (add-premise p arg)
   (make-argument (argument-id arg)
                  (argument-direction arg)
                  (argument-conclusion arg)
                  (cons p (argument-premises arg))
                  (argument-scheme arg)))
 
 (define (status? sym) (member sym '(stated questioned accepted rejected))) 
 
 (define (proof-standard? sym)
   (member sym '(se        ; scintilla of the evidence
                 ~se       ; complement of se
                 nse       ; negation of se
                 n~se      ; negation of ~se
                 ba        ; best argument (was "preponderance of the evidence")
                 ~ba       ; complement of ba
                 nba       ; negation of ba
                 n~ba      ; negation of ~ba
                 dv        ; dialectical validity                  
                 ~dv       ; complement of dv
                 ndv       ; negation of dv
                 n~dv      ; negation of ~dv
                 brd       ; beyond a reasonable doubt
                 ~brd      ; complement of brd
                 nbrd      ; negation of brd
                 n~brd     ; negation of ~brd
                 )))   
 
 (define (complementary-proof-standard ps)
   (case ps
     ((se) '~se)
     ((~se) 'se)
     ((ba) '~ba)
     ((~ba) 'ba)
     ((dv) '~dv)
     ((~dv) 'dv)
     ((brd) '~brd)
     ((~brd) 'brd)))
 
 (define-record-type context 
   (fields status        ; statement -> status table
           standard      ; statement -> standard
           compare))     ; argument * argument -> {-1, 0, 1}   
 
 (define default-context 
   (make-context (table:make-table) 
                 (lambda (statement) 'dv) ; default standard: dialectical validity
                 (lambda (arg1 arg2) 0))) ; default order: all arguments equally strong
 
 
 ; status: context statement -> status
 (define (status c s)
   (let ((v (table:lookup (context-status c) (statement-atom s) 'stated)))
     (if (statement-positive? s)
         v
         (case v 
           ((stated) 'stated)
           ((questioned) 'questioned)
           ((accepted) 'rejected)
           ((rejected) 'accepted)))))
 
 ; proof-standard: context statement -> standard
 (define (proof-standard c s) 
   (let ((v ((context-standard c) (statement-atom s))))
     (if (statement-positive? s)
         v
         (complementary-proof-standard v))))
 
 ; prior: context argument argument -> boolean
 (define (prior c a1 a2) (= ((context-compare c) a1 a2) 1))
 
 ; question: context (list-of statement) -> context
 (define (question context statements)
   (fold-right (lambda (s c) 
                 (make-context (table:insert (context-status c) 
                                             (statement-atom s) 
                                             'questioned)
                               (context-standard c) 
                               (context-compare c)))
               context 
               statements))
 
 
 ; accept: context (list-of statement) -> context
 (define (accept context statements)
   (fold-right (lambda (s c) 
                 (make-context (table:insert (context-status c)
                                             (statement-atom s)
                                             (if (statement-positive? s)
                                                 'accepted
                                                 'rejected))
                               (context-standard c) 
                               (context-compare c)))
               context 
               statements))
 
 
 ; reject: context (list-of statement) -> context
 (define (reject context statements)
   (fold-right (lambda (s c) 
                 (make-context (table:insert (context-status c)
                                             (statement-atom s)
                                             (if (statement-positive? s)
                                                 'rejected
                                                 'accepted)) 
                               (context-standard c) 
                               (context-compare c)))
               context
               statements))
 
 ; assign-standard: context  proof-standard (list-of statement) -> context
 (define (assign-standard context ps statements)
   (fold-right (lambda (s1 c) 
                 (make-context (context-status c)
                               (lambda (s2) 
                                 (if (statement-equal? (statement-atom s1) s2)
                                     ps
                                     ((context-standard c) s1)))
                               (context-compare c)))
               context
               statements))
 
 (define-record-type node 
   (fields statement ; datum
           pro       ; (list-of argument)
           con       ; (list-of argument)
           ))
 
 (define-record-type argument-graph
   (fields nodes        ; table: statement -> node
           arguments))  ; table: argument id -> argument 
 
 (define empty-argument-graph 
   (make-argument-graph (table:make-table)
                        (table:make-table)))
 
 ; put-node: argument-graph node -> argument-graph 
 (define (put-node ag n)
   (make-argument-graph (table:insert (argument-graph-nodes ag) (node-statement n) n)
                        (argument-graph-arguments ag)))
 
 ; get-node: argument-graph statement -> node | #f
 (define (get-node ag s)
   (table:lookup (argument-graph-nodes ag) s #f))
 
 ; get-nodes: argument-graph -> (list-of node)
 (define (get-nodes ag) (table:values (argument-graph-nodes ag)))
 
 ; put-argument argument-graph argument -> argument-graph
 (define (put-argument ag arg)
   ; first create and insert nodes for any new premises
   (let ((ag1 (fold-right (lambda (p ag1)
                            (if (get-node ag1 (premise-atom p))
                                ag1
                                (put-node ag1 (make-node (premise-atom p) null null ))))
                          ag
                          (argument-premises arg))))
     (make-argument-graph (argument-graph-nodes ag1)
                          (table:insert (argument-graph-arguments ag1)(argument-id arg) arg))))
 
 ; get-argument: argument-graph symbol -> argument | #f
 (define (get-argument ag id)
   (table:lookup (argument-graph-arguments ag) id #f))
 
 ; list-arguments: argument-graph -> (list-of argument)
 (define (list-arguments ag) (table:values (argument-graph-arguments ag)))
 
 ; instantiate-argument-graph: argument-graph substitutions -> argument-graph
 ; replace any variables in the statements of the arguments in the 
 ; argument graph with their values in the substitutions
 (define (instantiate-argument-graph ag subs)
   (list->argument-graph 
    (map (lambda (arg) (subs (argument->datum arg)))
         (list-arguments ag))))
 
 ; add-string: string (list-of string) -> (list-of string)
 ; Add the string, s, the list l, if it is not already a member of l
 (define (add-string s l)
   (if (memq s l) l (cons s l)))
 
 ; pro-arguments: argument-graph statement  -> (list-of argument)
 (define (pro-arguments ag s)
   (let ((n (get-node ag s)))
     (if (not (node? n))
         null
         (fold-right (lambda (id l) 
                       (let ((arg (get-argument ag id)))
                         (if (not arg) l (cons arg l))))
                     null
                     (node-pro n)))))
 
 ; con-arguments: argument-graph statement -> (list-of argument)
 (define (con-arguments ag s)
   (let ((n (get-node ag s)))
     (if (not (node? n))
         null
         (fold-right (lambda (id l) 
                       (let ((arg (get-argument ag id)))
                         (if (not arg) l (cons arg l))))
                     null
                     (node-con n)))))
 
 ; schemes-applied:argument-graph statement -> (list-of symbol)
 (define (schemes-applied ag s)
   (let ((n (get-node ag s))
         (f (lambda (id l) 
              (let ((arg (get-argument ag id)))
                (if (not arg) l (cons (argument-scheme arg) l))))))
     (if (not (node? n))
         null
         (let ((pro-schemes  (fold-right f null (node-pro n)))
               (con-schemes (fold-right f null (node-con n))))
           (append pro-schemes con-schemes)))))
 
 
 ; accepted?: context statement -> boolean
 (define (accepted? c s)
   (let ((v (table:lookup (context-status c)  (statement-atom s) 'undecided)))
     (if (statement-positive? s)
         (eq? v 'accepted)
         (eq? v 'rejected))))
 
 ; rejected?: context statement -> boolean
 (define (rejected? c s)
   (let ((v (table:lookup (context-status c) (statement-atom s) 'undecided)))
     (if (statement-positive? s)
         (eq? v 'rejected)
         (eq? v 'accepted))))   
 
 ; decided?: context statement -> boolean
 (define (decided? c s)
   (member (table:lookup  (context-status c) (statement-atom s) 'undecided)
           '(rejected accepted)))
 
 ; questioned?: context statement -> boolean
 (define (questioned? c s)
   (eq? 'questioned (table:lookup (context-status c)  
                                  (statement-atom s)
                                  'undecided)))
 
 ; stated?: context statement -> boolean
 (define (stated? c s)
   (eq? 'stated (table:lookup (context-status c) 
                              (statement-atom s)
                              'undecided)))
 
 ; issue?: context statement -> boolean
 ; a statement is a an issue if it is undecided in the context
 ; An acceptable statement is still an issue, due to nonmonotonicity:
 ; Additional arguments may make the statement unacceptable again.
 (define (issue? c s) (not (decided? c s)))
 
 ; questions: argument-graph context -> (list-of statement)
 (define (questions ag c)
   (filter (lambda (s) (questioned? c s)) (statements ag)))
 
 ; facts: argument-graph context -> (list-of statement)
 (define (facts ag c)
   (map (lambda (s) 
          (if (rejected? c s)
              `(not ,s)
              s))
        (filter (lambda (s) (decided? c s)) (statements ag))))
 
 ; accepted-statements: context -> (list-of statement)
 (define (accepted-statements c)
   (table:filter-keys (lambda (pair) (eq? (cdr pair) 'accepted)) (context-status c)))
 
 ; rejected-statements: context -> (list-of statement)
 (define (rejected-statements c)
   (table:filter-keys (lambda (pair) (eq? (cdr pair) 'rejected)) (context-status c)))
 
 ; stated-statements: argument-graph context -> (list-of statement)
 (define (stated-statements ag c)
   (filter (lambda (s) (stated? c s)) (statements ag)))
 
 ; issues: argument-graph context -> (list-of statement)
 (define (issues ag c)
   (filter (lambda (s) (not (decided? c s))) (statements ag)))
 
 ; all-premises: argument-graph statement -> (list-of premise)
 ; Returns the set of all the premises of all arguments pro or con the 
 ; statement s in the argument graph ag. The set of premises is represented as a list.         
 (define (all-premises ag s)
   (fold-right (lambda (arg l) 
                 (list:lset-union premise=? (argument-premises arg) l))
               null
               (append (pro-arguments ag s) (con-arguments ag s))))
 
 ; depends-on?: argument-graph statement statement -> boolean
 ; s1 depends on s2 in ag if s1 equals s2 or, recursively, some premise 
 ; of some argument pro or con s1 depends on s2 in ag. 
 (define (depends-on? ag s1 s2)
   (or (statement-equal? s1 s2)
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
   (map node-statement (table:values (argument-graph-nodes ag))))
 
 ; relevant-statements: argument-graph statement -> (list-of statement)
 ; (relevant-statements ag g) finds the statements in ag which are
 ; relevant for proving g. Since only nodes in the graph are 
 ; considered, g itself will be a member of the resulting list
 ; only if it has a node in the argument graph.
 
 (define (relevant-statements ag g)
   (filter (lambda (s) (relevant? ag s g))
           (statements ag))) 
 
 ; assert*: argument-graph argument boolean -> argument-graph
 ; (assert* arg ag r):  Add the argument, arg, to the argument graph, ag,
 ; if doing so would not introduce a cycle. "r" is a flag
 ; for choosing whether or not to replace an argument having
 ; the same id as arg.  If r is false and some argument with
 ; this id exists in ag, then arg is not put asserted, otherwise 
 ; arg replaces the prior argument. An entry for a statement is added 
 ; to the node table of the argument graph if one does not yet exist. 
 ; It is the responsiblity of the protocol to question the
 ; conclusion of the argument, if this is wanted.
 
 (define (assert* ag arg replace)
   (cond ((and (not replace) 
               (get-argument ag (argument-id arg)))
          (error "assert*: attempt to replace an existing argument." arg))
         ((not (cycle-free? arg ag))
          (error "assert*: cyclic argument." arg))
         (else (let* ((n (or (get-node ag (argument-conclusion arg))
                             (make-node (argument-conclusion arg) null null)))
                      (ag1 (if (eq? (argument-direction arg) 'pro)
                               (put-node ag (make-node (node-statement n)
                                                       (add-string (argument-id arg) 
                                                                   (node-pro n))
                                                       (node-con n)))
                               (put-node ag (make-node (node-statement n)
                                                       (node-pro n)
                                                       (add-string (argument-id arg) 
                                                                   (node-con n)))))))
                 (put-argument ag1 arg)))))
 
 ; assert: argument-graph (list-of argument) -> argument-graph
 (define (assert ag args) 
   (fold-right (lambda (arg ag) (assert* ag arg #f))
               ag 
               args))
 
 ; list->argument-graph: (list-of datum) -> argument-graph
 ; converts a list of expressions representing arguments into an argument graph
 (define (list->argument-graph l)
   (fold-right (lambda (arg ag) (assert* ag arg #f))
               empty-argument-graph 
               (map datum->argument l)))
 
 ; update: argument-graph (list-of argument) -> argument-graph
 (define (update ag args)
   (fold-right (lambda (arg ag) (assert* ag arg #t))
               ag args))
 
 ; satisfies: argument-graph context statement proof-standard -> boolean
 (define (satisfies? ag c s ps)
   (if (statement-negative? s)
       (satisfies? ag c (statement-atom s) (complementary-proof-standard ps))
       (let ((n (get-node ag s)))
         (if (not n) 
             #f
             (case ps
               ((se) (scintilla? ag c n)) 
               ((~se) (~scintilla? ag c n))
               ((nse) (not (scintilla? ag c n)))
               ((n~se) (not (~scintilla? ag c n)))
               ((ba) (best-argument? ag c n))
               ((~ba) (~best-argument? ag c n))
               ((nba) (not (best-argument? ag c n)))
               ((n~ba) (not (~best-argument? ag c n)))
               ((dv) (dialectically-valid? ag c n))
               ((~dv) (~dialectically-valid? ag c n))
               ((ndv) (not (dialectically-valid? ag c n)))
               ((n~dv) (not (~dialectically-valid? ag c n)))
               ((brd) (beyond-reasonable-doubt? ag c n))
               ((~brd) (~beyond-reasonable-doubt? ag c n))
               ((nbrd) (not (beyond-reasonable-doubt? ag c n)))
               ((n~brd) (not (~beyond-reasonable-doubt? ag c n)))
               (else (error "satisfies: unknown proof standard." ps)))))))
 
 ; acceptable?: argument-graph context statement -> boolean
 (define (acceptable? ag c s)
   (satisfies? ag c s (proof-standard c (statement-atom s))))
 
 ; in?: argument-graph context statement -> boolean
 (define (in? ag c s)
   (or (accepted? c s) (acceptable? ag c s)))
 
 ; out?: argument-graph context statement -> boolean
 (define (out? ag c s)
   (not (in? ag c s)))
 
 ; holds?: argument-graph context premise -> boolean
 (define (holds? ag c p) 
   (let ((s (status c (premise-atom p))))
     (cond ((ordinary-premise? p)
            (case s
              ((accepted) (positive-premise? p))
              ((rejected) (negative-premise? p))
              ((questioned stated) 
               (if (positive-premise? p)
                   (acceptable? ag c (premise-statement p))
                   (acceptable? ag c (statement-complement (premise-statement p)))))))
           ((assumption? p)
            (case s
              ((stated) #t) ; whether the premise is positive or negative 
              ((accepted) (positive-premise? p))
              ((rejected) (negative-premise? p))
              ((questioned) 
               (if (positive-premise? p)
                   (acceptable? ag c (premise-statement p))
                   (acceptable? ag c (statement-complement (premise-statement p)))))))
           ((exception? p)
            (case s
              ((accepted) (negative-premise? p))
              ((rejected) (positive-premise? p))
              ((stated questioned) 
               (if (positive-premise? p)
                   (not (acceptable? ag c (premise-statement p)))
                   (not (acceptable? ag c (statement-complement (premise-statement p)))))
               )))
           (else (error "holds: not a premise." p)))))
 
 ; all-premises-hold?: argument-graph context argument -> boolean
 ; Was called "defensible" in the 2007 AIJ article on Carneades, but I now
 ; prefer this more descriptive and presumably less confusing term.
 (define (all-premises-hold? ag c arg)
   (list:every (lambda (p) (holds? ag c p)) (argument-premises arg)))
 
 ; scintilla?: argument-graph context node -> boolean
 (define (scintilla? ag c n)
   (list:any (lambda (arg) (all-premises-hold? ag c arg)) 
             (pro-arguments ag (node-statement n))))
 
 (define (~scintilla? ag c n)  
   (list:any (lambda (arg) (all-premises-hold? ag c arg)) 
             (con-arguments ag (node-statement n))))
 
 ; dialectically-valid?:  argument-graph context node -> boolean
 (define (dialectically-valid? ag c n)
   (and (scintilla? ag c n)
        (not (list:any (lambda (arg) (all-premises-hold? ag c arg))
                       (con-arguments ag (node-statement n))))))
 
 (define (~dialectically-valid? ag c n)
   (and (~scintilla? ag c n)
        (not (list:any (lambda (arg) (all-premises-hold? ag c arg))
                       (pro-arguments ag (node-statement n))))))
 
 
 ; beyond-reasonable-doubt?: argument-graph context node -> boolean
 (define (beyond-reasonable-doubt? ag c n)
   (and (scintilla? ag c n)
        (list:every (lambda (arg) (all-premises-hold? ag c arg))
                    (pro-arguments ag (node-statement n)))
        (not (list:any (lambda (arg) (all-premises-hold? ag c arg))
                       (con-arguments ag (node-statement n))))))
 
 
 (define (~beyond-reasonable-doubt? ag c n)
   (and (~scintilla? ag c n)
        (list:every (lambda (arg) (all-premises-hold? ag c arg))
                    (con-arguments ag (node-statement n)))
        (not (list:any (lambda (arg) (all-premises-hold? ag c arg))
                       (pro-arguments ag (node-statement n))))))
 
 ; best-argument?: argument-graph context node -> boolean
 ; changed name from "preponderance of the evidence" as per Trevor's suggestion
 (define (best-argument? ag c n)
   (let* ((pro (filter (lambda (arg) (all-premises-hold? ag c arg))
                       (pro-arguments ag (node-statement n))))
          (con (filter (lambda (arg) (all-premises-hold? ag c arg))
                       (con-arguments ag (node-statement n))))
          (best-pro (and (not (null? pro))
                         (apply compare:max-compare 
                                (cons (context-compare c) pro))))
          (best-con (and (not (null? con))
                         (apply compare:max-compare 
                                (cons (context-compare c) con)))))
     (and (not (null? pro)) 
          (or (null? con)
              (= 1 ((context-compare c) best-pro best-con))))))
 
 (define (~best-argument? ag c n)
   (let* ((pro (filter (lambda (arg) (all-premises-hold? ag c arg))
                       (pro-arguments ag (node-statement n))))
          (con (filter (lambda (arg) (all-premises-hold? ag c arg))
                       (con-arguments ag (node-statement n))))
          (best-pro (and (not (null? pro))
                         (apply compare:max-compare
                                (cons (context-compare c) pro))))
          (best-con (and (not (null? con))
                         (apply compare:max-compare 
                                (cons (context-compare c) con)))))
     (and (not (null? con)) 
          (or (null? pro)
              (= 1 ((context-compare c) best-con best-pro))))))
 
 
 ) ; module argument
