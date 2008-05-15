(module lkif mzscheme
  ;; TODO: 
  ;;    * what to do with namespaced symbols?
  ;;    * condition attribute role is not parsed!
  ;;    * register non empty statements!!!!!
  
  ;; Carneades
  (require "rule.scm")
  (require "argument.scm")
  (require "statement.scm")
  ;; PLT
  (require (lib "match.ss"))                               ;; match
  (require (lib "list.ss"))                                ;; filter, foldl, foldr, find
  (require (lib "string.ss"))                              ;; regexp-split
  ;; SXML
  (require (planet "sxml.ss" ("lizorkin" "sxml.plt" 2 0)))
  
  (provide import export)
  
  ; *texts*: table mapping sexpressions to statement texts (see statement.scm)
  (define *texts* (make-hash-table 'equal))
  
   ; *statements* : (symbol -> statement) table
  (define *statements* (make-hash-table))
  
  ; type lkif-object: statement | rule | argument-graph
  
  ;; import: path [function-names ....] -> (list-of lkif-object)
  (define (import path)
    (let ((document (sxml:document path '())))
      (map create-objects (get-lkif-body document))))
  
  ; export: (list-of lkif-object) path -> void
  ; does not overwrite path
  (define (export l . path)    
    (let* ((objects (map object->sxml l))
           (statements (map object->sxml (hash-table-map 
                                          *texts*
                                          (lambda (key value) value))))
           (sxml-obj `(lkif ,@(append statements objects))))
      ; (display "debug: ") (display sxml-obj) (newline)
      (apply srl:sxml->xml (cons sxml-obj path)))) 
  
  (define (sexp->sxml sexp)
    (cond ((pair? sexp)
           (cons 's (map sexp->sxml sexp)))
          ((symbol? sexp)
           (string-append (symbol->string sexp) " "))
          (else 
           (let ((sp (open-output-string)))
             (display sexp sp)
             (string-append (get-output-string sp) " ")))))
  
  ; sexp->text: s-expression -> text
  ; converts an s-expression into a statement text
  (define (sexp->text sexp)
    (hash-table-get *texts* sexp 
                    (lambda () 
                      (let* ((id (gensym 's))
                             (txt (make-text id sexp (summarize sexp) "")))
                        (hash-table-put! *texts* sexp txt)
                        txt))))
  
  
  ; text->statement-in-sxml: text -> sxml
  ; converts an statement text into an sxml representation of 
  ; an LKIF statement 
  (define (text->statement-in-sxml txt)
    `(s (@ (id ,(symbol->string (text-id txt)))
           (summary ,(text-summary txt))
           (src ,(text-src txt)))
        ,@(map sexp->sxml 
               (if (pair? (text-statement txt))
                   (text-statement txt)
                   (list (text-statement txt))))))
  
  (define (boolean->string b)
    (if b "true" "false"))
  
  (define (polarity->string p)
    (if p "positive" "negative"))
  
  (define (role->string r)
    (if r (symbol->string r) "false"))
  
  
  ; condition->sexp: condition -> expression
  (define (condition->sexp condition)
    (define (statement->sxml s)
      (cond ((and (list? s) (eq? (car s) 'not))
             `(not ,(sexp->sxml (cdr s))))
            (else (sexp->sxml s))))
    (cond ((and (list? condition) (eq? (car condition) 'unless))
           `(unless ,(statement->sxml (cadr condition))))
          ((and (list? condition) (eq? (car condition) 'assuming))
           `(assuming ,(statement->sxml (cadr condition))))
          (else (statement->sxml condition))))
  
  ; clause->sexp: (list-of condition) -> expression
  (define (clause->sexp conditions)
    (if (< 1 (length conditions)) ; more than one condition
        `(and ,@(map condition->sexp conditions))
        (condition->sexp (car conditions))))
  
  (define (rule->sxml rule)
    (let ((name (symbol->string (rule-id rule)))
          (head (map sexp->sxml (rule-head rule)))
          (body (cond ((= 0 (length (rule-body rule))) null)
                      ((< 1 (length (rule-body rule)))
                       (list (cons 'or (map clause->sexp (rule-body rule)))))
                      (else (map condition->sexp (car (rule-body rule)))))))
      (if (null? (rule-body rule))
          `(rule (@ (id ,name)) ,@head)
          `(rule (@ (id ,name))
                 (head ,@head)
                 ; (body ,(body->sexp (rule-body rule))))))) 
                 (body ,@body)))))
  
  (define (argument-graph->sxml ag)
    `(argument-graph ,@(map argument->sexp (list-arguments ag))))
  
  (define (premises->sexp prem)
    (let ((atom     (premise-atom prem))
          (polarity (polarity->string (premise-polarity prem)))
          (role     (role->string (premise-role prem))))
      (cond 
        ((ordinary-premise? prem)
         `(premise (@  (role ,role)
                       (polarity ,polarity)
                       (statement ,(symbol->string (text-id (sexp->text atom)))))))
        ((exception? prem)
         `(exception (@ (role ,role) 
                        (polarity ,polarity)
                        (statement ,(symbol->string (text-id (sexp->text atom)))))))
        ((assumption? prem)
         `(assumption (@ (role ,role) 
                         (polarity ,polarity)
                         (statement ,(symbol->string (text-id (sexp->text atom)))))))
        (else (error "unknown premise")))))
  
  (define (argument->sexp arg)
    (let ((id (symbol->string (argument-id arg)))
          (scheme     (argument-scheme arg))
          (direction  (symbol->string (argument-direction arg)))
          (conclusion (sexp->text (argument-conclusion arg)))
          (premises   (map premises->sexp (argument-premises arg))))
      `(argument (@ (id ,id) (scheme ,scheme) (direction ,direction))
                 ,@premises
                 ;; FIXME: statements as attributes
                 (conclusion (@ (statement ,(symbol->string (text-id conclusion))))))))
  
  
  ;; checks type of lkif object and call 
  ;; corresponding function
  (define (object->sxml object)
    (cond 
      ((rule? object)
       (rule->sxml object))
      ((argument-graph? object)
       (argument-graph->sxml object))
      ((text? object)
       (text->statement-in-sxml object))
      (else (text->statement-in-sxml (sexp->text object)))))
 
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; import functions
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (get-lkif-body root)
    (match root [('*TOP* ('*PI* . p) ___ ('lkif . b)) b]))
  
  ;; calls create-(rule,string,ag) for every rule object
  (define (create-objects object)
    ;;(display object)(newline)
    (match object 
      [('rule . x)            (create-rule   x)]
      [('s . s)               (create-statement object)]
      [('argument-graph . ag) (create-argument-graph ag)]))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Rules
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; creates a rule struct
  (define (create-rule rule)
    ;;(write rule)(newline)
    (match-let* ([(attributes . rest) rule])
      (let ((name-value   (string->symbol (attr-value attributes 'id (gensym 'r))))
            (strict-value (attr-value attributes 'strict "no")))
        (if (match rest
              [(or (('head . head) ('body . body)) 
                   (('body . body) ('head . head))) #t]
              [_ #f])
            ;; THEN: head and body
            (match-let* ([(or (('head . head) ('body . body)) 
                              (('body . body) ('head . head))) rest]) ;;get body and head
              (make-rule name-value
                         (if (string=? strict-value "yes") #t #f)
                         (map create-statement head) 
                         (create-body body)))
            ;; ELSE: literal rule
            (let ((head (map create-statement rest))
                  (body null))
              (make-rule name-value
                         (if (string=? strict-value "yes") #t #f)
                         (map create-statement head) 
                         body))))))
  
  
  ; create-clause: (list-of (union condition conjunction)) -> (list-of (list-of condition))
  (define (create-clause l)
    (map (lambda (conjunct) 
           (if (and (list? conjunct) (eq? (car conjunct) 'and))
               (map create-condition (cdr conjunct))
               (create-condition conjunct)))
         l))
  
  ; create-body: expression -> (list-of clause)
  (define (create-body expr)
    (cond ((and (list? expr) (eq? (car expr) 'or))
           (map create-clause (cdr expr)))
          ((and (list? expr) (eq? (car expr) 'and))
           (list (create-clause (cdr expr))))
          (else (list (map create-condition expr)))))        
  
  (define (create-condition condition)
    ;; FIXME: parse for role attribute!
    (create-statement condition))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Statements
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ; create-statement: sxml -> statement | text
  (define (create-statement tree)
    (define (remove-attributes x)
      (not (and (pair? x)
                (eq? (car x) '@))))
    
    (define (split-string x)
      (if (string? x)
          (filter (lambda (x)
                    (> (string-length x) 0)) (regexp-split " +" x))
          x))
    
    (define (minimize-tree e l)
      (if (and (pair? e) (string? (car e)))
          ; (append (map string->symbol e) l)
          (append (map (lambda (s) (read (open-input-string s))) e)
                  l)
          (if (eq? e 's)
              l
              (cons e l))))
    
    
    (match tree
      [('s ('@ . attributes) . rest)
       (let* ((idv (attr-value attributes
                               'id 
                               #f))
              (id (if idv (string->symbol idv) (gensym 's)))
              (src (attr-value attributes 'src ""))
              (summary (attr-value attributes 'summary ""))
              (statement (if (pair? rest)
                             (map create-statement 
                                  (foldr minimize-tree '() 
                                         (map split-string rest)))
                             rest)))
         ;; register statement
         (if (not (null? statement)) (hash-table-put! *statements* id statement))
         (make-text id statement summary src))]
      [_  (if (pair? tree)
              (map create-statement 
                   (foldr minimize-tree '() 
                          (map split-string tree)))
              tree)]))
 
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Argument 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (get-statement attributes expr)
    (let ((id (string->symbol (attr-value attributes 'statement #f))))
      (if id
          ; use the id as a propositional letter if no statement is associated with the id.
          (let ((s (hash-table-get *statements* id (lambda () id))))
            ; (printf "debug: id=~v~n" id)
            ; (printf "debug: statement=~v~n" s)
            s)
          
          (let ((s (create-statement (car expr))))
           ; (printf "debug: expr=~v~n" expr)
           ;  (printf "debug: statement=~v~n" s)
            s))))
  
  (define (create-premise premise-expr) 
    (define (polarity attributes) 
      (string=? (attr-value attributes 'polarity "positive") "positive"))
    
    (define (role attributes) (attr-value attributes 'role ""))
    
    (match premise-expr
      ;; FIXME: attributes should be optional
      (('premise attributes . rest) 
       (make-ordinary-premise (get-statement attributes rest)
                              (polarity attributes)
                              (role attributes)))
      (('exception attributes . rest)
       (make-exception (get-statement attributes rest)
                       (polarity attributes)
                       (role attributes)))
      (('assumption attributes . rest) 
       (make-assumption (get-statement attributes rest)
                        (polarity attributes)
                        (role attributes)))
      (_ (error "create-premise: expression does not represent a premise" premise-expr))))
  
  
  (define (create-conclusion conclusion-expr)
    (match conclusion-expr
      (('conclusion attributes . rest)
       (get-statement attributes rest))
      (_ (error "create-conclusion: expression does not represent a conclusion" conclusion-expr))))
  
  (define (premise-expr? expr)
    (and (list? expr) (member (car expr) '(premise exception assumption))))
  
  (define (conclusion-expr? expr)
    (and (list? expr) (eq? (car expr) 'conclusion)))
    
  (define (create-argument arg)
    (let* ((attributes (car arg))
           (premises-or-conclusion (cdr arg))
           (id (string->symbol (attr-value attributes 'id (gensym 'a))))
           (direction (string->symbol (attr-value attributes 'direction "pro")))
           (premises (map create-premise (filter premise-expr? premises-or-conclusion)))
           (conclusion (create-conclusion (car (filter conclusion-expr? premises-or-conclusion))))
           (scheme (attr-value attributes 'scheme "")))
      (make-argument id direction conclusion premises scheme)))
    
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Argument Graph
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (create-argument-graph ag)
    (let ((arguments (map create-argument (match ag [(('argument . rest) ___ ) rest]))))
      (assert empty-argument-graph arguments)))
  

  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Attributes
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; get value or default value from (sxml) attributes  
  (define (attr-value attr-list name . default)
    ;;(display "attr-value")(write default)(newline)
    (let ((alist (match attr-list
                   [('@ . rest)        rest] 
                   [((a . b) ...) attr-list]
                   [_                   '()])))
      (if (assq name alist)
          (cadr (assq name alist))
          (if (not (null? default))
              (car default)
              #f))))
  )