#!r6rs

(library (dlp)
         
         (export)
         (import (rnrs)
                 (carneades match)
                 (carneades rule)
                 )
         
         ;----------------------
         ; Syntax:
         ;----------------------
         ; Standard-roles:         
         ;
         ; <role> = top | bottom | <rolename> | <rinverse> | <rtransitive>
         ; <rinverse> = (inverse <rolename>)
         ; <rtransitive> = (transitive-closure <rolename>)
         ;
         ;----------------------
         ; Standard-concepts:
         ; 
         ; <concept> = TOP | BOTTOM | <conceptname> | <cconjunction> 
         ;                 | <cdisjunction> | <univrestriction> | <existrestriction>
         ; <cconjunction> = (and <concept> <concept>+)
         ; <cdisjunction> = (or <concept> <concept>+)
         ; <univrestriction> = (all <rolen> <concept>)
         ; <existrestriction> = (some <role> <concept>)
         ;
         ;
         ; Lh - concepts from Lh can be mapped into the head of LP rules:
         ;
         ; <lhclass> = <conceptname> | <lhconjunction> | <lhunivrestrict>
         ; <lhconjunction> = (and <lhclass> <lhclass>)
         ; <lhunivrestrict> = (all <rolename> <lhclass>
         ;
         ;
         ; Lb - concepts from Lb can be mapped into the body of LP rules:
         ;
         ; <lbclass> = <conceptname> | <lbconjunction> | <lbdisjunction> | <lbexistrestrict>
         ; <lbconjunction> = (and <lbclass> <lbclass>)
         ; <lbdisjunction> = (or <lbclass> <lbclass>)
         ; <lbexistrestrict> = (some <rolename> <lbclass>)
         ;
         ;
         ; L  - L is the intersection of Lh and Lb:
         ;
         ; <lclass> = <conceptname> | <lconjunction>
         ; <lconjunction> = (and <lclass> <lclass>)
         ;
         ;----------------------
         ; Individuals:
         ;
         ; <individual> = <individualname> | <string> | <integer>
         ;
         ;----------------------
         ; Standard-statements:
         ;
         ; <requivalence> = (define-role <rolename> <role>)
         ; <rinclusion> = (define-primitive-role <rolename> <role>)
         ; <cequivalence> = (define-concept <conceptname> <concept>)
         ; <cinclusion> = (define-primitive-concept <conceptname> <concept>)
         ;
         ;
         ;----------------------
         ; Standard-assertions:
         ;
         ; <assertion> = <concinstance> | <roleinstance>
         ; <concinstance> = (instance <individualname> <concept>)
         ; <roleinstance> = (related <individualname> <individual> <rolename>)
         ;
         ;----------------------
         ; DLP:
         ;
         ; <dlp> = <dlpcinclusion> | <dlpcequivalence> | <dlprange> | <dlpdomain> 
         ;                         | <dlprinclusion> | <dlprequivalence> | <dlpinverse>
         ;                         | <dlptransitivity> | <dlpassertion>
         ; <dlpcinclusion> = (define-primitive-concept <lbclass> <lhclass>)
         ; <dlpcequivalence> = (define-concept <lclass> <lclass>)
         ; <dlprange> = (define-primitive-concept TOP <lhunivrestrict>)
         ; <ldpdomain> = (define-primitive-concept TOP (all <rinverse> <lhclass>))
         ; <dlprinclusion> = <rinclusion>
         ; <dlprequivalence> = <requivalence>
         ; <dlpinverse> = (define-role <rolename> <rinverse>)
         ; <dlptransitivity> = (define-primitive-role (transitive-closure R) R) for R in <rolename>
         ; <dlpassertion> = <dlpconcinstance> | <dlproleinstance>
         ; <dlpconcinstance> = (instance <individualname> <lhclass>)
         ; <dlproleinstance> = <roleinstance>
         ;
         ;----------------------
         
         
         
         ;----------------------
         ; Mapping (T) - maps all statements and assertions from <dlp> into an LP rule:
         ;----------------------
         ; Assertions:
         ;
         ; <dlpconcinstance> T(instance i C) := (rule Th(C i))
         ; <dlproleinstance> T(related i1 i2 R) := (rule (R i1 i2))
         ;
         ;----------------------
         ; Statements:
         ;
         ; <dlpcequivalence> (T (define-concept C1 C2)) := (T (define-primitive-concept C1 C2)) + (T(define-primitive-concept C2 C1))
         ; <dlpcinclusion>   (T (define-primitive-concept C1 C2)) := (rule (Th (C2 y)) (Tb (C1 y)))
         ; <dlprange>        (T (define-primitive-concept TOP (all R C))) := (rule (Th (C y)) (R x y))
         ; <dlpdomain>       (T (define-primtive-concept TOP (all (inverse R) C))) := (rule (Th (C x)) (R x y))
         ; <dlprinclusion>   (T (define-primtive-role R1 R2)) := (rule (R2 x y) (R1 x y))
         ; <dlprequivalence> (T (define-role R1 R2) := (rule (R2 x y) (R1 x y)) + (rule (R1 x y) (R2 x y))
         ; <dlpinverse>      (T (define-role R1 (inverse R2))) := (rule (R2 x y) (R1 y x)) + (rule (R1 y x) (R2 x y))
         ; <dlptransitivity> (T (define-role (transitive-closure R) R) := (rule (R x z) (and (R x y) (R y z)))
         ;
         ;----------------------
         ; Body-Mapping (Tb):
         ;
         ; <conceptname>     (Tb (C x)) := (C x)
         ; <lbconjunction>   (Tb ((and C1 C2) x)) := (and (Tb (C1 x)) (Tb (C2 x)))
         ; <lbdisjunction>   (Tb ((or C1 C2) x)) := (or (Tb (C1 x)) (Tb (C2 x)))
         ; <lbexistrestrict> (Tb ((some R C) x)) := (and (R x y) (Tb (C y)))
         ;
         ;----------------------
         ; Head-Mapping (Th):
         ;
         ; <conceptname>    (Th (C x)) := (C x)
         ; <lhconjunction>  (Th ((and C1 C2) x)) := (and (Th (C1 x)) (Th (C2 x)))
         ; <lhunivrestrict> (Th ((all R C) x)) := (rule (Th (C y)) (R x y))
         ;
         ;----------------------
         ; Rule-rewriting:
         ;
         ; (rule (and H1 H2) B) -> (rule H1 B) + (rule H2 B) ; no need to rewrite
         ; (rule (rule H1 H2) B) -> (rule H1 (and H2 B))
         ; (rule (rule H (or B1 B2)) -> (rule H B1) + (rule H B2) ; no need to rewrite
         ;
         ;----------------------
         
         (define symbolcounter 0)
         
         (define newsym
           (lambda ()
             (set! symbolcounter (+ symbolcounter 1))
             (string->symbol (string-append "?x" (number->string symbolcounter)))))

         (define (notsyntax s)
           (and
            (not (eq? s 'and))
            (not (eq? s 'or))
            (not (eq? s 'inverse))
            (not (eq? s 'transitive-closure))
            (not (eq? s 'all))
            (not (eq? s 'some))
            (not (eq? s 'define-role))
            (not (eq? s 'define-primitive-role))
            (not (eq? s 'define-concept))
            (not (eq? s 'define-primitive-concept))
            (not (eq? s 'instance))
            (not (eq? s 'related))
            (not (eq? s 'top))
            (not (eq? s 'bottom))
            (not (eq? s 'TOP))
            (not (eq? s 'BOTTOM))
            (not (eq? s 'rule))))
         
         ; ----------------------------
         ; standard-role syntax
         
         (define (rolename? r)
           (and
             (symbol? r)
             (notsyntax r)))
         
         (define (role? r)
           (or
            (eq? r 'top)
            (eq? r 'bottom)
            (rolename? r)
            (rinverse? r)
            (rtransitive? r)))
         
         (define (roles*? r*)
           (if (pair? r*)
               (if (role? (car r*))
                   (roles*? (cdr r*))
                   #f)
               (if (list? r*)
                   #t
                   (role? r*))))

         
         (define (rinverse? r)
           (and
            (pair? r)
            (= (length r) 2)
            (eq? (car r) 'inverse)
            (rolename? (cadr r))))
         
         (define (rtransitive? r)
           (and
            (pair? r)
            (= (length r) 2)
            (eq? (car r) 'transitive-closure)
            (rolename? (cadr r))))
           
         ; ----------------------------
         ; standard-concept syntax
             
         (define (conceptname? c)
           (and
             (symbol? c)
             (notsyntax c)))
         
         (define (concept? c)
           (or
            (eq? c 'TOP)
            (eq? c 'BOTTOM)
            (conceptname? c)
            (cconjunction? c)
            (cdisjunction? c)
            (univrestriction? c)
            (existrestriction? c)))
         
         (define (concepts*? c*)
           (if (pair? c*)
               (if (concept? (car c*))
                   (concepts*? (cdr c*))
                   #f)
               (if (list? c*)
                   #t
                   (concept? c*))))
         
         (define (cconjunction? c)
           (and
            (pair? c)
            (> (length c) 1)
            (eq? (car c) 'and)
            (concepts*? (cdr c))))
         
         (define (cdisjunction? c)
           (and
            (pair? c)
            (> (length c) 1)
            (eq? (car c) 'or)
            (concepts*? (cdr c))))
         
         (define (univrestriction? c)
           (and
            (pair? c)
            (= (length c) 3)
            (eq? (car c) 'all)
            (role? (cadr c))
            (concept? (caddr c))))
         
         (define (existrestriction? c)
           (and
            (pair? c)
            (= (length c) 3)
            (eq? (car c) 'some)
            (role? (cadr c))
            (concept? (caddr c))))
         
         ; ----------------------------
         ; Lh syntax
         
         (define (lhclass? c)
           (or
            (conceptname? c)
            (lhconjunction? c)
            (lhunivrestrict? c)))
         
         (define (lhconjunction? c)
           (and
            (pair? c)
            (= (length c) 3)
            (eq? (car c) 'and)
            (lhclass? (cadr c))
            (lhclass? (caddr c))))
         
         (define (lhunivrestrict? c)
           (and
            (pair? c)
            (= (length c) 3)
            (eq? (car c) 'all)
            (role? (cadr c))
            (lhclass? (caddr c))))
         
         ; ----------------------------
         ; Lb syntax
         
         (define (lbclass? c)
           (or
            (conceptname? c)
            (lbconjunction? c)
            (lbdisjunction? c)
            (lbexistrestrict? c)))
         
         (define (lbconjunction? c)
           (and
            (pair? c)
            (= (length c) 3)
            (eq? (car c) 'and)
            (lbclass? (cadr c))
            (lbclass? (caddr c))))
         
         (define (lbdisjunction? c)
           (and
            (pair? c)
            (= (length c) 3)
            (eq? (car c) 'or)
            (lbclass? (cadr c))
            (lbclass? (caddr c))))
         
         (define (lbexistrestrict? c)
           (and
            (pair? c)
            (= (length c) 3)
            (eq? (car  c) 'some)
            (role? (cadr c))
            (lbclass? (caddr c))))
         
         ; ----------------------------
         ; L syntax
         
         (define (lclass? c)
           (or
            (conceptname? c)
            (lconjunction? c)))
         
         (define (lconjunction? c)
           (and
            (pair? c)
            (= (length c) 3)
            (eq? (car c) 'and)
            (lclass? (cadr c))
            (lclass? (caddr c))))
            
         
         ; ----------------------------
         ; individual syntax
         
         (define (individualname? i)
          (and
             (symbol? i)
             (notsyntax i)))
         
         (define (individual? i)
           (or
            (individualname? i)
            (string? i)
            (integer? i)))
         
         
         ; ----------------------------
         ; standard-statement syntax
         
         (define (requivalence? s)
           (and
            (pair? s)
            (= (length s) 3)
            (eq? (car s) 'define-role)
            (rolename? (cadr s))
            (role? (caddr s))))
         
         (define (rinclusion? s)
           (and
            (pair? s)
            (= (length s) 3)
            (eq? (car s) 'define-primitive-role)
            (rolename? (cadr s))
            (role? (caddr s))))
         
         (define (cequivalence? s)
           (and
            (pair? s)
            (= (length s) 3)
            (eq? (car s) 'define-concept)
            (conceptname? (cadr s))
            (concept? (caddr s))))
         
         (define (cinclusion? s)
           (and
            (pair? s)
            (= (length s) 3)
            (eq? (car s) 'define-primitive-concept)
            (conceptname? (cadr s))
            (concept? (caddr s))))
         
         (define (statement? s)
           (or
            (requivalence? s)
            (rinclusion? s)
            (cequivalence? s)
            (cinclusion? s)))
         
         
         ; ----------------------------
         ; standard assertion syntax
         
         (define (concinstance? a)
           (and
            (pair? a)
            (= (length a) 3)
            (eq? (car a) 'instance)
            (individualname? (cadr a))
            (concept? (caddr a))))
         
         (define (roleinstance? a)
           (and
            (pair? a)
            (= (length a) 4)
            (eq? (car a) 'related)
            (individualname? (cadr a))
            (individual? (caddr a))
            (rolename? (cadddr a))))
         
         (define (assertion? a)
           (or
            (concinstance? a)
            (roleinstance? a)))
         
         ; ----------------------------
         ; DLP syntax
         
         (define (dlp? s)
           (or
            (dlpcinclusion? s)
            (dlpcequivalence? s)
            (dlprange? s)
            (dlpdomain? s)
            (dlprinclusion? s)
            (dlprequivalence? s)
            (dlpinverse? s)
            (dlptransitivity? s)
            (dlpassertion? s)))
         
         (define (dlpcinclusion? s)
           (and
            (pair? s)
            (= (length s) 3)
            (eq? (car s) 'define-primitive-concept)
            (lbclass? (cadr s))
            (lhclass? (caddr s))))
         
         (define (dlpcequivalence? s)
           (and
            (pair? s)
            (= (length s) 3)
            (eq? (car s) 'define-concept)
            (lclass? (cadr s))
            (lclass? (caddr s))))
         
         (define (dlprange? s)
           (and
            (pair? s)
            (= (length s) 3)
            (eq? (car s) 'define-primtive-concept)
            (eq? (cadr s) 'TOP)
            (lhunivrestrict? (caddr s))))
         
         (define (dlpdomain? s)
           (and
            (pair? s)
            (= (length s) 3)
            (eq? (car s) 'define-primitive-concept)
            (eq? (cadr s) 'TOP)
            (let ((d (caddr s)))
              (and
               (pair? d)
               (= (length d) 3)
               (eq? (car d) 'all)
               (rinverse? (cadr d))
               (lhclass? (caddr d))))))
         
         (define (dlprinclusion? s)
           (rinclusion? s))
         
         (define (dlprequivalence? s)
           (requivalence? s))
         
         (define (dlpinverse? s)
           (and
            (pair? s)
            (= (length s) 3)
            (eq? (car s) 'define-role)
            (rolename? (cadr s))
            (rinverse? (caddr s))))
         
         (define (dlptransitivity? s)
           (match s
             (('define-primitive-role ('transitive-closure r) r) #t)
             (_ #f))
           )
         
         (define (dlpassertion? a)
           (or
            (dlpconcinstance? a)
            (dlproleinstance? a)))
         
         (define (dlpconcinstance? a)
           (and
            (pair? a)
            (= (length a) 3)
            (eq? (car a) 'instance)
            (individualname? (cadr a))
            (lhclass? (caddr a))))
         
         (define (dlproleinstance? a)
           (roleinstance? a))
            
         
         ; ----------------------------
         ; syntax
         
         (define (syntax? s)
           (or
            (role? s)
            (concept? s)
            (individual? s)
            (statement? s)
            (assertion? s)))
         

         ; ----------------------------
         ; rule
         
         ; rules with only one argument are treated as rules with an empty body
         (define (rule? r)
           (and
            (pair? r)
            (<= (length r) 3)
            (eq? (car r) 'rule)))
         
         (define (rules*? r*)
           (if (pair? r*)
               (if (rule? (car r*))
                   (rules*? (cdr r*))
                   #f)
               (if (list? r*)
                   #t
                   (rule? r*))))
         
         ; ----------------------------
         ; head-mapping
         
         (define (to-head lh)
           (cond (                           ; <conceptname>
                  (and                       ; (Th (C ?x) := (C ?x)
                   (pair? lh)
                   (= (length lh) 2)
                   (conceptname? (car lh))
                   (symbol? (cadr lh)))
                  lh)
                 (                           ; <lhconjunction>
                  (and                       ; (Th ((and C1 C2) ?x)) := (and (Th (C1 ?x)) (Th (C2 ?x)))
                   (pair? lh)
                   (= (length lh) 2)
                   (lhconjunction? (car lh))
                   (symbol? (cadr lh)))
                  (let ((x (cadr lh)) (c (car lh)))
                    (list 'and (to-head (list (cadr c) x)) (to-head (list (caddr c) x)))))
                 (
                  (and                       ; <lhunivrestrict>
                   (pair? lh)                ; (Th ((all R C),x)) := (rule (Th (C,y)) (R x y))
                   (= (length lh) 2)
                   (lhunivrestrict? (car lh))
                   (symbol? (cadr lh)))
                  (let ((x (cadr lh)) (u (car lh)) (y (newsym)))
                    (list 'rule (to-head (list (caddr u) y)) (list (cadr u) x y))))))
                  
         
           
         ; ----------------------------
         ; body-mapping
         
         (define (to-body lb)
           (cond (                           ; <conceptname>
                  (and                       ; (Tb (C ?x) := (C ?x)
                   (pair? lb)
                   (= (length lb) 2)
                   (conceptname? (car lb))
                   (symbol? (cadr lb)))
                  lb)
                 (                           ; <lbconjunction>
                  (and                       ; (Tb ((and C1 C2) ?x)) := (and (Tb (C1 ?x)) (Tb (C2 ?x)))
                   (pair? lb)
                   (= (length lb) 2)
                   (lbconjunction? (car lb))
                   (symbol? (cadr lb)))
                  (let ((x (cadr lb)) (c (car lb)))
                    (list 'and (to-body (list (cadr c) x)) (to-body (list (caddr c) x)))))
                 (                           
                  (and                       ; <lbdisjunction>
                   (pair? lb)                ; (Tb ((or C1 C2) ?x)) := (or (Tb (C1 ?x)) (Tb (C2 ?x)))
                   (= (length lb) 2)
                   (lbdisjunction? (car lb))
                   (symbol? (cadr lb)))
                  (let ((x (cadr lb)) (c (car lb)))
                    (list 'or (to-body (list (cadr c) x)) (to-body (list (caddr c) x)))))
                 (
                  (and                       ; <lbexistrestrict>
                   (pair? lb)                ; (Th ((all R C),x)) := (rule (Th (C,y)) (R x y))
                   (= (length lb) 2)
                   (lbexistrestrict? (car lb))
                   (symbol? (cadr lb)))
                  (let ((x (cadr lb)) (u (car lb)) (y (newsym)))
                    (list 'and (list (cadr u) x y) (to-body (list (caddr u) y)))))))
               
         
         ; ----------------------------
         ; assertion- & statement-mapping
         
         (define (to-rule l)
           (cond (
                  (dlpconcinstance? l)                               ; <dlpconcinstance> 
                  (list 'rule (to-head (list (caddr l) (cadr l)))))  ; T(instance i C) := (rule Th(C i))
                 (
                  (dlproleinstance? l)                               ; <dlproleinstance>
                  (list 'rule (list (cadddr l) (cadr l) (caddr l)))) ; T(related i1 i2 R) := (rule (R i1 i2))
                 (
                  (dlpcequivalence? l)                               ; <dlpcequivalence>
                  (list                                              ; (T (define-concept C1 C2)) := 
                   (to-rule (list 'define-primitive-concept (cadr l) (caddr l)))   ; (T (define-primitive-concept C1 C2)) +
                   (to-rule (list 'define-primitive-concept (caddr l) (cadr l))))) ; (T(define-primitive-concept C2 C1))
                 (
                  (dlpcinclusion? l)                                 ; <dlpcinclusion>
                  (let ((x (newsym)))                                ; (T (define-primitive-concept C1 C2)) := 
                    (list 'rule (to-head (list (caddr l) x)) (to-body (list (cadr l) x))))) ; (rule (Th (C2 y)) (Tb (C1 y)))
                 (
                  (dlprange? l)                                      ; <dlprange>
                  (let ((x (newsym)) (y (newsym)))                   ; (T (define-primitive-concept TOP (all R C))) := 
                    (list 'rule (to-head (list (caddr (caddr l)) y)) (list (cadr (caddr l)) x y)))) ; (rule (Th (C y)) (R x y))
                 (
                  (dlpdomain? l)                                     ; <dldomain>
                  (let ((x (newsym)) (y (newsym)))                   ; (T (define-primitive-concept TOP (all (inverse R) C))) := 
                    (list 'rule (to-head (list (caddr (caddr l)) x)) (list (cadr (caddr l)) x y)))) ; (rule (Th (C x)) (R x y))
                 (
                  (dlprinclusion? l)                                 ; <dlprinclusion>
                  (let ((x (newsym)) (y (newsym)))                   ; (T (define-primtive-role R1 R2)) := 
                    (list 'rule (list (caddr l) x y) (list (cadr l) x y)))) ; (rule (R2 x y) (R1 x y))
                 (
                  (dlprequivalence? l)                               ; <dlprequivalence>
                  (list                                              ; (T (define-role R1 R2) := 
                   (let ((x (newsym)) (y (newsym)))
                     (list 'rule (list (caddr l) x y) (list (cadr l) x y)))   ; (rule (R2 x y) (R1 x y)) + 
                   (let ((x (newsym)) (y (newsym)))
                     (list 'rule (list (cadr l) x y) (list (caddr l) x y))))) ; (rule (R1 x y) (R2 x y))
                 (
                  (dlpinverse? l)                                    ; <dlpinverse>
                  (list                                              ; (define-role R1 (inverse R2))) := 
                   (let ((x (newsym)) (y (newsym)))
                     (list 'rule (list (cadr (caddr l)) x y) (list (cadr l) y x)))   ; (rule (R2 x y) (R1 y x)) + 
                   (let ((x (newsym)) (y (newsym)))
                     (list 'rule (list (cadr l) y x) (list (cadr (caddr l)) x y))))) ; (rule (R1 y x) (R2 x y))
                 (
                  (dlptransitivity? l)                               ; <dlptransitivity>
                  (let ((x (newsym)) (y (newsym)) (z (newsym)) (r (caddr l))) ; (T (define-role (transitive-closure R) R) :=
                    (list 'rule (list r x z) (list 'and (list r x y) (list r y z))))) ;  (rule (R x z) (and (R x y) (R y z)))
                  ))
         
         ; ----------------------------
         ; rule-rewriting
         
         (define (rulerewrite r)
           (if (rule? r)
               (if (rule? (cadr r))
                   (let ((h (rulerewrite (cadr r))))
                     (list 'rule (cadr h) (list 'and (caddr h) (caddr r))))
                   r)
               (begin
                 (display "no rule: ")
                 (display r)
                 (list 'rule))))
         
         ; ----------------------------
         ; ontology-syntax
         
         (define (ontology oname ont)
           oname)
)