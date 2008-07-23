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
 (carneades dlp-changed) ; description logic programming
 
 (export ontology axiom generate-arguments-from-ontologies dlp? to-rule dlprule? dlprules*? rulerewrite)
 (import (rnrs)
         (carneades lib match)
         (carneades rule)
         (carneades gensym)
         (carneades system)
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
 ; <univrestriction> = (all <role> <concept>)
 ; <existrestriction> = (some <role> <concept>)
 ;
 ;
 ; Lh - concepts from Lh can be mapped into the head of LP rules:
 ;
 ; <lhclass> = <conceptname> | <lhconjunction> | <lhunivrestrict>
 ; <lhconjunction> = (and <lhclass> <lhclass>+)
 ; <lhunivrestrict> = (all <rolename> <lhclass>
 ;
 ;
 ; Lb - concepts from Lb can be mapped into the body of LP rules:
 ;
 ; <lbclass> = <conceptname> | <lbconjunction> | <lbdisjunction> | <lbexistrestrict>
 ; <lbconjunction> = (and <lbclass> <lbclass>+)
 ; <lbdisjunction> = (or <lbclass> <lbclass>+)
 ; <lbexistrestrict> = (some <rolename> <lbclass>)
 ;
 ;
 ; L  - L is the intersection of Lh and Lb:
 ;
 ; <lclass> = <conceptname> | <lconjunction>
 ; <lconjunction> = (and <lclass> <lclass>+)
 ;
 ;----------------------
 ; Individuals:
 ;
 ; <individual> = <individualname> | <string> | <integer>
 ;
 ;----------------------
 ; Standard-statements: (statements with top or TOP as 2nd argument can also be written as unary statements)
 ;
 ; <requivalence> = (define-role <rolename> <role>) | (define-role <rolename>)
 ; <rinclusion> = (define-primitive-role <rolename> <role>) | (define-primitive-role <rolename>)
 ; <cequivalence> = (define-concept <conceptname> <concept>) | (define-concept <conceptname>)
 ; <cinclusion> = (define-primitive-concept <conceptname> <concept>) | (define-primitive-concept <conceptname>)
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
 ; <dlpcinclusion> = (define-primitive-concept <lbclass> <lhclass>) | (define-primitive-concept <lbclass>)
 ; <dlpcequivalence> = (define-concept <lclass> <lclass>) | (define-concept <lclass>)
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
 ; <lbconjunction>   (Tb ((and C1 C2 ...) x)) := (and (Tb (C1 x)) (Tb (C2 x)) ...)
 ; <lbdisjunction>   (Tb ((or C1 C2 ...) x)) := (or (Tb (C1 x)) (Tb (C2 x)) ...)
 ; <lbexistrestrict> (Tb ((some R C) x)) := (and (R x y) (Tb (C y)))
 ;
 ;----------------------
 ; Head-Mapping (Th):
 ;
 ; <conceptname>    (Th (C x)) := (C x)
 ; <lhconjunction>  (Th ((and C1 C2 ...) x)) := (and (Th (C1 x)) (Th (C2 x)) ...)
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
 (define axiomcounter 0)
 (define errorcounter 0)
 (define successcounter 0)
 
 (define newsym
   (lambda ()
     (set! symbolcounter (+ symbolcounter 1))
     (string->symbol (string-append "?xgen" (number->string symbolcounter)))))
 
 (define newaxiom
   (lambda (n)
     (set! axiomcounter (+ axiomcounter 1))
     (string->symbol (string-append (symbol->string n) "-" (number->string axiomcounter)))))
 
 
 (define initerror
   (lambda ()
     (set! errorcounter 0)))
  
 (define initsuccess
   (lambda ()
     (set! successcounter 0)))
 
 (define initaxiom
   (lambda ()
     (set! axiomcounter 0)))
 
 
 (define adderror
   (lambda ()
     (set! errorcounter (+ errorcounter 1))))
 
 (define addsuccess
   (lambda ()
     (set! successcounter (+ successcounter 1))))
 
 
 
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
    ;(not (eq? s 'top))
    ;(not (eq? s 'bottom))
    ;(not (eq? s 'TOP))
    ;(not (eq? s 'BOTTOM))
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
  
 (define (lhclasses*? c*)
   (if (pair? c*)
       (if (lhclass? (car c*))
           (lhclasses*? (cdr c*))
           #f)
       (if (list? c*)
           #t
           (lhclass? c*))))
 
 (define (lhconjunction? c)
   (and
    (pair? c)
    (>= (length c) 3)
    (eq? (car c) 'and)
    (lhclasses*? (cdr c))))
 
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
 
 (define (lbclasses*? c*)
   (if (pair? c*)
       (if (lbclass? (car c*))
           (lbclasses*? (cdr c*))
           #f)
       (if (list? c*)
           #t
           (lbclass? c*))))
 
 (define (lbconjunction? c)
   (and
    (pair? c)
    (>= (length c) 3)
    (eq? (car c) 'and)
    (lbclasses*? (cdr c))))
 
 (define (lbdisjunction? c)
   (and
    (pair? c)
    (>= (length c) 3)
    (eq? (car c) 'or)
    (lbclasses*? (cdr c))))
 
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
 
 (define (lclasses*? c*)
   (if (pair? c*)
       (if (lclass? (car c*))
           (lclasses*? (cdr c*))
           #f)
       (if (list? c*)
           #t
           (lclass? c*))))
 
 (define (lconjunction? c)
   (and
    (pair? c)
    (>= (length c) 3)
    (eq? (car c) 'and)
    (lclasses*? (cdr c))))
 
 
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
   (or
    (and
     (pair? s)
     (= (length s) 3)
     (eq? (car s) 'define-role)
     (rolename? (cadr s))
     (role? (caddr s)))
    (and
     (pair? s)
     (= (length s) 2)
     (eq? (car s) 'define-role)
     (rolename? (cadr s)))))
 
 (define (rinclusion? s)
   (or
    (and
     (pair? s)
     (= (length s) 3)
     (eq? (car s) 'define-primitive-role)
     (rolename? (cadr s))
     (role? (caddr s)))
    (and
     (pair? s)
     (= (length s) 2)
     (eq? (car s) 'define-primitive-role)
     (rolename? (cadr s)))))
 
 (define (cequivalence? s)
   (or
    (and
     (pair? s)
     (= (length s) 3)
     (eq? (car s) 'define-concept)
     (conceptname? (cadr s))
     (concept? (caddr s)))
    (and
     (pair? s)
     (= (length s) 2)
     (eq? (car s) 'define-concept)
     (conceptname? (cadr s)))))
 
 (define (cinclusion? s)
   (or
    (and
     (pair? s)
     (= (length s) 3)
     (eq? (car s) 'define-primitive-concept)
     (conceptname? (cadr s))
     (concept? (caddr s)))
    (and
     (pair? s)
     (= (length s) 2)
     (eq? (car s) 'define-primitive-concept)
     (conceptname? (cadr s)))))
 
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
   (or
    (and
     (pair? s)
     (= (length s) 3)
     (eq? (car s) 'define-primitive-concept)
     (lbclass? (cadr s))
     (lhclass? (caddr s)))
    (and
     (pair? s)
     (= (length s) 2)
     (eq? (car s) 'define-primitive-concept)
     (lbclass? (cadr s)))))
 
 (define (dlpcequivalence? s)
   (or
    (and
     (pair? s)
     (= (length s) 3)
     (eq? (car s) 'define-concept)
     (lclass? (cadr s))
     (lclass? (caddr s)))
    (and
     (pair? s)
     (= (length s) 2)
     (eq? (car s) 'define-concept)
     (lclass? (cadr s)))))
   
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
 (define (dlprule? r)
   (and
    (pair? r)
    (<= (length r) 3)
    (eq? (car r) 'rule)))
 
 (define (dlprules*? r*)
   (if (pair? r*)
       (if (dlprule? (car r*))
           (dlprules*? (cdr r*))
           #f)
       (if (list? r*)
           #t
           (dlprule? r*))))
 
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
          (and                       ; (Th ((and C1 C2 ...) ?x)) := (and (Th (C1 ?x)) (Th (C2 ?x)) ...)
           (pair? lh)
           (= (length lh) 2)
           (lhconjunction? (car lh))
           (symbol? (cadr lh)))
          (let ((x (cadr lh)) (c (car lh)))
            (cons 'and (map to-head (map (lambda (y) (list y x)) (cdr c))))))
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
            (cons 'and (map to-body (map (lambda (y) (list y x)) (cdr c))))))
         (                           
          (and                       ; <lbdisjunction>
           (pair? lb)                ; (Tb ((or C1 C2) ?x)) := (or (Tb (C1 ?x)) (Tb (C2 ?x)))
           (= (length lb) 2)
           (lbdisjunction? (car lb))
           (symbol? (cadr lb)))
          (let ((x (cadr lb)) (c (car lb)))
            (cons 'or (map to-body (map (lambda (y) (list y x)) (cdr c))))))
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
 
 ; to-rule: dlp -> dlprule
 (define (to-rule l)
   (cond (
          (dlpconcinstance? l)                               ; <dlpconcinstance> 
          (list 'rule (to-head (list (caddr l) (cadr l)))))  ; T(instance i C) := (rule Th(C i))
         (
          (dlproleinstance? l)                               ; <dlproleinstance>
          (list 'rule (list (cadddr l) (cadr l) (caddr l)))) ; T(related i1 i2 R) := (rule (R i1 i2))
         (
          (dlpcequivalence? l)                               ; <dlpcequivalence>
          (if (= (length l) 3)
              (list                                          ; (T (define-concept C1 C2)) := 
               (to-rule (list 'define-primitive-concept (cadr l) (caddr l)))  ; (T (define-primitive-concept C1 C2)) +
               (to-rule (list 'define-primitive-concept (caddr l) (cadr l)))) ; (T(define-primitive-concept C2 C1))
              (to-rule (append l '(TOP)))))
          
         (
          (dlpcinclusion? l)                                 ; <dlpcinclusion>
          (if (= (length l) 3)
              (let ((x (newsym)))                                ; (T (define-primitive-concept C1 C2)) := 
                (list 'rule (to-head (list (caddr l) x)) (to-body (list (cadr l) x)))) ; (rule (Th (C2 y)) (Tb (C1 y)))
              (to-rule (append l '(TOP)))))
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
          (if (= (length l) 3)
              (let ((x (newsym)) (y (newsym)))                   ; (T (define-primtive-role R1 R2)) := 
                (list 'rule (list (caddr l) x y) (list (cadr l) x y))) ; (rule (R2 x y) (R1 x y))
              (to-rule (append l '(top)))))
         (
          (dlprequivalence? l)                               ; <dlprequivalence>
          (if (= (length l) 3)
              (list                                              ; (T (define-role R1 R2) := 
               (let ((x (newsym)) (y (newsym)))
                 (list 'rule (list (caddr l) x y) (list (cadr l) x y)))   ; (rule (R2 x y) (R1 x y)) + 
               (let ((x (newsym)) (y (newsym)))
                 (list 'rule (list (cadr l) x y) (list (caddr l) x y)))) ; (rule (R1 x y) (R2 x y))
              (to-rule (append l '(top)))))
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
 
 ; rulerewrite: dlprule -> dlprule | dlprules* -> dlprules*
 (define (rulerewrite r)
   (if (dlprule? r)
       (if (dlprule? (cadr r))
           (let ((h (rulerewrite (cadr r))))
             (list 'rule (cadr h) (list 'and (caddr h) (caddr r))))
           r)
       (if (dlprules*? r)
           (map rulerewrite r)
           (assertion-violation "rulerewrite" "no rule" r))))
 
 ; ----------------------------
 ; ontology-syntax
 
 ; %axiom: symbol symbol -> (list-of ontology)
 (define (%axiom oname ont)
   (if (dlp? ont)
       (let ((r (rulerewrite (to-rule ont))))
         (begin
           (define make-ontology
             (lambda (n r)
               (if (= (length r) 3)
                   (make-rule n
                              #f
                              (make-rule-head (cadr r))
                              (make-rule-body (caddr r)))
                   (make-rule n
                              #f
                              (make-rule-head (cadr r))
                              '()))))
           (addsuccess)
           (if (dlprule? r)
               (list (make-ontology oname r))
               (if (dlprules*? r)
                   (map (lambda (r) (make-ontology (newaxiom oname) r)) r)
                   (error "ontology" "no good conversion" r)))))
       ;(assertion-violation "ontology" "error: no valid dlp ontology" ont)))
       (begin
         (adderror)
         (pretty-print "ontology-error - no valid dlp ontology: ")
         (pretty-print ont)
         (newline)
         '())))
 
 (define-syntax axiom
   (lambda (x)
     (syntax-case x ()
       ((_ oname ont) #'(%ontology (quote oname) (quote ont))))))
 
 ;empty-knowledgebase: -> knowledgebase
 (define empty-knowledgebase empty-rulebase)
 
 ; (define add-ontology add-rules)
  
 
 ; TODO: FIX
 ; add-ontologies: knowledgebase (list-of (list-of ontology)) -> knowledgebase
 (define (add-axioms kb o*)
   (add-rules kb (fold-right append '() o*)))
 
 ; %ontology: ontology (list-of ontology) ... -> knowledgebase
 (define (%ontology l)
   ;(display l)
   (newline)
   (display "Nr. of axioms read: ")
   (display successcounter)
   (newline)
   (display "Nr. of non dlp axioms: ")
   (display errorcounter)
   (newline)
   (add-rules empty-knowledgebase (fold-right append '() l)))
 
 (define-syntax ontology
   (syntax-rules ()
     ((_ oname axiom1 ...) (begin
                             (initerror)
                             (initsuccess)
                             (initaxiom)
                             (define oname
                               (%ontology (map (lambda (x)
                                                 ;(display x)
                                                 ;(newline)
                                                 (%axiom (gensym (string-append (symbol->string (quote oname)) "-axiom-")) x))
                                               (list (quote axiom1) ...))))))))
 
 ; generate-arguments-from-ontologies: knowledgebase (list-of question-types) -> generator
 (define generate-arguments-from-ontologies generate-arguments-from-rules)
 
 
 
 )