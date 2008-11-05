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

(library (carneades dnf)
         
         
         #|
             This library is implemented to convert every logical formula into an equivalent
             "disjunctive normalform" (DNF). A DNF is a disjunction of a conjunction of literals.
             You can find a more detailed, that is a more mathematic, description of logical
             formulas below.
             DNF is the only way, the "rule"-library can handle logical formulas. To support every
             valid formula, it is converted, before used.
             The algorithm used to convert the formulas to DNF runs through two main steps:
               - negations are only allowed before atoms (only in literals)
                   (not (not t1)) -> t1
                   (not (and t1 t2) -> (or (not t1) (not t2))
                   (not (or t1 t2) -> (and (not t1) (not t2))
               - usage of distributivity to bring inner disjunctions to an outer level
                   (and t1 (or t2 t3)) -> (or (and t1 t2) (and t1 t3))
                   (and (or t1 t2) t3) -> (or (and t1 t3) (and t2 t3))
             Before these steps could be run through, equivalences have to be substituted for
             implications, which themselves have to be substituted for disjunctions.
             You do not need to care, if you only use the "to-dnf" method.

             The library was extended to the use of the predicates "assuming" and "unless". These
             extensions were made in order to distinguish special rules for burden of proof.
             Throughout the conversions the predicates are handled as identity and negation.

             !!! BEWARE !!!
             Worst case conversion includes exponential time complexity. For example, a conversion
             from CNF to DNF with n variables could induce 2^n operations. As this library is especially
             designed for the "rules"-library and these rules won't be too long, this is ok for
             "carneades" to still run quick.
         |#
         
         
         (export to-dnf
                 dnf?
                 compare-formulas)
         
         (import (rnrs)
                 (rnrs eval))
         
         #|

      <atom>
      <literal> = <atom> | (not <atom>)
      <assumption> = (assuming <literal>)
      <exception> = (unless <literal>)
      <extliteral> = <literal> | <assumption> | <exception>                ; extended literals
      <negation> = (not <formula>)
      <conjunction> = (and <formula> <formula>+)                           ; pure conjunction
      <lconjunction> = <extliteral> | (and <extliteral> <extliteral>+)     ; conjunctions of extended literals
      <disjunction> = (or <formula> <formula>+)                            ; pure disjunction
      <implication> = (if <formula> <formula>)
      <equivalence> = (iff <formula> <formula>)
      <dnf> = <lconjunction> | (or <lconjunction> <lconjunction>+)         ; disjunctive normalform
      <formula> = <extliteral> | <conjunction> | <disjunction> | <negation> | <implication> | <equivalence>
         
      |#
         
         
         
         ;----------------------------------------------
         ; Predicates
         
         ; <formula> -> <bool>
         ; every symbol, except 'not 'and 'or 'if 'iff 'assuming 'unless
         (define (atom? formula)
           (or (not (list? formula))
               (if (> (length formula) 0)
                   (not (member (car formula)
                                '(not or and if iff unless assuming)))
                   #f)))
           
           #;(and
            (symbol? formula)
            (not (eq? 'not formula))
            (not (eq? 'and formula))
            (not (eq? 'or formula))
            (not (eq? 'if formula))
            (not (eq? 'iff formula))
            (not (eq? 'assuming formula))
            (not (eq? 'unless formula)))
         
         ; <formula> -> <bool>
         (define (literal? formula)
           (or
            (atom? formula)
            (and 
             (pair? formula)
             (= (length formula) 2)
             (eq? (car formula) 'not)
             (atom? (cadr formula)))))
         
         
         ; <formula> -> <bool>
         (define (assumption? formula)
           (and
            (pair? formula)
            (= (length formula) 2)
            (eq? (car formula) 'assuming)
            (literal? (cadr formula))))
         
         ; <formula> -> <bool>
         (define (exception? formula)
           (and
            (pair? formula)
            (= (length formula) 2)
            (eq? (car formula) 'unless)
            (literal? (cadr formula))))            
         
         ; <formula> -> <bool>
         (define (extliteral? formula)
           (or
            (literal? formula)
            (assumption? formula)
            (exception? formula)))
         
         ; <formula>* -> <bool>
         (define (literals*? formula)
           (if (pair? formula)
               (if (literal? (car formula))
                   (literals*? (cdr formula))
                   #f)
               (if (list? formula)
                   #t
                   (literal? formula))))
         
         ; <formula>* -> <bool>
         (define (extliterals*? formula)
           (if (pair? formula)
               (if (extliteral? (car formula))
                   (extliterals*? (cdr formula))
                   #f)
               (if (list? formula)
                   #t
                   (extliteral? formula))))
         
         ; <formula> -> <bool>
         (define (negation? formula)
           (and
            (pair? formula)
            (= (length formula) 2)
            (eq? (car formula) 'not)
            (formula? (cadr formula))))
         
         ; <formula> -> <bool>
         (define (conjunction? formula)
           (and
            (pair? formula)
            (> (length formula) 2)
            (eq? (car formula) 'and)
            (formulas*? (cdr formula))))
         
         ; <formula> -> <bool>
         (define (disjunction? formula)
           (and
            (pair? formula)
            (> (length formula) 2)
            (eq? (car formula) 'or)
            (formulas*? (cdr formula))))
         
         ; <formula> -> <bool>
         (define (lconjunction? formula)
           (or
            (extliteral? formula)
            (and
             (pair? formula)
             (> (length formula) 2)
             (eq? (car formula) 'and)
             (extliterals*? (cdr formula)))))
         
         ; <formula>* -> <bool>
         (define (lconjunctions*? formula)
           (if (pair? formula)
               (if (lconjunction? (car formula))
                   (lconjunctions*? (cdr formula))
                   #f)
               (if (list? formula)
                   #t
                   (lconjunction? formula))))
         
         ; <formula> -> <bool>
         (define (implication? formula)
           (and
            (pair? formula)
            (= (length formula) 3)
            (eq? (car formula) 'if)
            (formula? (cadr formula))
            (formula? (caddr formula))))
         
         ; <formula> -> <bool>
         (define (equivalence? formula)
           (and
            (pair? formula)
            (= (length formula) 3)
            (eq? (car formula) 'iff)
            (formula? (cadr formula))
            (formula? (caddr formula))))
         
         ; <formula> -> <bool>
         (define (dnf? formula)
           (or
            (lconjunction? formula)
            (and
             (pair? formula)
             (> (length formula) 2)
             (eq? (car formula) 'or)
             (lconjunctions*? (cdr formula)))))
         
         ; <formula> -> <bool>
         (define (formula? formula)
           (or
            (extliteral? formula)
            (negation? formula)
            (conjunction? formula)
            (disjunction? formula)
            (implication? formula)
            (equivalence? formula)))
         
         ; <formula>* -> <bool>
         (define (formulas*? formula)
           (if (pair? formula)
               (if (formula? (car formula))
                   (formulas*? (cdr formula))
                   #f)
               (if (list? formula)
                   #t
                   (formula? formula))))
         
         
         
         ;----------------------------------------
         ; Conversion functions
         
         ; (every conversion function implemented here is doing only logical equivalent conversions)
         
         
         ; TODO: support for equivalences and implications in all functions
         
         ; !!!!!!!!!!
         ; the functions "negation-conversion", "associative-conversion" and "distributive-conversion"
         ; should not be used, when there are still equivalences or implications in the formula
         ; at least, don't expect any success, because these functions will not work beyond any
         ; equivalence or implication
         
         ; <formula> -> <formula>
         ; gets a formula and returns a formula, where every occurence of an equivalence is substituted
         ; a<=>b -> a=>b and b=>a
         (define (equivalence-conversion formula)
           (cond
             ((equivalence? formula)
              (let ((rformula (map equivalence-conversion (cdr formula))))
                (list 'and
                      (list 
                       'if 
                       (car rformula)
                       (cadr rformula))
                      (list
                       'if
                       (cadr rformula)
                       (car rformula)))))
             ((literal? formula) formula)
             ((formula? formula) (cons (car formula) (map equivalence-conversion (cdr formula))))
             (else formula)))
         
         ; <formula> -> <formula>
         ; gets a formula and returns a formula, where every occurence of an implication is substituted
         ; a=>b -> (not a) or b
         (define (implication-conversion formula)
           (cond
             ((implication? formula)
              (let ((rcdrformula (map implication-conversion (cdr formula))))
                (list 'or (list 'not (car rcdrformula)) (cadr rcdrformula))))
             ((literal? formula) formula)
             ((formula? formula) (cons (car formula) (map implication-conversion (cdr formula))))
             (else formula)))
         
         ; <formula> -> <formula>
         ; the resulting formula is the negated formula
         (define (negate formula)
           (list 'not formula))
         
         ; <formula> -> <formula>
         ; the resulting formula has negations only in literals
         (define (negation-conversion formula)
           (cond
             ((literal? formula) formula)
             ((conjunction? formula) (cons (car formula) (map negation-conversion (cdr formula))))
             ((disjunction? formula) (cons (car formula) (map negation-conversion (cdr formula))))
             ((negation? formula) (cond
                                    ; formula = (not (not t)) -> t                    cadadr = t
                                    ((negation? (cadr formula)) (negation-conversion (cadadr formula)))
                                    ; formula = (not (and t1 t2 ...)) -> (or (not t1) (not t2) ...)
                                    ((conjunction? (cadr formula))
                                     (cons 'or (map negation-conversion (map negate (cdadr formula)))))
                                    ; formula = (not (or t1 t2 ...)) -> (and (not t1) (not t2) ...)
                                    ((disjunction? (cadr formula))
                                     (cons 'and (map negation-conversion (map negate (cdadr formula)))))
                                    ; formula = (not (assuming t)) -> (assuming (not t))
                                    ((assumption? (cadr formula))
                                     (list 'assuming (negation-conversion (cons 'not (cdadr formula)))))
                                    ; formula = (not (unless t)) -> (unless (not t))
                                    ((exception? (cadr formula))
                                     (list 'unless (negation-conversion (cons 'not (cdadr formula)))))
                                    (else (negation-conversion (cadr formula)))))
             (else formula)))
         
         
         ; <formula>* -> <formula>*
         ; gets a list of formulas
         ; returns those formulas in a list, which are disjunctions
         (define (get-disjunctions formulas*)
           (if (pair? formulas*)
               (if (disjunction? (car formulas*))
                   (cons (car formulas*) (get-disjunctions (cdr formulas*)))
                   (get-disjunctions (cdr formulas*)))
               formulas*))
         
         ; <formula>* -> <formula>*
         ; gets a list of formulas
         ; returns those formulas in a list, which are NO disjunctions
         (define (get-nodisjunctions formulas*)
           (if (pair? formulas*)
               (if (disjunction? (car formulas*))
                   (get-nodisjunctions (cdr formulas*))
                   (cons (car formulas*) (get-nodisjunctions (cdr formulas*))))
               formulas*))
         
         ; <formula>* x <formula> -> <disjunction>
         ; gets a list of formulas and a disjunction and returns a disjunction of conjunctions
         ; f.e.: '(a b c),'(or d e f) |-> (or (and d a b c) (and e a b c) (and f a b c))
         ; it us used as a single application of the distributive law, where the elements of the
         ;   list of formulas as well as the disjunction were operands of a conjunction
         (define (distri formulas* disj)
           (cons 'or (map (lambda (t) (cons 'and (cons t formulas*))) (cdr disj))))
         
         ; <formula> -> <formula>
         ; uses distributiv law to bring inner disjunctions to an outer level
         ; it works recursively, where nested expressions are converted from inside to outside
         ; after every conversion, a simplification through the associative law is used
         (define (distributive-conversion formula)
           (cond
             ((literal? formula) formula)
             ((negation? formula) (list (car formula) (distributive-conversion (cadr formula))))
             ((disjunction? formula) (cons (car formula) (map distributive-conversion (cdr formula))))
             ((conjunction? formula) ; if conjunction 
              ; first, apply the conversion recursively to all operands
              (let ((rformulas* (map distributive-conversion (cdr formula))))
                ; split the operands to disjunctions and no disjunctions
                (let ((disj* (get-disjunctions rformulas*)) (no-disj* (get-nodisjunctions rformulas*)))
                  (cond
                    ((= (length disj*) 0) (cons (car formula) rformulas*)) ; no disjunctions in the conjunction
                    ((= (length disj*) 1) ; one disjunction -> apply distributive law once
                     (associative-conversion (distri no-disj* (car disj*))))   
                    ((> (length disj*) 1) ; more than one disjunction -> apply distributive law once and start again
                     (distributive-conversion (associative-conversion (distri (append (cdr disj*) no-disj* ) (car disj*)))))  
                    ))))   
             (else formula))
           )
         
         ; <formula>* -> <formula>*
         ; gets a list of formulas, which are originally operands of a conjunction
         ; returns a list of formulas, in which no formula is a conjunction
         ; formulas, who were conjunctions, were substituted for their operands
         (define (con-flatlist formulas*)
           (if (pair? formulas*)
               (if (> (length formulas*) 1)
                   (if (conjunction? (car formulas*))
                       (append (con-flatlist (cdar formulas*)) (con-flatlist (cdr formulas*)))
                       (cons (car formulas*) (con-flatlist (cdr formulas*))))
                   (if (conjunction? (car formulas*))
                       (con-flatlist (cdar formulas*))
                       formulas*))
               formulas*))    
         
         ; <formula>* -> <formula>*
         ; gets a list of formulas, which are originally operands of a disjunction
         ; returns a list of formulas, in which no formula is a disjunction
         ; formulas, who were disjunctions, were substituted for their operands
         (define (dis-flatlist formulas*)
           (if (pair? formulas*)
               (if (> (length formulas*) 1)
                   (if (disjunction? (car formulas*))
                       (append (dis-flatlist (cdar formulas*)) (dis-flatlist (cdr formulas*)))
                       (cons (car formulas*) (dis-flatlist (cdr formulas*))))
                   (if (disjunction? (car formulas*))
                       (dis-flatlist (cdar formulas*))
                       formulas*))
               formulas*))
         
         
         ; <formula> -> <formula>
         ; uses associative law to simplify nested conjunctions and disjunctions to lists
         (define (associative-conversion formula)
           (cond
             ((literal? formula) formula)
             ((negation? formula) (list (car formula) (associative-conversion (cadr formula))))
             ((conjunction? formula) (cons (car formula) (con-flatlist (map associative-conversion (cdr formula)))))
             ((disjunction? formula) (cons (car formula) (dis-flatlist (map associative-conversion (cdr formula)))))
             (else formula)))
         
         ; <formula> -> <formula>
         ; converts every valid logical formula into disjunctive normalform
         (define (to-dnf formula)
           (if (formula? formula)
               (if (not (dnf? formula))
                   (associative-conversion
                    (distributive-conversion
                     (associative-conversion 
                      (negation-conversion 
                       (implication-conversion 
                        (equivalence-conversion formula))))))
                   formula)
               (begin
                 (newline)
                 (display "Error: no valid formula - ")
                 (display formula)
                 (newline)
                 '())))
         
         
         
         ; -----------------------------------------------------------------------
         ; Library testing ....
         
         (define (test-all formula)
           #;(define negt (negation-conversion formula))
           #;(define asst (associative-conversion negt))
           #;(define dist (distributive-conversion asst))
           (define impeqf (implication-conversion (equivalence-conversion formula)))
           (define dnft (to-dnf formula))
           (display "----------------------------------------------")
           (newline)
           (display "Testing formula < ")
           (display formula)
           (display " >")
           (newline)
           (display "Länge: ")
           (if (pair? formula)
               (display (length formula))
               (display 0))
           (newline)
           (display "  - atom?                   : ")
           (display (atom? formula))
           (newline)
           (display "  - literal?                : ")
           (display (literal? formula))
           (newline)
           (display "  - extliteral?             : ")
           (display (extliteral? formula))
           (newline)
           (display "  - negation?               : ")
           (display (negation? formula))
           (newline)
           (display "  - conjunction?            : ")
           (display (conjunction? formula))
           (newline)
           (display "  - lconjunction?           : ")
           (display (lconjunction? formula))
           (newline)
           (display "  - disjunction?            : ")
           (display (disjunction? formula))
           (newline)
           (display "  - implication?            : ")
           (display (implication? formula))
           (newline)
           (display "  - equivalence?            : ")
           (display (equivalence? formula))
           (newline)
           (display "  - dnf?                    : ")
           (display (dnf? formula))
           (newline)
           (display "  - formula?                : ")
           (display (formula? formula))
           (newline)
           #|         (display "  - negation-conversion     : ")            
           (display negt)
           (newline)
           (display "  - associative-conversion  : ")
           (display asst)
           (newline)
           (display "  - distributive-conversion : ")
           (display dist)
           (newline)                                    |#
           (display "  - to-dnf                  : ")
           (display dnft)
           (newline)
           (display "  - dnf after conversion?   : ")
           (display (dnf? dnft))
           (newline)
           (display "  - well-converted?         : ")
           (display (compare-formulas (assexcconv impeqf) (assexcconv dnft)))
           (newline)
           (display "----------------------------------------------")
           (newline))
         
         (define (test-cases)
           (define t 'a)
           (test-all t)
           (set! t '(not a))
           (test-all t)
           (set! t '(not a b))
           (test-all t)
           (set! t '(not (and a b)))
           (test-all t)
           (set! t '(and (not a) b))
           (test-all t)
           (set! t '(or a b))
           (test-all t)
           (set! t '(or a (and b c)))
           (test-all t)
           (set! t '(or a (and (not b) c)))
           (test-all t)
           (set! t '(and a (and b c)))
           (test-all t)
           (set! t '(or (not a) (and a b) (and c d)))
           (test-all t)
           (set! t '(or a b c (not d)))
           (test-all t)
           (set! t '(or a b c (not (and a b))))
           (test-all t)
           (set! t '(or (not (not a)) (not (and (not b) (not c))) (not (and a b))))
           (test-all t)
           (set! t '(not (and a (assuming b) (unless c) (or d (assuming e)))))
           (test-all t)
           (set! t '(and a b (and c (or d (or (assuming f) g)) h) (or (not (and (unless i) j (and (not k) l))) m)))
           (test-all t)
           (set! t '(if (and a (or b (iff c d))) e))
           (test-all t)
           (set! t '(or (and a (if b (and c d))) e))
           (test-all t)
           (set! t '(or (and a (or b (and c d))) e))
           (test-all t)
           (set! t '(iff (and a (or b (if c d))) e))
           (test-all t))
           
           #;(define (get-atoms formula)
             (if (pair? formula)
                 (if (atom? (car formula))
                     (cons (car formula) (get-atoms (cdr formula)))
                     (append (get-atoms (car formula)) (get-atoms (cdr formula))))
                 (if (atom? formula)
                     (list formula)
                     '())))
           
           (define (get-atoms formula)
             (if (atom? formula)
                 (list formula)
                 (if (pair? formula)
                     (fold-left append '() (map get-atoms (cdr formula)))
                     '())))
           
           (define (rmv-double dbllist)
             (if (pair? dbllist)
                 (cons (car dbllist) (rmv-double (filter (lambda (x) (not (eq? x (car dbllist)))) (cdr dbllist))))
                 dbllist))
           
           (define (binlist+ l)
             (if (pair? l)
                 (append (list (append (car l) '(#f)) (append (car l) '(#t))) (binlist+ (cdr l)))
                 l))           
           
           (define (generate-substitutions n)
             (cond ((= n 1) '((#f) (#t)))
                   ((> n 1) (binlist+ (generate-substitutions (- n 1))))
                   (else '())))
           
           (define (is-in? a l)
             (if (pair? l)
                 (if (equal? (car l) a)
                     #t
                     (is-in? a (cdr l)))
                 #f))
           
           (define (compare-unilist l1 l2)
             (if (pair? l1)
                 (and
                  (is-in? (car l1) l2)
                  (compare-unilist (cdr l1) l2))
                 #t))
           
           (define (compare-lists l1 l2)
             (and
              (compare-unilist l1 l2)
              (compare-unilist l2 l1)))
           
           ; <atom> x <boolean> x <formula> -> <formula>
           ; atom a is substituted for the boolean b in the formula f
           #;(define (substitute a b f)
             (if (atom? f)
                 (if (equal? a f)
                     b
                     f)
                 (if (null? f)
                     '()
                     (if (equal? (car f) a)
                         (cons b (substitute a b (cdr f)))
                         (cons (substitute a b (car f)) (substitute a b (cdr f)))))
                 ))
           
           (define (substitute a b f)
             (if (equal? a f)
                 b
                 (if (atom? f)
                     f
                     (if (pair? f)
                         (cons (car f) (map (lambda (g) (substitute a b g)) (cdr  f)))
                         '()))))
           
           ; <atom>n x <boolean>n x <formula> -> <formula>
           ; given a list of atoms and a list of boolean, every atom is
           ; substituted by the corresponding boolean value
           (define (substitute-all a* b* f)
             (if (pair? a*)
                 (substitute-all (cdr a*) (cdr b*) (substitute (car a*) (car b*) f))
                 (substitute a* b* f)))
         
           (define (eval-formulas a* s* f1 f2)
             (if (pair? s*)
                 (and
                  (eq?
                   (guard (con
                           (else
                            (display "error! formula  couldn't be evaluated: ")
                            (display (substitute-all a* (car s*) f1))
                            (newline)
                            '()))
                           (eval (substitute-all a* (car s*) f1) (environment '(rnrs))))
                  (guard (con
                           (else
                            (display "error! formula  couldn't be evaluated: ")
                            (display (substitute-all a* (car s*) f2))
                            (newline)
                            '()))
                           (eval (substitute-all a* (car s*) f2) (environment '(rnrs)))))                 
                  (eval-formulas a* (cdr s*) f1 f2))
                 #t))
           
           (define (assexcconv f)
             (if (pair? f)
                 (if (assumption? f)
                     (cadr f)
                     (if (exception? f)
                         (cons 'not (cdr f))
                         (cons (assexcconv (car f)) (map assexcconv (cdr f)))))
                 f))
           
           ; <formula> x <formula> -> <boolean>
           ; returns true, iff both formulas are logical equivalent
           (define (compare-formulas f1 f2)
             (define a1* (rmv-double (get-atoms f1)))
             (define a2* (rmv-double (get-atoms f2)))
             (define s* (generate-substitutions (length a1*)))
             (if (compare-lists a1* a2*)
                 (eval-formulas a1* s* f1 f2)
                 #f))
           
           
           
           ;(test-cases)
           
           
           
           )