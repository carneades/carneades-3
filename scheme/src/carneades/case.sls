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
 (carneades case)   ; generator for arguments from cases
 
 (export other-party make-factor factor? factor-statement factor-favors factor-parent
         make-factors make-case case? case-name case-pfactors case-dfactors case-factors
         case-winner case-statements make-casebase casebase? casebase-issue casebase-factors casebase-cases
         list-cases get-case partition1 partition2 partition3 partition4 partition5 partition6
         partition7 more-on-point as-on-point common-parent? decided-for-other-party? current-case
         generate-arguments-from-cases)
         
 (import (rnrs)
         (rnrs records syntactic)
         (only (rnrs lists) partition)
         (only (carneades system) gensym)
         (carneades stream)
         (prefix (carneades argument) arg:)
         (carneades argument-search)
         (carneades statement)
         (carneades unify)
         (carneades lib match)
         (prefix (carneades set) set:))
         
  (define *debug* #f)
  
  ; specialize set operations
  (define empty-set (set:empty-set eq?))
  (define list->set (set:list->set eq?))
  (define union (set:union eq?))
  (define intersection (set:intersection eq?))
  (define difference (set:difference eq?))
  (define subset? (set:subset? eq?))
    
  
  
  ; TO DO: critical questions, e.g. about the applicability of the precedent due to, e.g.
  ; being from another jurisdiction.
  
  ;; type party = 'plaintiff | 'defendant
  
  (define (other-party party)
    (case party
      ((defendant) 'plaintiff)
      ((plaintiff) 'defendant)
      (else (error "not a party" party))))
  
  (define-record-type factor 
    (fields statement   ; the statement expressed by the factor
            favors      ; a party
            parent))    ; a factor or #f if there is none    
  
  ;; type factors = (set-of factor)
  
  ; make-factors: (list-of factor) -> factors
  (define (make-factors factors) ((set:list->set eq?) (list factors)))
  
  (define-record-type %case
    (fields name        ; string, "current" if the current case
            pfactors    ; a set of factors
            dfactors    ; a set of factors
            winner))    ; the winning party or 'undecided, if the current case
  
  ; make-case: string party (list-of factors) -> case
  (define (make-case name winner factors)
    (let-values (((pfactors dfactors) 
                  (partition (lambda (f) (eq? (factor-favors f) 'plaintiff)) factors)))
      (make-%case name 
                  (list->set pfactors)
                  (list->set dfactors)
                  winner)))
  
  (define case? %case?)
  (define case-name %case-name)
  (define case-pfactors %case-pfactors)
  (define case-dfactors %case-dfactors)
  (define case-winner %case-winner)
  
  ; case-factors: case -> (set-of factors)
  ; the union of the pfactors and dfactors of the case
  (define (case-factors c)
    (union (case-pfactors c) (case-dfactors c)))
  
  ; case-statement: case -> (list-of statement)
  (define (case-statements c)
    (map factor-statement (set:set->list (case-factors c))))
  
  (define-record-type %casebase
    (fields issue      ; a statement, the plaintiff's claim
            factors    ; the sef of all factors in the casebase
            cases))    ; a set of cases
  
  (define casebase? %casebase?)
  
  ; make-casebase: statement (list-of factors) (list-of case) -> casebase
  (define (make-casebase issue factors cases)
    (make-%casebase issue 
                    (list->set factors)
                    (list->set cases)))
  
  ; casebase-issue: casebase -> statement
  (define (casebase-issue cb) (%casebase-issue cb))
  
  ; casebase-factors: casebase -> (set-of factors)
  (define (casebase-factors cb) (%casebase-factors cb))
  
  ; casebase-cases: casebase -> (set-of case)
  (define (casebase-cases cb) (%casebase-cases cb))
  
  ; list-cases: casebase -> (list-of case)
  (define (list-cases cb) (set:set->list (%casebase-cases cb)))
  
  ; get-case: casebase string
  ; Retrieves a case in the case base with the given name.  
  ; If there is more than one case with this name, one is picked nondeterminstically.
  ; Returns #f if there is no case with this name.
  (define (get-case cb name)
    (let ((s (set:filter (lambda (c) 
                           (equal? (case-name c) name)) 
                         (casebase-cases cb))))
      (if (set:empty? s)
          #f
          (set:select s))))
  
  ;; case comparison partitions
  
  ; partition1: case case -> (set-of factors)
  ; plaintiff factors in common
  (define (partition1 cc pc)
    (intersection (case-pfactors cc) (case-pfactors pc)))
  
  ; partition2: case case -> (set-of factors)
  ; defendant factors in common
  (define (partition2 cc pc)
    (intersection (case-dfactors cc) (case-dfactors pc)))
  
  ; partition3: case case -> (set-of factors)
  ; plaintiff factors in cc but not in pc
  (define (partition3 cc pc)
    (difference (case-pfactors cc) (case-pfactors pc)))
  
  ; partition4: case case -> (set-of factors)
  ; defendant factors in pc but not in cc
  (define (partition4 cc pc)
    (difference (case-dfactors pc) (case-dfactors cc)))
  
  ; partition5: case case -> (set-of factors)
  ; defendant factors in cc but not in pc
  (define (partition5 cc pc)
    (difference (case-dfactors cc) (case-dfactors pc)))
  
  ; partition6: case case -> (set-of factors)
  ; plaintiff factors in pc but not in cc
  (define (partition6 cc pc)
    (difference (case-pfactors pc) (case-pfactors cc)))
  
  ; partition7: casebase case case -> (set-of factors)
  ; factors of the casebase which are not in either the cc or the pc
  (define (partition7 cb cc pc)
    (difference (casebase-factors cb)
                    (union (case-factors cc) 
                           (case-factors pc))))
  
  
  ; more-on-point: case case case -> (set-of factor)
  ; Check if pc1 is more on point than pc2, relative to cc
  ; pc1 is more on point than pc2 only the factors of pc1 are a *proper* superset
  ; of the factors of pc2.  If pc1 is more on point, the factors
  ; in pc1 which are not pc2 are returned.  If pc1 is not more on point,
  ; the empty set is returned.
  (define (more-on-point pc1 pc2 cc)
    (let* ((p1-pc1 (partition1 cc pc1))  ; pfactors in common with pc1
           (p1-pc2 (partition1 cc pc2))   ; pfactors in common with pc2
           (p2-pc1 (partition2 cc pc1))   ; dfactors in common with pc1
           (p2-pc2 (partition2 cc pc2))   ; dfactors in common with pc2
           (s1 (union p1-pc1 p2-pc1))
           (s2 (union p1-pc2 p2-pc2)))
      (if (and (subset? s2 s1)
               (not (subset? s1 s2)))
          (difference s1 s2)
          ; else return an empty set
          empty-set)))
  
  ; as-point-point: case case case -> (set-of factor)
  ; same as more-on-point, except the factors of pc1 need only to be a superset,
  ; but not a proper superset, of the factors of pc2.
  (define (as-on-point pc1 pc2 cc)
    (let* ((p1-pc1 (partition1 cc pc1))  ; pfactors in common with pc1
           (p1-pc2 (partition1 cc pc2))   ; pfactors in common with pc2
           (p2-pc1 (partition2 cc pc1))   ; dfactors in common with pc1
           (p2-pc2 (partition2 cc pc2))   ; dfactors in common with pc2
           (s1 (union p1-pc1 p2-pc1))
           (s2 (union p1-pc2 p2-pc2)))
      (if (subset? s2 s1)
          (difference s1 s2)
          ; else return an empty set
          empty-set)))
  
  ; NOTE: The code for common-ancestor below is useless, since it ignores reverses in the party
  ; favored by a factor, from child to parent
  
  ;  ; ancestors: factor -> (set-of factor)
  ;  ; WARNING: Assumes the factor hierarchy is acyclic.  May not 
  ;  ; terminate if cycles exist.
  ;  (define (ancestors f)
  ;    (let ((parent (factor-parent f)))
  ;      (if parent
  ;          (union ((set:list->set eq?) (list parent)) (ancestors parent))
  ;          ; else the empty set
  ;          empty-set))) 
  ;  
  ;  ; common-ancestor?: factor factor -> boolean
  ;  (define (common-ancestor? f1 f2)
  ;    (not (set:empty? (set:intersection (ancestors f1) (ancestors f2)))))
  ;  
  ;  
  ;  ; factor-with-common-ancestor: factor case -> factor | #f
  ;  ; Returns a factor in a case which has an ancestor in common with f1, or #f if there
  ;  ; is no such factor in the case.
  ;  (define (factor-with-common-ancestor f1 case)
  ;    (let ((candidates (set:filter (lambda (f2) (common-ancestor? f1 f2))
  ;                                  (case-factors case))))
  ;      (if (set:empty? candidates)
  ;          #f
  ;          (set:select candidates))))
  ;  
  ;  ; common-ancestor: factor case -> factor | #f
  ;  ; Returns a common ancestor of f1 and some factor in the given case, or #f if there
  ;  ; is no such factor
  ;  (define (common-ancestor f1 case)
  ;    (let ((f2 (factor-with-common-ancestor f1 case)))
  ;      (if (not f2)
  ;          #f ; there is no factor in the case with an ancestor in common with some ancestor of f1
  ;          (let ((candidates (intersection (ancestors f1) (ancestors f2))))
  ;            (if (set:empty? candidates) ; should never be the case
  ;                #f
  ;                (set:select candidates))))))
  
  (define (common-parent? f1 f2)
    (and (factor-parent f1)
         (factor-parent f2)
         (eq? (factor-parent f1)
              (factor-parent f2))))
  
  ; decided-for-other-party: case case -> boolean
  (define (decided-for-other-party? pc1 pc2)
    (cond ((eq? (case-winner pc1) 'plaintiff)
           (eq? (case-winner pc2) 'defendant))
          ((eq? (case-winner pc1) 'defendant)
           (eq? (case-winner pc2) 'plaintiff))
          (else #f))) ; to handle undecided cases
  
  ; current-case: state casebase -> case
  ; The factors of the current case are the factors in the casebase which 
  ; have been accepted or are acceptable in the argument graph of the state
  (define (current-case state cb)
    (let ((ag (state-arguments state)))
      (make-case "current" 
                 'undecided 
                 (set:set->list (set:filter (lambda (factor)
                                             (or (arg:accepted? ag (factor-statement factor))
                                                 (arg:acceptable? ag (factor-statement factor))))
                                           (casebase-factors cb))))))
  
  
  ; type generator: statement state  -> (stream-of response)
  
  ; generate-arguments-from-cases: casebase -> generator
  (define (generate-arguments-from-cases cb)
    (lambda (goal state)
      (let ((subs (state-substitutions state))
            (args (state-arguments state)))
      ; dispatch: statement casebase  -> generator | #f
      ; (if *debug* (printf "cbr debug: goal is ~v~n" goal))
      (match goal
        (('factors-favor p i)
         ; scheme: cite on-point precedent 
         ; AS2 ("preference from precedent") of [Wyner & Bench-Capon, 2007]
         ; (if *debug* (printf "cbr debug, factors-favor~n"))
         (let ((party (subs p))
               (issue (subs i)))
           ; (printf "debug: factors-favor ~v ~v~n" party issue)
           (if (not (ground? `(factors-favor ,party ,issue)))
               (stream) ; fail
               (stream-flatmap 
                (lambda (precedent)
                  (let* ((cc (current-case state cb))
                         (common-factors 
                          (union (partition1 cc precedent)    ; factors in common for plaintiff
                                 (partition2 cc precedent)))  ; factors in common for defendant
                         (scheme (string-append "AS2. cite: \"" (case-name precedent) "\""))
                         (previous-schemes (arg:schemes-applied (state-arguments state) 
                                                            (statement-atom goal))))
                    (if (and (not (set:empty? common-factors))
                             (eq? (case-winner precedent) party)
                             (not (member scheme previous-schemes))) 
                        ; cc and pc have pfactors in common and pc was decided in favor of plaintiff
                        (stream (make-response
                                 subs ; no new subs
                                 (arg:make-argument
                                  ; id
                                  (gensym 'a)
                                  ; direction:
                                  'pro
                                  ; conclusion: 
                                  goal
                                  ; premises
                                  (append (map arg:pr 
                                               (map factor-statement 
                                                    (set:set->list common-factors)))
                                          
                                          ; exceptions
                                          (map arg:ex 
                                               (list `(has-counterexample ,(other-party party) ,(case-name precedent))
                                                     `(distinguishable ,(other-party party) ,(case-name precedent))
                                                     )))
                                  ; scheme:
                                  scheme)))
                        ; else fail
                        (stream))))
                (list->stream (set:set->list (casebase-cases cb))))))) 
        
        (('has-counterexample p cn)
                 
         ; scheme: cite an more-on-point counterexample to the cited case, i.e. a precedent
         ; with more pfactors and dfactors in common with cc than the cited case that went the other
         ; way. 
         
         ; (if *debug* (printf "cbr debug, has-counterexample~n"))

         (let* ((party (subs p))
                (cname (subs cn))
                (previous-precedent (get-case cb cname)))
           (if (or (not (ground? `(has-counterexample ,party ,cname)))
                   (not previous-precedent))
               (stream)
               (let ((cc (current-case state cb))) ; the current case
                 (stream-flatmap 
                  (lambda (new-precedent)
                    (let* ((diff (more-on-point new-precedent previous-precedent cc))
                           (scheme (string-append "counterexample: \"" (case-name new-precedent) "\""))
                           (previous-schemes (arg:schemes-applied (state-arguments state)
                                                              (statement-atom goal))))
                      (if (and (not (set:empty? diff))
                               (decided-for-other-party? new-precedent previous-precedent)
                               (not (member scheme previous-schemes)))
                          (stream (make-response 
                                   subs ; no new subs
                                   (arg:make-argument
                                    ; id
                                    (gensym 'a)
                                    ; direction
                                    'pro 
                                    ; conclusion:
                                    goal
                                    ; premises 
                                    (append 
                                     ; ordinary premises
                                     (map arg:pr (map factor-statement (set:set->list diff)))
                                     ; exceptions
                                     (map arg:ex (list  `(has-counterexample ,(other-party party) ,(case-name new-precedent))
                                                    `(distinguishable ,(other-party party) ,(case-name new-precedent))
                                                    )))
                                    ; scheme: 
                                    scheme)))
                          ; else fail
                          (stream))))
                  (list->stream (set:set->list (casebase-cases cb)))))))) 
        
        (('distinguishable p cn)
         ; scheme: distinguish with pfactors in PC not in CC
         ; this is AS3 and AS4 of [Wyner & Bench-Capon, 2007] generalized to handle
         ; arguments for defedant as well as plaintiff. The scheme looks for 
         ; arguments favoring the given party.
         
         ; (if *debug* (printf "cbr debug, distinguishable~n"))
                  
         (let* ((party (subs p))
                (cname (subs cn))
                (pc (get-case cb cname)))
           (if (or (not (ground? `(distinguishable ,party ,cname)))
                   (not pc))
               (stream) ; fail, no precedent with this name
               (let* ((cc (current-case state cb))
                      (p3 (partition3 cc pc)) ; pfactors in cc but not in pc
                      (p4 (partition4 cc pc)) ; dfactors in pc but not in cc
                      (p5 (partition5 cc pc)) ; dfactors in cc but not in pc
                      (p6 (partition6 cc pc)) ; pfactors in pc but not in cc
                      (weaker-cc-factors 
                       (case party
                         ((plaintiff) p3)
                         ((defendant) p5)  
                         (else (error "not a party" party))))
                      (stronger-pc-factors 
                       (case party 
                         ((plaintiff) p4) 
                         ((defendant) p6) 
                         (else (error "not a party" party))))) 
                 (stream-append 
                  ; AS3: PC Stronger
                  (let* ((scheme "AS3. precedent stronger")
                         (previous-schemes (arg:schemes-applied (state-arguments state) 
                                                            (statement-atom goal))))
                    (if (or (set:empty? stronger-pc-factors)
                            (member scheme previous-schemes))
                        (stream) ; fail, the precedent is not distinguishable
                        ; else 
                        (stream (make-response
                                 subs ; no new subs
                                 (arg:make-argument
                                  ; id
                                  (gensym 'a)
                                  ; direction
                                  'pro
                                  ; conclusion:
                                  goal
                                  ; premises
                                  (map arg:ex (append (map factor-statement (set:set->list stronger-pc-factors))
                                                  (list `(downplay precedent-stronger ,(other-party party) ,cname))))
                                  ; scheme: 
                                  scheme)))))
                  ; AS4: CC Weaker
                  (let* ((scheme "AS4. current case weaker")
                         (previous-schemes (arg:schemes-applied (state-arguments state)
                                                            (statement-atom goal))))
                    (if (or (set:empty? weaker-cc-factors)
                            (member scheme previous-schemes))
                        (stream) ; fail, the precedent is not distinguishable
                        ; else 
                        (stream (make-response 
                                 subs ; no new subs
                                 (arg:make-argument
                                  ; id
                                  (gensym 'a)
                                  ; direction
                                  'pro
                                  ; conclusion:
                                  goal
                                  ; premises
                                  (append 
                                   ; ordinary premises 
                                   (map arg:pr (map factor-statement (set:set->list weaker-cc-factors)))
                                   ; exceptions
                                   (list (arg:ex `(downplay current-case-weaker ,(other-party party) ,cname))))
                                  
                                  ; scheme: 
                                  scheme))))))
                 ))))
       
        
        (('downplay dt p cn)
         ; scheme: the distinguishing factors of the cc can be explained away by using the factor hierarchy to
         ; substitute or cancel factors. See [Wyner & Bench-Capon, 2007].
         ; distinction-type: 'precedent-stronger | 'current-case-weaker
         
         ; (if *debug* (printf "cbr debug, downplay~n"))

                  
         (let* ((distinction-type (subs dt))
                (party (subs p))
                (cname (subs cn))
                (pc (get-case cb cname))) ; get the precedent case
           (if (or (not (ground? `(downplay ,distinction-type ,party ,cname)))
                   (not pc))
               (stream) ; fail, no precedent with this name
               (let* ((cc (current-case state cb))
                      (base-partition 
                       (case distinction-type 
                         ((current-case-weaker) (partition5 cc pc))
                         ((precedent-stronger) (partition6 cc pc))
                         (else (error "Not a distinction type:" distinction-type))))
                      (p3 (partition3 cc pc)) ; p factors in cc not in pc
                      (p4 (partition4 cc pc)) ; d factors in pc not in cc
                      (cancelling-partition 
                       (case distinction-type
                         ((current-case-weaker) p3)
                         ((precedent-stronger) p4)
                         (else (error "Not a distinction type:" distinction-type))))
                      (substituting-partition 
                       (case distinction-type
                         ((current-case-weaker) p4)
                         ((precedent-stronger) p3)
                         (else (error "Not a distinction type:" distinction-type))))
                      (p3-downplayed 
                       (set:filter (lambda (p3-factor)
                                     (set:any? (lambda (base-factor) 
                                                 (common-parent? base-factor p3-factor))
                                               base-partition))
                                   p3))
                      (p4-downplayed 
                       (set:filter (lambda (p4-factor)
                                     (set:any? (lambda (base-factor) 
                                                 (common-parent? base-factor p4-factor))
                                               base-partition))
                                   p4))
                      (cancelling (or (and (eq? distinction-type 'precedent-stronger)
                                           (not (set:empty? p4-downplayed)))
                                      (and (eq? distinction-type 'current-case-weaker)
                                           (not (set:empty? p3-downplayed)))))
                      (substituting (or (and (eq? distinction-type 'precedent-stronger)
                                             (not (set:empty? p3-downplayed)))
                                        (and (eq? distinction-type 'current-case-weaker)
                                             (not (set:empty? p4-downplayed)))))
                      (scheme "downplay")
                      (previous-schemes (arg:schemes-applied (state-arguments state)
                                                         (statement-atom goal))))
                 (if (and (not (set:empty? (union p3-downplayed p4-downplayed)))
                          (not (member scheme previous-schemes)))
                     (stream (make-response 
                              subs ; no new subs
                              (arg:make-argument 
                               ; id:
                               (gensym 'a)
                               ; direction:
                               'pro
                               ; conclusion:
                               goal
                               ; premises -- the downplayed factors
                               (map arg:pr (append (map factor-statement (set:set->list p3-downplayed))
                                               (map statement-complement 
                                                    (map factor-statement 
                                                         (set:set->list p4-downplayed)))))
                               ; scheme: 
                               scheme)))
                     ; else fail
                     (stream))))))
        
        ; else handle goals about the issue of the case base
        (_ (match goal 
             (('not statement)
              
              ; (if *debug* (printf "cbr debug, AS1, negative goal~v~n" (subs goal)))

              (let* ((scheme (string-append "AS1. factor comparison"))
                     (previous-schemes (arg:schemes-applied (state-arguments state) 
                                                        (statement-atom goal))))
                ; complementary version of AS1 for negated goals
                (if (and (eq? (subs statement)
                              (casebase-issue cb))
                         (not (member scheme previous-schemes)))
                    (stream (make-response 
                             subs ; no new subs
                             (arg:make-argument 
                              ; id:
                              (gensym 'a)
                              ; direction:
                              'con
                              ; conclusion:
                              statement
                              ; premises 
                              (list (arg:pr `(factors-favor defendant ,(casebase-issue cb))))
                              ; scheme: 
                              scheme)))
                    (stream)))) ; fail
             (_ (if (eq? (subs goal)
                         (casebase-issue cb))
                    ; scheme: factor comparison
                    ; AS1 ("main scheme") of [Wyner & Bench-Capon, 2007]
                    ; Note: it is assumed that factors of the current case needed for comparing the case
                    ; with precedents are acceptable in the argument graph.  One way to do this is to first query
                    ; the user, using the evidence module. Alternatively, the factors can be accepted in the 
                    ; argument context. Deriving the factors using rules or other schemes will also work, so long
                    ; as care is taken to assure the required factors are acceptable in the argument graph 
                    (let* ((scheme (string-append "AS1. factor comparison"))
                           (previous-schemes (arg:schemes-applied (state-arguments state) 
                                                              (statement-atom goal))))
                      ; (if *debug* (printf "cbr debug, AS1, positive goal ~v~n" (subs goal)))
                      (if (and (eq? (subs goal)
                                    (casebase-issue cb))
                               (not (member scheme previous-schemes)))
                          ; before using the CBR schemes.
                          (begin
                            ; (printf "cbrd debug, AS1 success~n")
                            (stream (make-response 
                                     subs ; no new subs
                                     (arg:make-argument 
                                      ; id:
                                      (gensym 'a)
                                      ; direction:
                                      'pro
                                      ; conclusion:
                                      goal
                                      ; premises 
                                      (list (arg:pr `(factors-favor plaintiff ,(casebase-issue cb))))
                                      ; scheme: 
                                      scheme))))
                          (begin
                            ; (printf "cbr debug, AS1 fail~n")
                            (stream))))
                    (begin 
;                      (if *debug* (printf "cbr debug, AS1, goal ~v does not equal issue ~v~n" 
;                                          (subs goal) 
;                                          (casebase-issue cb)))
                      (stream)))))) ; fail
        )))) ; end of generate-arguments-from-cases
  
  ) ; end of case-based reasoning module