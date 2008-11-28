#!r6rs

(import (except (rnrs) assert values)
        (carneades argument)
        (carneades argument-diagram)
        (carneades case)
        (carneades argument-builtins)
        (carneades rule)
        (carneades lkif2)
        (carneades dlp)
        (carneades dnf)
        (carneades shell)
        (carneades unify)
        (carneades lib srfi format)
        (carneades lib srfi lightweight-testing))

(define (printn s)
  (display s)
  (newline))

(define null '())

(define error-counter 0)

(define error-message "")

(define (init-errors)
  (set! error-counter 0)
  (set! error-message ""))

(define (adderror s)
  (set! error-counter (+ error-counter 1))
  (set! error-message (string-append error-message s "\n")))

(define (run-single-test b test-string failure-string)
  (display (string-append "testing " test-string " ... "))  
  (if b
      (display "OK")
      (begin (display "failed")
             (adderror failure-string)))
  (newline))



(printn "--------------")
(printn "Carneades Testing Module")
(newline)

; -------------------------------
; argument tests
; -------------------------------

(define (run-argument-tests)
  
  (define-argument a1 (pro "Tweety flies"
                           (pr "Tweety is a bird")
                           (am "Birds fly")
                           (ex "Tweety is an abnormal bird")))
  
  (define-argument a2 (pro "Tweety is an abnormal bird"
                           (pr "Tweety is a penguin")
                           (am "Penguins are abnormal birds")))
  
  (define tweety-graph (assert-arguments empty-argument-graph (list a1 a2)))
  
  (define c1 (accept default-context (list "Tweety is a bird" "Tweety is a penguin")))
  
  ; string-length of d1 should be 690
  (define d1 (call-with-values open-string-output-port (lambda (port extract)
                                                         (diagram* tweety-graph
                                                                   c1
                                                                   identity
                                                                   (lambda (s) (format "~a" s))
                                                                   port)
                                                         (extract))))
  

  
  (printn "starting with argument tests ...")

  (run-single-test (= (string-length d1) 690)
                   "argument-diagram"
                   "argument-diagram: the created diagram is not identical with the presetting")
  
  (newline)
  
  )



; -------------------------------
; case tests
; -------------------------------

(define (run-case-tests)
  
  (define f101 (make-factor 'info-trade-secret 'plaintiff #f))
  (define f102 (make-factor 'efforts-to-maintain-secrecy 'plaintiff f101))
  (define f120 (make-factor 'info-legitimately-obtained-or-obtainable 'defendant #f))
  (define f104 (make-factor 'info-valuable 'plaintiff f101))
  (define f105 (make-factor 'info-known-or-available 'defendant f101))
  (define f108 (make-factor 'info-available-elsewhere 'defendant f105))
  (define f111 (make-factor 'questionable-means 'plaintiff f120))
  (define f114 (make-factor 'confidential-relationship 'plaintiff #f))
  (define f115 (make-factor 'notice-of-confidentiality 'plaintiff f114))
  (define f122 (make-factor 'efforts-to-maintain-secrecy-vis-a-vis-defendant 'plaintiff f102))
  (define f1 (make-factor 'disclosure-in-negotiations 'defendant f122))
  (define f2 (make-factor 'bribed-employee 'plaintiff f111))
  (define f4 (make-factor 'agreed-not-to-disclose 'plaintiff f122))
  (define f6 (make-factor 'security-measures 'plaintiff f102))
  (define f7 (make-factor 'brought-tools 'plaintiff #f))
  (define f10 (make-factor 'secrets-disclosed-to-outsiders 'defendant f105))
  (define f12 (make-factor 'outsider-disclosures-restricted 'plaintiff f105))
  (define f15 (make-factor 'unique-product 'plaintiff f104))
  (define f16 (make-factor 'info-reverse-engineerable 'defendant f108))
  (define f21 (make-factor 'knew-info-confidential 'plaintiff f115))
  (define f25 (make-factor 'information-reverse-engineered 'defendant f111))
  (define f26 (make-factor 'used-deception 'plaintiff f111))
  (define f27 (make-factor 'disclosure-in-public-forum 'defendant f105))
  
  (define factors (list f1 f2 f4 f6 f7 f10 f12 f15 f16 f21 f25 f26 f27  f101 f102 f104 f105 
                        f108 f111 f114 f115 f120 f122))
  
  ; cases from [Wyner & Bench-Capon, 2007]
  (define vanilla (make-case "Vanilla" 'plaintiff (list f1 f15)))
  (define bribe (make-case "Bribe" 'plaintiff (list f1 f2 f15)))
  (define deceit (make-case "Deceit" 'plaintiff (list f1 f15 f26)))
  (define disclose (make-case "Disclose" 'plaintiff (list f1 f10 f15)))
  (define restrict (make-case "Restrict" 'plaintiff (list f1 f10 f12 f15)))
  (define bribe2 (make-case "Bribe2" 'plaintiff (list f1 f2 f15 f25)))
  (define rev (make-case "Reverse" 'plaintiff (list f1 f15 f25)))
  (define announce (make-case "Announce" 'defendant (list f1 f2 f15 f27))) ; added f2 for testing more-on-point scheme
  
  
  (define wyner-cases (list vanilla bribe deceit disclose restrict bribe2 rev announce))
  
  (define wyner-cb (make-casebase 'trade-secret-violation factors wyner-cases))
  (define bribe-cb (make-casebase 'trade-secret-violation factors (list bribe)))
  (define vanilla-cb (make-casebase 'trade-secret-violation factors (list vanilla)))
  
  ; Initial contexts for some test "current" cases, with their
  ; accepted statements copied from factors of cases
  
  (define (make-context-from-case case)
    (accept default-context (case-statements case)))
  
  (define c-vanilla (make-context-from-case vanilla))
  (define c-disclose (make-context-from-case disclose))
  (define c-bribe (make-context-from-case bribe))
  (define c-announce (make-context-from-case announce))  
  (define c-as1 (accept c-vanilla (list '(factors-favor plaintiff trade-secret-violation))))
  (define c-as2 (reject c-vanilla (list '(distinguishable defendant "Bribe"))))
  (define c-mop (reject  c-bribe '((distinguishable plaintiff "Announce"))))
  
  
  ; cbr-engine: integer integer case-base context -> statement -> (stream-of argument-state)
  (define (cbr-engine max-nodes max-turns case-base context)
    (make-engine* max-nodes max-turns context
                  (list builtins 
                        (generate-arguments-from-cases case-base))))
  
  (printn "starting with case tests ...")
  
  ; to do: further and more systematic tests
  
  ; AS1: Main Scheme  
  (run-single-test (all-in? 'trade-secret-violation (cbr-engine 50 4 bribe-cb c-as1))
                   "case: AS1 - Main Scheme"
                   "case: 'trade-secret-violation should be in! (AS1)")
  
  ; AS2: Preference-From-Precedent Scheme
  (run-single-test (all-in? '(factors-favor plaintiff trade-secret-violation) (cbr-engine 50 4 bribe-cb c-as2))
                   "case: AS2 - Preference-From-Precedent Scheme"
                   "case: '(factors-favor plaintiff trade-secret-violation) should be in! (AS2)")
  
  ; AS3: Precedent-Stronger Scheme
  (run-single-test (all-in? '(distinguishable defendant "Bribe") (cbr-engine 50 4 bribe-cb c-vanilla))
                   "case: AS3 - Precedent-Stronger Scheme"
                   "case: '(distinguishable defendant \"Bribe\") should be in! (AS3)")
  
  ; AS4: Current-Case-Weaker Scheme
  (run-single-test (all-in? '(distinguishable defendant "Vanilla") (cbr-engine 50 4 vanilla-cb c-disclose))
                   "case: AS4 - Current-Case-Weaker Scheme"
                   "case: '(distinguishable defendant \"Vanilla\") should be in! (AS4)")
  
  ; Counterexample: More-On-Point Scheme
  (run-single-test (all-in? '(has-counterexample defendant "Reverse") (cbr-engine 50 4 wyner-cb c-mop))
                   "case: Counterexample - More-On-Point Scheme"
                   "case: '(has-counterexample defendant \"Reverse\") should be in! (Counterexample)")
  
  ; Downplay-Precedent-Stronger Scheme
  (run-single-test (all-in? '(downplay precedent-stronger plaintiff "Deceit") (cbr-engine 50 4 wyner-cb c-bribe))
                   "case: Downplay-Precedent-Stronger Scheme"
                   "case '(downplay precedent-stronger plaintiff \"Deceit\") should be in! (Downplay-Precedent-Stronger)")
  
  
  (newline)
  
  )



; -------------------------------
; rule tests
; -------------------------------

(define (run-rule-tests)
  
  (define rb1 
    (rulebase
     
     (rule r1 
           (if (and (movable ?c)
                    (unless (money ?c)))
               (goods ?c)))
     
     (rule r3 (if (coins ?x) (money ?x)))     
     
     ; r4 and r5 are to test multiple conclusions
     (rule r4
           (if (movable ?x)
               (and (light ?x)
                    (shipable ?x))))
     
     (rule r5
           (if (and (light ?x)
                    (shipable ?x))
               (convenient ?x)))
     
     (rule* lex-posterior
            (if (and (enacted ?r1 ?d1)
                     (enacted ?r2 ?d2)
                     (later ?d2 ?d1))
                (prior ?r2 ?r1)))
     
     (rule r6 
           (if (edible ?x) 
               (not (goods ?x))))
     
     ; to test exclusionary rules
     (rule r14 (if (bird ?x) (flies ?x)))
     (rule* r15 (if (penguin ?x) (excluded r14 (flies ?x))))     
     
     ; to test negative exceptions, by asking the validity critical question.
     (rule repeal
           (if (repealed ?r)
               (not (valid ?r))))
     
     ; disjunction tests
     (rule r19 
           (if (or (p1 ?x) 
                   (p2 ?x)) 
               (p3 ?x)))
     
     (rule r20 
           (if (or (and (p4 ?x) (p5 ?x))
                   (and (p6 ?x) (p7 ?x))
                   (p8 ?x))
               (p9 ?x)))
     
     ; eval test
     (rule r21 
           (if (and (p10 ?x) (eval ?z (reverse ?x))) 
               (p11 ?z)))     
     
     ; another eval test, to demonstrate calculations
     (rule r22 
           (if (and (income ?x ?i)
                    (deductions ?x ?d)
                    (eval ?t (- ?i ?d)))
               (taxable-income ?x ?t)))
     
     )) ; end of rule base
  
  ; accept some facts in the context
  (define as1 
    '((coins item1)
      (bird Tweety)
      (penguin Tweety)
      (enacted r1 d1)
      (enacted r6 d2)
      (later d2 d1)
      (repealed r5)
      (movable item1)
      (movable item2)
      (edible item2)
      (foo2 a)
      (p1 a)
      (p2 b)
      (p6 a)
      (p7 a)
      (p8 a)
      (p10 '(a b c d e))
      (income Sam 60000)
      (deductions Sam 7000)))
  
  (define elephants-rulebase 
    (rulebase
     
     (rule r1 
           (if (and (elephant ?x)
                    (unless (royal ?x)))
               (gray ?x)))
     
     (rule r2 
           (if (and (elephant ?x)
                    (royal ?x))
               (not (gray ?x))))      
     ))
  
  (define elephant-facts
    '((elephant clyde)
      (african clyde)
      (royal clyde)
      (elephant dumbo)
      (african dumbo)))
  
  (define dutch-rulebase
    (rulebase
     
     (rule r3
           (if (native-speaker ?x Pa-Dutch)
               (born ?x America)))
     
     (rule r4
           (if (and (native-speaker ?x German)
                    (unless (native-speaker ?x Pa-Dutch)))
               (not (born ?x America))))
     
     (rule r5 
           (if (native-speaker ?x Pa-Dutch)
               (native-speaker ?x German)))
     ))
  
  (define dutch-facts
    '((native-speaker Herman Pa-Dutch)
      (native-speaker Fritz German)))
  
  (define gullible-rulebase
    (rulebase
     
     (rule r6 (if (and (citizen ?x)
                       (crook ?y))
                  (not (like ?x ?y))))
     
     ; r7 would only work if rebuttals are search for and found
     ;   (rule r7 (if (and (citizen ?x)
     ;                     (gullible ?x)
     ;                     (crook ?y)
     ;                     (elected ?y))
     ;                (like ?x ?y)))
     
     (rule r7 (if (and (citizen ?x)
                       (gullible ?x)
                       (crook ?y)
                       (elected ?y))
                  (excluded r6 (not (like ?x ?y)))))
     ))
  
  (define gullible-facts
    '((citizen Fred)
      (citizen John)
      (gullible Fred)
      (crook Dick)
      (elected Dick)))
  
  (define blocks-world-rulebase
    (rulebase
     
     
     (rule r8 
           (if (and (block ?x)
                    (heavy ?x))
               (on ?x table)))
     
     (rule r9 
           (if (on A table)
               (excluded r8 (on B table))))
     
     (rule r10 
           (if (on B table)
               (excluded r8 (on A table))))  
     ))
  
  (define blocks-world-facts 
    '((block A)
      (block B)
      (block C)
      (heavy A)
      (heavy B)
      (heavy C)
      (not (on B table))))
  
  (define dancer-rulebase
    (rulebase
     
     (rule r12
           (if (dancer ?x)
               (graceful ?x)))
     
     (rule r13
           (if (and (dancer ?x)
                    (graceful ?x))
               (ballerina ?x))) 
     
     (rule r14 
           (if (or (rock-and-roller ?x) 
                   (square-dancer ?x))
               (excluded r12 (graceful ?x))))
     
     (rule r15
           (if (square-dancer ?x)
               (dancer ?x)))
     
     (rule r16
           (if (rock-and-roller ?x)
               (dancer ?x)))
     
     )) ; end of rule base
  
  (define dancer-facts 
    '((dancer Naomi)
      (dancer Mikhail)
      (rock-and-roller Norbert)
      (square-dancer Sally)))
  
  
  ; engine integer integer (list-of symbol) -> statement -> (stream-of argument-state)
  (define (engine max-nodes max-turns rules assumptions critical-questions)
    (make-engine* max-nodes max-turns 
                  (accept default-context assumptions)
                  (list (generate-arguments-from-rules rules critical-questions) 
                        builtins
                        )))
  
  (printn "starting with rule tests ...")
  
  
  ; -- taken from rule-tests
  
  (run-single-test (all-in? '(bird Tweety) (engine 20 1 rb1 as1 null))
                   "simple rules"
                   "rule test: (bird Tweety) should be acceptable! (simple)")
  
  (run-single-test (all-in? '(bird ?x) (engine 20 1 rb1 as1 null))
                   "simple rules with variables"
                   "rule test: (bird Tweety) should be acceptable! (simple variables)")
  
  ; coins are money
  (run-single-test (all-in? '(money item1) (engine 20 1 rb1 as1 null))
                   "simple rules"
                   "rule test: (money item1) should be acceptable! (simple)")
  
  (run-single-test (all-in? '(prior ?r1 ?r2) (engine 20 1 rb1 as1 null))
                   "rules with conjunctions"
                   "rule test: (prior r6 r1) should be acceptable! (conjunction)")
  
  ; disjunction of atomic statements
  (run-single-test (all-in? '(p3 b) (engine 20 1 rb1 as1 null))
                   "rules with disjunctions of atomic statements"
                   "rule test: (p3 a) should be acceptable! (disjunction of atomic statements)")
  
  ; disjunction of conjunctions
  (run-single-test (all-in? '(p9 a) (engine 20 1 rb1 as1 null))
                   "rules with disjunctions of conjunctions"
                   "rule test: (p9 a) should be acceptable! (disjunction of conjunction)")
  
  ; find pro argument
  (run-single-test (all-in? '(goods item1) (engine 20 1 rb1 as1 null))
                   "rules with one turn"
                   "rule test: (goods item1) should be acceptable! (one turn)")
  
  ; unless money exception
  (run-single-test (not (all-in? '(goods item1) (engine 20 2 rb1 as1 null)))
                   "rules with two turns"
                   "rule test: (goods item1) should not be acceptable! (2 turns - exception)")
    
  ; repealed rules are not valid
  (run-single-test (not (all-in? '(convenient item1) (engine 20 2 rb1 as1 '(valid))))
                   "repealed rules should not be acceptable"
                   "rule test: (convenient item1) should not be acceptable! (repealed rules are not valid)")
  
  ; lex posterior
  (run-single-test (not (all-in? '(goods item2) (engine 20 2 rb1 as1 '(priority)))) 
                   "lex posterior rules"
                   "rule test: (goods item2) should not be acceptable! (lex posterior)")
  
  ; to do: fix the following test. The success predicate tests only whether one is found, not all  
  ; (test-true "multiple rule conclusions" (all-in? '(convenient ?x)) (engine 20 1 null))
  
  (run-single-test (all-in? '(not (goods item2)) (engine 20 3 rb1 as1 null)) 
                   "rules with 3 turns and negated query"
                   "rule test: (not (goods item2)) should be acceptable! (3 turns - negated query)")
  
  (run-single-test (not (all-in? '(flies Tweety) (engine 20 2 rb1 as1 '(excluded))))
                   "exclusionary rules"
                   "rule test: (flies Tweety) should not be acceptable! (exclusionary)")
  
  (run-single-test (all-in? '(applies ?r (goods ?x)) (engine 20 1 rb1 as1 null))
                   "rules with applies"
                   "rule test: (applies (goods item1)) and (applies (goods item2)) shpuld be acceptable! (applies)")
  
  ; to do: test negative conditions and exceptions
  ; to do: test rules with negative conclusions
  
  ; reverse a list
  (run-single-test (all-in? '(p11 ?x) (engine 20 1 rb1 as1 null)) 
                   "rules with eval (reverse)"
                   "rule test: (p11 (e d c b a)) should be acceptable! (eval)")
  
  ; calculations
  (run-single-test (all-in? '(taxable-income Sam ?x) (engine 20 1 rb1 as1 null))
                   "rules with eval (calculation)"
                   "rule test: (taxable-income Sam 53000) should be acceptable! (eval)")
  
  ; to do: test assumptions -- a statement is questioned by making an argument pro or con the statement
  ; to do: event calculus tests
  
  
  ; -- Royal Elephant Benchmark
  
  (run-single-test (some-in? '(gray ?x) (engine 20 2 elephants-rulebase elephant-facts null))
                   "rules: Royal Elephant Benchmark 1"
                   "rule test: (gray dumbo) should be acceptable! (Royal Elephant Benchmark)")
  
  (run-single-test (some-in? '(not (gray ?x)) (engine 20 2 elephants-rulebase elephant-facts null))
                   "rules: Royal Elephant Benchmark 2"
                   "rule test: (not (gray clyde)) should be acceptable! (Royal Elephant Benchmark)")
  
  ; -- Pennsylvania Dutch Benchmark
  
  (run-single-test (some-in? '(born ?x ?y) (engine 20 2 dutch-rulebase dutch-facts null))
                   "rules: Pennsylvania Dutch Benchmark 1"
                   "rule test: (born Herman America) should be acceptable! (Pennsylvania Dutch Benchmark)")

  
  (run-single-test (some-in? '(not (born ?x America)) (engine 20 2 dutch-rulebase dutch-facts null))
                   "rules: Pennsylvania Dutch Benchmark 2"
                   "rule test: (not (born Fritz America)) should be acceptable! (Pennsylvania Dutch Benchmark)")
  
  ; -- Gullible Citizens Benchmark
  
  (run-single-test (some-in? '(not (like ?x ?y)) (engine 20 2 gullible-rulebase gullible-facts '(excluded)))
                   "rules: Gullible Citizens Benchmark 1"
                   "rule test: (not (like John Dick)) should be acceptable! (Gullible Citizens Benchmark)")
  
  (run-single-test (not (some-in? '(not (like Fred Dick)) (engine 20 2 gullible-rulebase gullible-facts '(excluded))))
                   "rules: Gullible Citizens Benchmark 2"
                   "rule test: (not (like Fred Dick)) should not be acceptable! (Gullible Citizens Benchmark)")
  
  ; -- Blocks World Benchmark
  
  (run-single-test (some-in? '(block ?x) (engine 20 2 blocks-world-rulebase blocks-world-facts '(excluded)))
                   "rules: Blocks World Benchmark 1"
                   "rule test: (block C), (block B) and (block A) should be acceptable! (Blocks World Benchmark)")
  
  (run-single-test (some-in? '(on ?x table) (engine 20 2 blocks-world-rulebase blocks-world-facts '(excluded)))
                   "rules: Blocks World Benchmark 2"
                   "rule test: (on C table) should be acceptable! (Blocks World Benchmark)")
  
  (run-single-test (some-in? '(not (on ?x table)) (engine 20 2 blocks-world-rulebase blocks-world-facts '(excluded)))
                   "rules: Blocks World Benchmark 3"
                   "rule test: (not (on B table)) should be acceptable! (Blocks World Benchmark)")
  
  ; -- Dancer Benchmark
  
  (run-single-test (some-in? '(dancer Sally) (engine 50 2 dancer-rulebase dancer-facts '(excluded)))
                   "rules: Dancer Benchmark 1"
                   "rule test: (dancer Sally) should be acceptable! (Dancer Benchmark)")
  
  (run-single-test (not (some-in? '(ballerina Sally) (engine 50 2 dancer-rulebase dancer-facts '(excluded))))
                   "rules: Dancer Benchmark 2"
                   "rule test: (ballerina Sally) should not be acceptable! (Dancer Benchmark)")
  
  (newline)
  
  )


; -------------------------------
; ontology tests
; -------------------------------

(define (run-ontology-tests)
  
  (ontology kb1
            
            ; <dlpassertion>
            (instance i1 c1)
            (instance i2 (and c3 c4))
            (instance i3 c7)
            (instance i4 c8)
            (related i3 i33 r11)
            (related i4 i44 r11)
            (related i5 i6 r1)
            (instance i6 c11)
            (instance i7 c14)
            (instance i8 c13)
            (instance i9 (and c17 c18))
            (instance i10 (and c15 c16))
            (related i11 i12 r2)
            (related i13 i14 r3)
            (related i15 i16 r6)
            (related i17 i18 r5)
            (related i19 i20 r7)
            (related i21 i22 r8)
            (related i23 i24 r9)
            (related i24 i25 r9)
            (related i26 i27 r10)
            
            ; <dlpcinclusion>
            (define-primitive-concept c1 c2) ; (c2 x) <- (c1 x)
            (define-primitive-concept (and c3 c4) (and c5 c6)) ; (and (c5 x) (c6 x)) <- (and (c3 x) (c4 x))
            (define-primitive-concept (or c7 c8) (all r11 c9)) ; (c9 y) <- (and (r11 x y) (or (c7 x) (c8 x)))
            (define-primitive-concept (some r1 c11) c12) ; (c12 x) <- (and (r1 x y) (c11 y))
            
            ; <dlpcequivalence>
            (define-concept c13 c14) ; (c13 x) <- (c14 x)
            ; (c14 x) <- (c13 x)
            (define-concept (and c15 c16) (and c17 c18)) ; (and (c15 x) (c16 x)) <- (and (c17 x) (c18 x))
            ; (and (c17 x) (c18 x)) <- (and (c15 x) (c16 x))
            
            ; <dlprange>
            (define-primitive-concept TOP (all r2 c19)) ; (c19 y) <- (r2 x y)
            
            ; <dlpdomain>
            (define-primitive-concept TOP (all (inverse r2) c20)) ; (c20 x) <- (r2 x y)
            
            ; <dlprinclusion>
            (define-primitive-role r3 r4) ; (r4 x y) <- (r3 x y)
            
            ; <dlprequivalence>
            (define-role r5 r6) ; (r5 x y) <- (r6 x y)
            ; (r6 x y) <- (r5 x y)
            
            ; <dlpinverse>
            (define-role r7 (inverse r8)) ; (r8 y x) <- (r7 x y)
            ; (r7 y x) <- (r8 x y)
            
            ; <dlptransitivity>
            (define-primitive-role (transitive-closure r9) r9) ; (r9 x z) <- (and (r9 x y) (r9 y z))
            
            ; <dlpassertion>
            (instance i26 (all r10 c21)) ; (c21 x) <- (r10 i26 x)            
            
            ) ; end of ontology
  
  (define (engine max-nodes max-turns)
    (make-engine max-nodes max-turns 
                 (list (generate-arguments-from-ontology kb1 '()))))
  
  (define e1 (engine 20 2))  
  
  (printn "starting with ontology tests ...")
  
  (run-single-test (all-in? '(c2 i1) e1)
                   "ontologies: dlpcinclusion (simple)"
                   "ontology test: (c2 i1) should be acceptable! (dlpcinclusion - simple)")
  
  (run-single-test (and (all-in? '(c3 i2) e1)
                        (all-in? '(c4 i2) e1))
                   "ontologies: dlpcinclusion (conjunction)"
                   "ontology test: (c3 i2) and (c4 i2) should be acceptable! (dlpcinclusion - conjunction)")
  
  (run-single-test (and (all-in? '(c9 i33) e1)
                        (all-in? '(c9 i44) e1))
                   "ontologies: dlpcinclusion (disjunction and univrestriction)"
                   "ontology test: (c9 i33) and (c9 i44) should be acceptable! (dlpcinclusion - disjunction and univrestriction)")
  
  (run-single-test (all-in? '(c12 i5) e1)
                   "ontologies: dlpcinclusion (existrestriction)"
                   "ontology test: (c2 i5) should be acceptable! (dlpcinclusion - existrestriction)")
  
  (run-single-test (and (all-in? '(c13 i7) e1)
                        (all-in? '(c14 i8) e1))
                   "ontologies: dlpcequivalence (simple)"
                   "ontology test: (c13 i7) and (c14 i8) should be acceptable! (dlpcequivalence - simple)")
  
  (run-single-test (and (all-in? '(c15 i9) e1)
                        (all-in? '(c16 i9) e1)
                        (all-in? '(c17 i10) e1)
                        (all-in? '(c18 i10) e1))
                   "ontologies: dlpcequivalence (conjunction)"
                   "ontology test: (c15 i9), (c16 i9), (c17 i10) and (c18 i10) should be acceptable! (dlpcequivalence - conjunction)")
  
  (run-single-test (all-in? '(c19 i12) e1)
                   "ontologies: dlprange"
                   "ontology test: (c19 i12) should be acceptable! (dlprange)")
  
  (run-single-test (all-in? '(c20 i11) e1)
                   "ontologies: dlpdomain"
                   "ontology test: (c20 i11) should be acceptable! (dlpdomain)")
  
  (run-single-test (all-in? '(r4 i13 i14) e1)
                   "ontologies: dlprinclusion"
                   "ontology test: (r4 i13 i14) should be acceptable! (dlprinclusion)")
  
  (run-single-test (and (all-in? '(r5 i15 i16) e1)
                        (all-in? '(r6 i17 i18) e1))
                   "ontologies: dlprequivalence"
                   "ontology test: (r5 i15 i16) and (r6 i17 i18) should be acceptable! (dlprequivalence)")
  
  (run-single-test (and (all-in? '(r8 i20 i19) e1)
                        (all-in? '(r7 i22 i21) e1))
                   "ontologies: dlpinverse"
                   "ontology test: (r8 i20 i19) and (r7 i22 i21) should be acceptable! (dlpinverse)")
  
  (run-single-test (all-in? '(r9 i23 i25) e1)
                   "ontologies: dlptransitivity"
                   "ontology test: (r9 i23 i25) should be acceptable! (dlptransitivity)")
  
  (run-single-test (all-in? '(c21 i27) e1)
                   "ontologies: dlpassertion (univrestriction)"
                   "ontology test: (c21 i27) should be acceptable! (dlpassertion - univrestriction)")
  
  (newline)
  
  )

; -------------------------------
; lkif tests
; -------------------------------

(define (run-lkif-tests)
  
  (define i (lkif-import "lkif-test.xml"))
  
  (define sources (lkif-data-sources i))
  
  (define src1 (car sources))
  
  (define src2 (cadr sources))
  
  (define axioms (lkif-data-context i))
  
  (define rb1 (lkif-data-rulebase i))
  
  (define stages (lkif-data-stages i))
  
  (define stage1 (car stages))
  
  ;(view (stage-argument-graph stage1) (stage-context stage1))
  
  ; d1 should have a string-length of 645 
  (define d1 (call-with-values open-string-output-port 
                               (lambda (p e)
                                 (diagram* (stage-argument-graph stage1)
                                           (stage-context stage1)
                                           identity 
                                           (lambda (s) (format "~a" s)) p)
                                 (e))))  
  
  ; string-length of e1 is 3037 in plt and 3034 in ypsilon
  (define e1 (call-with-values open-string-output-port
                               (lambda (p e)
                                 (lkif-export i p)
                                 (e)))) 

  
  
  (define (engine max-nodes max-turns critical-questions)
    (make-engine* max-nodes max-turns
                  axioms
                  (list (generate-arguments-from-rules rb1 critical-questions) builtins)))
  
  (define lkif-engine1 (engine 20 2 '()))
  (define lkif-engine2 (engine 20 2 '(excluded)))
  
  (printn "starting with lkif-tests ...")
  
  (run-single-test (and (string=? (source-uri src1) "http://fokus.lkif.test.de")
                        (string=? (source-element src1) "p")
                        (string=? (source-uri src2) "http://fokus.lkif.test.de")
                        (string=? (source-element src2) "q"))
                   "lkif-import: sources"
                   "lkif-import test: the imported list of sources is not identical with the presetting")
  
  (run-single-test (= (string-length d1) 645)
                   "lkif-import: argument-diagram"
                   "lkif-import test: the imported diagram is not identical with the presetting")
  
  (run-single-test (all-in? '(flies ?bird) lkif-engine1)
                   "lkif-import: rule-base (simple)"
                   "lkif-import test: (flies Tweety) should be acceptable (simple)")
  
  (run-single-test (not (some-in? '(flies ?bird) lkif-engine2))
                   "lkif-import: rule-base (excluded)"
                   "lkif-import test: (flies Tweety) should not be acceptable (excluded)")
  
  (run-single-test (or (= (string-length e1) 3034)
                       (= (string-length e1) 3037))
                   "lkif-export: file-creation"
                   "lkif-export: the created lkif-file is not identical with the presetting")
  
  (newline)
  
  )


; -------------------------------
; dnf tests
; -------------------------------

(define (run-dnf-tests)
  
  (define t1 'a)
  (define t2 '(p x))
  (define t3 '(or a b))
  (define t4 '(and a b))
  (define t5 '(or (p a) (p b)))
  (define t6 '(and (or a b) c))
  (define t7 '(and (or a b) (or c d)))
  (define t8 '(and (or a b) (or a b)))
  (define t9 '(if (or a b) (or c d)))
  (define t10 '(and (or a b c) (or c d e)))
  (define t11 '(not (and (or a b c) (or c d e))))
  (define t12 '(not (or (or a b (not c)) (and (not (or c d)) (not (and e f))))))
  (define t13 '(not (iff (and (or a b) (not c)) d)))

  
  (define (test-term t c)
    (let* ((s (call-with-values open-string-output-port (lambda (p e)
                                                         (put-datum p t)
                                                         (e))))
           (d (to-dnf t)))
      (run-single-test (and d
                            (dnf? d)
                            (if c
                                (compare-formulas t d)
                                #t))
                       (string-append "dnf term: " s)
                       "dnf: term could not be converted to dnf or conversion is not logical equivalent")))
  
  
  (printn "starting with dnf tests ...")
  (test-term t1 #t)
  (test-term t2 #t)
  (test-term t3 #t)
  (test-term t4 #t)
  (test-term t5 #t)
  (test-term t6 #t)
  (test-term t7 #t)
  (test-term t8 #t)
  (test-term t9 #f)
  (test-term t10 #t)
  (test-term t11 #t)
  (test-term t12 #t)
  (test-term t13 #f)
  
  (newline)
  
  )


; -------------------------------
; test run and report
; -------------------------------

(define (run-all-tests)
  (printn "running all tests ...")
  (newline)
  (init-errors)
  (run-argument-tests)
  (run-case-tests)
  (run-rule-tests)
  (run-ontology-tests)
  (run-lkif-tests)
  (run-dnf-tests)
  (printn "all tests finished")
  (newline))

(define (test-report)
  (if (> error-counter 0)
      (begin (display "number of errors occurred: ")
             (printn error-counter)
             (display error-message)
             (newline)
             (newline)
             (printn "!!! diagram-errors may appear, if the tests are run under a different system")
             (printn "!!! this results from the system-specific handling of lists - don't worry!")
             (newline))
      (printn "all tests successful")))

; -------------------------------
; main program
; -------------------------------

(run-all-tests)

(test-report)

(newline)
(printn "--------------")
