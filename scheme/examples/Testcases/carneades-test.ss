#!r6rs

(import (except (rnrs) assert values)
        (carneades argument)
        (carneades argument-diagram)
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
  
  (define d1 (call-with-values open-string-output-port (lambda (port extract)
                                                         (diagram* tweety-graph
                                                                   c1
                                                                   identity
                                                                   (lambda (s) (format "~a" s))
                                                                   port)
                                                         (extract))))
  
  ; plt-scheme
  (define d2 "digraph g {\n    rankdir = \"RL\";\n    g1 [shape=box, label=\"Tweety is a penguin\", style=\"filled\"];\n    g2 [shape=box, label=\"Tweety is an abnormal bird\", style=\"filled\"];\n    g3 [shape=box, label=\"Birds fly\", style=\"\"];\n    g4 [shape=box, label=\"Penguins are abnormal birds\", style=\"\"];\n    g5 [shape=box, label=\"Tweety is a bird\", style=\"filled\"];\n    g6 [shape=box, label=\"Tweety flies\", style=\"\"];\n    g7 [shape=ellipse, label=\"a1\", style=\"\"];\n    g7 -> g6;\n    g5 -> g7 [arrowhead=\"none\"];\n    g3 -> g7 [arrowhead=\"dot\"];\n    g2 -> g7 [arrowhead=\"odot\"];\n    g8 [shape=ellipse, label=\"a2\", style=\"filled\"];\n    g8 -> g2;\n    g1 -> g8 [arrowhead=\"none\"];\n    g4 -> g8 [arrowhead=\"dot\"];\n}\n")
  
  ; ypsilon
  (define d3 "digraph g {\n    rankdir = \"RL\";\n    g1 [shape=box, label=\"Tweety is an abnormal bird\", style=\"filled\"];\n    g2 [shape=box, label=\"Penguins are abnormal birds\", style=\"\"];\n    g3 [shape=box, label=\"Tweety is a penguin\", style=\"filled\"];\n    g4 [shape=box, label=\"Tweety flies\", style=\"\"];\n    g5 [shape=box, label=\"Birds fly\", style=\"\"];\n    g6 [shape=box, label=\"Tweety is a bird\", style=\"filled\"];\n    g7 [shape=ellipse, label=\"a2\", style=\"filled\"];\n    g7 -> g1;\n    g3 -> g7 [arrowhead=\"none\"];\n    g2 -> g7 [arrowhead=\"dot\"];\n    g8 [shape=ellipse, label=\"a1\", style=\"\"];\n    g8 -> g4;\n    g6 -> g8 [arrowhead=\"none\"];\n    g5 -> g8 [arrowhead=\"dot\"];\n    g1 -> g8 [arrowhead=\"odot\"];\n}\n")
  
  (printn "starting with argument tests ...")
  
  (display "testing argument-diagram ...")  
  
  (if (or (string=? d1 d2) (string=? d1 d3))
      (printn " OK")
      (begin (adderror "argument-diagram: the created diagram is not identical with the presetting")
             (printn " failed!")))
  
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

(display "testing simple rules ...")
(if (all-in? '(bird Tweety) (engine 20 1 rb1 as1 null))
    (printn " OK") 
    (begin (adderror "rule test: (bird Tweety) should be acceptable! (simple)")
           (printn " failed!")))

(display "testing simple rules with variables ...")
(if (all-in? '(bird ?x) (engine 20 1 rb1 as1 null))
    (printn " OK")
    (begin (adderror "rule test: (bird Tweety) should be acceptable! (simple variables)")
           (printn " failed!")))

; coins are money
(display "testing simple rules ...")
(if (all-in? '(money item1) (engine 20 1 rb1 as1 null))
    (printn " OK") 
    (begin (adderror "rule test: (money item1) should be acceptable! (simple)")
           (printn " failed!"))) 

(display "testing rules with conjunctions ...")
(if (all-in? '(prior ?r1 ?r2) (engine 20 1 rb1 as1 null)) 
    (printn " OK") 
    (begin (adderror "rule test: (prior r6 r1) should be acceptable! (conjunction)")
           (printn " failed!")))

; disjunction of atomic statements
(display "testing rules with disjunctions of atomic statements ...")
(if (all-in? '(p3 a) (engine 20 1 rb1 as1 null))
    (printn " OK")
    (begin (adderror "rule test: (p3 a) should be acceptable! (disjunction of atomic statements)")
           (printn " failed!"))) 

; disjunction of conjunctions
(display "testing rules with disjunctions of conjunctions ...")
(if (all-in? '(p9 a) (engine 20 1 rb1 as1 null))
    (printn " OK")
    (begin (adderror "rule test: (p9 a) should be acceptable! (disjunction of conjunction)") 
           (printn " failed!"))) 

; find pro argument
(display "testing rules with one turn ...")
(if (all-in? '(goods item1) (engine 20 1 rb1 as1 null))
    (printn " OK")
    (begin (adderror "rule test: (goods item1) should be acceptable! (one turn)")
           (printn " failed!"))) 

; unless money exception
(display "testing rules with two turns ...")
(if (not (all-in? '(goods item1) (engine 20 2 rb1 as1 null)))
    (printn " OK") 
    (begin (adderror "rule test: (goods item1) should not be acceptable! (2 turns - exception)")
           (printn " failed!"))) 

; rebuttal: edible things are not goods
(display "testing rules with rebuttals ...")
(if (not (all-in? '(goods item2) (engine 20 2 rb1 as1 null))) 
    (printn " OK")
    (begin (adderror "rule test: (goods item2) should not be acceptable! (rebuttal - this failure is known and should appear)")
           (printn " failed!")))

; repealed rules are not valid
(display "testing repealed rules should not be acceptable ...")
(if (not (all-in? '(convenient item1) (engine 20 2 rb1 as1 '(valid))))
    (printn " OK") 
    (begin (adderror "rule test: (convenient item1) should not be acceptable! (repealed rules are not valid)") 
           (printn " failed!")))

; lex posterior
(display "testing lex posterior rules ...")
(if (not (all-in? '(goods item2) (engine 20 2 rb1 as1 '(priority)))) 
    (printn " OK") 
    (begin (adderror "rule test: (goods item2) should not be acceptable! (lex posterior)")
           (printn " failed!")))   

; to do: fix the following test. The success predicate tests only whether one is found, not all  
; (test-true "multiple rule conclusions" (all-in? '(convenient ?x)) (engine 20 1 null))

(display "testing rules with 3 turns and negated query ...")
(if (all-in? '(not (goods item2)) (engine 20 3 rb1 as1 null)) 
    (printn " OK") 
    (begin (adderror "rule test: (not (goods item2)) should be acceptable! (3 turns - negated query)")
           (printn " failed!")))

(display "testing exclusionary rules ...")
(if (not (all-in? '(flies Tweety) (engine 20 2 rb1 as1 '(excluded))))
    (printn " OK")
    (begin (adderror "rule test: (flies Tweety) should not be acceptable! (exclusionary)")
           (printn " failed!")))

(display "testing rules with applies ...")
(if (all-in? '(applies ?r (goods ?x)) (engine 20 1 rb1 as1 null))
    (printn " OK")
    (begin (adderror "rule test: (applies (goods item1)) and (applies (goods item2)) shpuld be acceptable! (applies)")
           (printn " failed!")))

; to do: test negative conditions and exceptions
; to do: test rules with negative conclusions

; reverse a list
(display "testing rules with eval (reverse) ...")
(if (all-in? '(p11 ?x) (engine 20 1 rb1 as1 null)) 
    (printn " OK")
    (begin (adderror "rule test: (p11 (e d c b a)) should be acceptable! (eval)")
           (printn " failed!"))) 

; calculations
(display "testing rules with eval (calculation) ...")
(if (all-in? '(taxable-income Sam ?x) (engine 20 1 rb1 as1 null))
    (printn " OK")
    (begin (adderror "rule test: (taxable-income Sam 53000) should be acceptable! (eval)")
           (printn " failed!"))) 

; to do: test assumptions -- a statement is questioned by making an argument pro or con the statement
; to do: event calculus tests


; -- Royal Elephant Benchmark

(display "testing rules: Royal Elephant Benchmark 1 ...")
(if (some-in? '(gray ?x) (engine 20 2 elephants-rulebase elephant-facts null))
    (printn " OK")
    (begin (adderror "rule test: (gray dumbo) should be acceptable! (Royal Elephant Benchmark)")
           (printn " failed!"))) 

(display "testing rules: Royal Elephant Benchmark 2 ...")
(if (some-in? '(not (gray ?x)) (engine 20 2 elephants-rulebase elephant-facts null))
    (printn " OK")
    (begin (adderror "rule test: (not (gray clyde)) should be acceptable! (Royal Elephant Benchmark)")
           (printn " failed!"))) 


; -- Pennsylvania Dutch Benchmark

(display "testing rules: Pennsylvania Dutch Benchmark 1 ...")
(if (some-in? '(born ?x ?y) (engine 20 2 dutch-rulebase dutch-facts null))
    (printn " OK")
    (begin (adderror "rule test: (born Herman America) should be acceptable! (Pennsylvania Dutch Benchmark)")
           (printn " failed!"))) 

(display "testing rules: Pennsylvania Dutch Benchmark 2 ...")
(if (some-in? '(not (born ?x America)) (engine 20 2 dutch-rulebase dutch-facts null))
    (printn " OK")
    (begin (adderror "rule test: (not (born Fritz America)) should be acceptable! (Pennsylvania Dutch Benchmark)")
           (printn " failed!"))) 


; -- Gullible Citizens Benchmark

(display "testing rules: Gullible Citizens Benchmark 1 ...")
(if (some-in? '(not (like ?x ?y)) (engine 20 2 gullible-rulebase gullible-facts '(excluded)))
    (printn " OK")
    (begin (adderror "rule test: (not (like John Dick)) should be acceptable! (Gullible Citizens Benchmark)")
           (printn " failed!")))

(display "testing rules: Gullible Citizens Benchmark 2 ...")
(if (not (some-in? '(not (like Fred Dick)) (engine 20 2 gullible-rulebase gullible-facts '(excluded))))
    (printn " OK")
    (begin (adderror "rule test: (not (like Fred Dick)) should not be acceptable! (Gullible Citizens Benchmark)")
           (printn " failed!")))


; -- Blocks World Benchmark

(display "testing rules: Blocks World Benchmark 1 ...")
(if (some-in? '(block ?x) (engine 20 2 blocks-world-rulebase blocks-world-facts '(excluded)))
    (printn " OK")
    (begin (adderror "rule test: (block C), (block B) and (block A) should be acceptable! (Blocks World Benchmark)")
           (printn " failed!")))

(display "testing rules: Blocks World Benchmark 2 ...")
(if (some-in? '(on ?x table) (engine 20 2 blocks-world-rulebase blocks-world-facts '(excluded)))
    (printn " OK")
    (begin (adderror "rule test: (on C table) should be acceptable! (Blocks World Benchmark)")
           (printn " failed!")))

(display "testing rules: Blocks World Benchmark 3 ...")
(if (some-in? '(not (on ?x table)) (engine 20 2 blocks-world-rulebase blocks-world-facts '(excluded)))
    (printn " OK")
    (begin (adderror "rule test: (not (on B table)) should be acceptable! (Blocks World Benchmark)")
           (printn " failed!")))


; -- Dancer Benchmark

(display "testing rules: Dancer Benchmark 1 ...")
(if (some-in? '(dancer Sally) (engine 50 2 dancer-rulebase dancer-facts '(excluded)))
    (printn " OK")
    (begin (adderror "rule test: (dancer Sally) should be acceptable! (Dancer Benchmark)")
           (printn " failed!")))

(display "testing rules: Dancer Benchmark 2 ...")
(if (not (some-in? '(ballerina Sally) (engine 50 2 dancer-rulebase dancer-facts '(excluded))))
    (printn " OK")
    (begin (adderror "rule test: (ballerina Sally) should not be acceptable! (Dancer Benchmark)")
           (printn " failed!")))  

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
  
  (display "testing ontologies: dlpcinclusion (simple) ...")
  (if (all-in? '(c2 i1) e1)
      (printn " OK")
      (begin (adderror "ontology test: (c2 i1) should be acceptable! (dlpcinclusion - simple)")
             (printn " failed!"))) 
  
  (display "testing ontologies: dlpcinclusion (conjunction) ...")
  (if (and (all-in? '(c3 i2) e1)
           (all-in? '(c4 i2) e1))
      (printn " OK")
      (begin (adderror "ontology test: (c3 i2) and (c4 i2) should be acceptable! (dlpcinclusion - conjunction)")
             (printn " failed!")))
  
  (display "testing ontologies: dlpcinclusion (disjunction and univrestriction) ...")
  (if (and (all-in? '(c9 i33) e1)
           (all-in? '(c9 i44) e1))
      (printn " OK")
      (begin (adderror "ontology test: (c9 i33) and (c9 i44) should be acceptable! (dlpcinclusion - disjunction and univrestriction)")
             (printn " failed!")))
  
  (display "testing ontologies: dlpcinclusion (existrestriction) ...")
  (if (all-in? '(c12 i5) e1)          
      (printn " OK")
      (begin (adderror "ontology test: (c2 i5) should be acceptable! (dlpcinclusion - existrestriction)")
             (printn " failed!")))
  
  (display "testing ontologies: dlpcequivalence (simple) ...")
  (if (and (all-in? '(c13 i7) e1)
           (all-in? '(c14 i8) e1))
      (printn " OK")
      (begin (adderror "ontology test: (c13 i7) and (c14 i8) should be acceptable! (dlpcequivalence - simple)")
             (printn " failed!")))
  
  (display "testing ontologies: dlpcequivalence (conjunction) ...")
  (if (and (all-in? '(c15 i9) e1)
           (all-in? '(c16 i9) e1)
           (all-in? '(c17 i10) e1)
           (all-in? '(c18 i10) e1))
      (printn " OK")
      (begin (adderror "ontology test: (c15 i9), (c16 i9), (c17 i10) and (c18 i10) should be acceptable! (dlpcequivalence - conjunction)")
             (printn " failed!")))
  
  (display "testing ontologies: dlprange ...")
  (if (all-in? '(c19 i12) e1)
      (printn " OK")
      (begin (adderror "ontology test: (c19 i12) should be acceptable! (dlprange)")
             (printn " failed!")))
  
  (display "testing ontologies: dlpdomain ...")
  (if (all-in? '(c20 i11) e1)
      (printn " OK")
      (begin (adderror "ontology test: (c20 i11) should be acceptable! (dlpdomain)")
             (printn " failed!")))
  
  (display "testing ontologies: dlprinclusion ...")
  (if (all-in? '(r4 i13 i14) e1)
      (printn " OK")
      (begin (adderror "ontology test: (r4 i13 i14) should be acceptable! (dlprinclusion)")
             (printn " failed!")))
  
  (display "testing ontologies: dlprequivalence ...")
  (if (and (all-in? '(r5 i15 i16) e1)
           (all-in? '(r6 i17 i18) e1))
      (printn " OK")
      (begin (adderror "ontology test: (r5 i15 i16) and (r6 i17 i18) should be acceptable! (dlprequivalence)")
             (printn " failed!")))
  
  (display "testing ontologies: dlpinverse ...")
  (if (and (all-in? '(r8 i20 i19) e1)
           (all-in? '(r7 i22 i21) e1))
      (printn " OK")
      (begin (adderror "ontology test: (r8 i20 i19) and (r7 i22 i21) should be acceptable! (dlpinverse)")
             (printn " failed!")))
  
  (display "testing ontologies: dlptransitivity ...")
  (if (all-in? '(r9 i23 i25) e1)
      (printn " OK")
      (begin (adderror "ontology test: (r9 i23 i25) should be acceptable! (dlptransitivity)")
             (printn " failed!")))
  
  (display "testing ontologies: dlpassertion (univrestriction) ...")
  (if (all-in? '(c21 i27) e1)
      (printn " OK")
      (begin (adderror "ontology test: (c21 i27) should be acceptable! (dlpassertion - univrestriction)")
             (printn " failed!")))
  
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
  
  (define d1 (call-with-values open-string-output-port 
                               (lambda (p e)
                                 (diagram* (stage-argument-graph stage1)
                                           (stage-context stage1)
                                           identity 
                                           (lambda (s) (format "~a" s)) p)
                                 (e))))
  
  (define e1 (call-with-values open-string-output-port
                               (lambda (p e)
                                 (lkif-export i p)
                                 (e))))
  
  ; plt
  (define d2 "digraph g {\n    rankdir = \"RL\";\n    g589 [shape=box, label=\"Tweety is a penguin.\", style=\"filled\"];\n    g590 [shape=box, label=\"Birds normally fly.\", style=\"\"];\n    g591 [shape=box, label=\"Tweety is an abnormal bird.\", style=\"filled\"];\n    g592 [shape=box, label=\"Tweety is a bird.\", style=\"filled\"];\n    g593 [shape=box, label=\"Tweety can fly.\", style=\"\"];\n    g594 [shape=ellipse, label=\"a1\", style=\"\"];\n    g594 -> g593;\n    g592 -> g594 [arrowhead=\"none\"];\n    g590 -> g594 [arrowhead=\"dot\"];\n    g591 -> g594 [arrowhead=\"odot\"];\n    g595 [shape=ellipse, label=\"a2\", style=\"filled\"];\n    g595 -> g591;\n    g589 -> g595 [arrowhead=\"none\"];\n}\n")
  
  ; ypsilon
  (define d3 "digraph g {\n    rankdir = \"RL\";\n    g589 [shape=box, label=\"Tweety is an abnormal bird.\", style=\"filled\"];\n    g590 [shape=box, label=\"Tweety is a penguin.\", style=\"filled\"];\n    g591 [shape=box, label=\"Tweety can fly.\", style=\"\"];\n    g592 [shape=box, label=\"Birds normally fly.\", style=\"\"];\n    g593 [shape=box, label=\"Tweety is a bird.\", style=\"filled\"];\n    g594 [shape=ellipse, label=\"a2\", style=\"filled\"];\n    g594 -> g589;\n    g590 -> g594 [arrowhead=\"none\"];\n    g595 [shape=ellipse, label=\"a1\", style=\"\"];\n    g595 -> g591;\n    g593 -> g595 [arrowhead=\"none\"];\n    g592 -> g595 [arrowhead=\"dot\"];\n    g589 -> g595 [arrowhead=\"odot\"];\n}\n")
  
  (define e2 "<lkif>\n  <sources>\n    <source uri=\"http://fokus.lkif.test.de\" element=\"p\"/>\n    <source uri=\"http://fokus.lkif.test.de\" element=\"q\"/>\n  </sources>\n  <theory id=\"theory596\">\n    <axioms>\n      <axiom id=\"a597\">\n        <s pred=\"is-a-penguin\">\n          <c>Tweety</c>\n        </s>\n      </axiom>\n    </axioms>\n    <rules>\n      <rule id=\"r1\" strict=\"true\">\n        <head>\n          <s pred=\"is-an-abnormal-bird\">\n            <v>bird</v>\n          </s>\n        </head>\n        <body>\n          <s pred=\"is-a-penguin\">\n            <v>bird</v>\n          </s>\n        </body>\n      </rule>\n      <rule id=\"r2\" strict=\"true\">\n        <head>\n          <s pred=\"is-a-bird\">\n            <v>bird</v>\n          </s>\n        </head>\n        <body>\n          <s pred=\"is-a-penguin\">\n            <v>bird</v>\n          </s>\n        </body>\n      </rule>\n      <rule id=\"r3\" strict=\"false\">\n        <head>\n          <s pred=\"flies\">\n            <v>bird</v>\n          </s>\n        </head>\n        <body>\n          <s pred=\"is-a-bird\">\n            <v>bird</v>\n          </s>\n        </body>\n      </rule>\n      <rule id=\"r4\" strict=\"false\">\n        <head>\n          <s pred=\"excluded\">\n            <c>r3</c>\n            <s pred=\"flies\">\n              <v>bird</v>\n            </s>\n          </s>\n        </head>\n        <body>\n          <s pred=\"is-a-penguin\">\n            <v>bird</v>\n          </s>\n        </body>\n      </rule>\n    </rules>\n  </theory>\n  <argument-graphs>\n    <argument-graph id=\"ag603\" title=\"\" main-issue=\"\">\n      <statements>\n        <statement id=\"s598\" value=\"true\" assumption=\"false\" standard=\"BA\">\n          <s>Tweety is a penguin.</s>\n        </statement>\n        <statement id=\"s599\" value=\"unknown\" assumption=\"true\" standard=\"BA\">\n          <s assumable=\"true\">Birds normally fly.</s>\n        </statement>\n        <statement id=\"s600\" value=\"unknown\" assumption=\"true\" standard=\"BA\">\n          <s>Tweety is an abnormal bird.</s>\n        </statement>\n        <statement id=\"s601\" value=\"true\" assumption=\"false\" standard=\"BA\">\n          <s>Tweety is a bird.</s>\n        </statement>\n        <statement id=\"s602\" value=\"unknown\" assumption=\"true\" standard=\"BA\">\n          <s>Tweety can fly.</s>\n        </statement>\n      </statements>\n      <arguments>\n        <argument id=\"a1\" title=\"\" direction=\"pro\" scheme=\"\" weight=\"0.5\">\n          <conclusion statement=\"s602\"/>\n          <premises>\n            <premise polarity=\"positive\" exception=\"false\" role=\"\" statement=\"s601\"/>\n            <premise polarity=\"positive\" exception=\"false\" role=\"\" statement=\"s599\"/>\n            <premise polarity=\"positive\" exception=\"true\" role=\"\" statement=\"s600\"/>\n          </premises>\n        </argument>\n        <argument id=\"a2\" title=\"\" direction=\"pro\" scheme=\"\" weight=\"0.5\">\n          <conclusion statement=\"s600\"/>\n          <premises>\n            <premise polarity=\"positive\" exception=\"false\" role=\"\" statement=\"s598\"/>\n          </premises>\n        </argument>\n      </arguments>\n    </argument-graph>\n  </argument-graphs>\n</lkif>")

  
  (define (engine max-nodes max-turns critical-questions)
    (make-engine* max-nodes max-turns
                  axioms
                  (list (generate-arguments-from-rules rb1 critical-questions) builtins)))
  
  (define lkif-engine1 (engine 20 2 '()))
  (define lkif-engine2 (engine 20 2 '(excluded)))
  
  (printn "starting with lkif-tests ...")
  
  (display "testing lkif-import: sources ...")
  (if (and (string=? (source-uri src1) "http://fokus.lkif.test.de")
           (string=? (source-element src1) "p")
           (string=? (source-uri src2) "http://fokus.lkif.test.de")
           (string=? (source-element src2) "q"))
      (printn " OK")
      (begin (adderror "lkif-import test: the imported list of sources is not identical with the presetting")
             (printn " failed!")))
  
  (display "testing lkif-import: argument-diagram ...")
  (if (or (string=? d1 d2)
          (string=? d1 d3))
      (printn " OK")
      (begin (adderror "lkif-import test: the imported diagram is not identical with the presetting")
             (printn " failed!")))  
  
  (display "testing lkif-import: rule-base (simple) ...")
  (if (all-in? '(flies ?bird) lkif-engine1)
      (printn " OK")
      (begin (adderror "lkif-import test: (flies Tweety) should be acceptable (simple)")
             (printn " failed!")))
  
  (display "testing lkif-import: rule-base (excluded) ...")
  (if (not (some-in? '(flies ?bird) lkif-engine2))
      (printn " OK")
      (begin (adderror "lkif-import test: (flies Tweety) should not be acceptable (excluded)")
             (printn " failed!")))
  
  (display "testing lkif-export: file-creation ...")
  (if (string=? e1 e2)
      (printn " OK")
      (begin (adderror "lkif-export: the created lkif-file is not identical with the presetting")
             (printn " failed!")))
  
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
    (display "testing dnf term: ")
    (display t)
    (display " ... ")
    (let ((d (to-dnf t)))
      (if (and d
               (dnf? d)
               (if c
                   (compare-formulas t d)
                   #t))
          (printn " OK")
          (begin (adderror "dnf: term could not be converted to dnf or conversion is not logical equivalent")
                 (printn " failed!")))))
     
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
