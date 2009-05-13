#!r6rs

(import (rnrs base)
        (carneades base)
        (carneades argument)
        (carneades argument-builtins)
        (carneades rule)
        (carneades shell)
        (carneades lib srfi lightweight-testing)
        (carneades stream)
        (carneades argument-search)
        (carneades table))


; Examples from "The Case for Explicit Exceptions", by L. Thorne McCarty and William W. Cohen

; type question = excluded | priority | valid

; engine integer integer rulebase (list-of statement) (list-of symbol) -> statement -> (stream-of argument-state)
(define (engine max-nodes max-turns rules assumptions critical-questions)
  (make-engine* max-nodes max-turns 
                (accept empty-argument-graph assumptions)
                (list builtins 
                      (generate-arguments-from-rules rules critical-questions) 
                      )))

; Royal Elephants Benchmark

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

(define e1 (engine 20 2 elephants-rulebase elephant-facts null))
(check (some-in? '(gray ?x) e1) => #t)
(check (some-in? '(not (gray ?x)) e1) => #t)
  
; Pennsylvania Dutch Benchmark

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

(define e2 (engine 20 2 dutch-rulebase dutch-facts null))
(check (some-in? '(born ?x ?y) e2) => #t)
(check (some-in? '(not (born ?x America)) e2) => #t)

; Gullible Citizens Benchmark

(define gullible-rulebase
  (rulebase
   
   (rule r6 (if (and (citizen ?x)
                     (crook ?y))
                (not (like ?x ?y))))
   
   ; r7 would override r7 only if rebuttals are searched for and found
   (rule r7 (if (and (citizen ?x)
                     (gullible ?x)
                     (crook ?y)
                     (elected ?y))
                (like ?x ?y)))
   
;   (rule r7 (if (and (citizen ?x)
;                     (gullible ?x)
;                     (crook ?y)
;                     (elected ?y))
;                (excluded r6 (not (like ?x ?y)))))
))

(define gullible-facts
  '((citizen Fred)
    (citizen John)
    (gullible Fred)
    (crook Dick)
    (elected Dick)))

(define e3 (engine 20 2 gullible-rulebase gullible-facts '(excluded)))
(check (some-in? '(not (like ?x ?y)) e3) => #t)
(check (some-in? '(not (like Fred Dick)) e3) => #f)

; Blocks World Benchmark

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

(define e4 (engine 20 2 blocks-world-rulebase blocks-world-facts '(excluded)))
(check (some-in? '(block ?x) e4) => #t)
(check (some-in? '(on ?x table) e4) => #t)
(check (some-in? '(not (on ?x table)) e4) => #t)


; Dancer Benchmark
(define dancer-rulebase
   (rulebase
   
   ;     (rule r11
   ;           (if (dancer ?x)
   ;               (not (ballerina ?x))))
   
   (rule r12
         (if (dancer ?x)
             (graceful ?x)))
   
   (rule r13
         (if (and (dancer ?x)
                  (graceful ?x))
             (ballerina ?x)))
   
   ;     Rule r11 is problematical. The intention is to express 
   ;     that dancers are not normally or usually ballerinas.  
   ;     But can one construct an argument for someone not being a
   ;     ballerina by showing that she is a dancer?  Does her being a dancer 
   ;     give us a *reason* to believe she is not a ballerina?
   ;     If someone has put forward an argument for someone being a ballerina,
   ;     should it be possible to rebut this argument by proving she is a dancer?
   ;     In particular, if one uses r12 to argue that she is a ballerina, by
   ;     proving she is a graceful dancer, should it be possible to rebut this
   ;     argument by showing she is a dancer?  In my view, r11 illustrates two
   ;     different interpretations of defeasible rules:  1) as expressions of 
   ;     normality conditions (e.g. dancers are not normally ballerinas) and 
   ;     2) as inference rules or argumentation schemes for guiding reasoning 
   ;     (e.g. to argue that someone is a ballerina, prove she is a graceful 
   ;     dancer.)  Carneades is designed only for this second purpose, 
   ;     representing argumentation schemes.
   
   ;     Also, r12 and r13 alone hardly make sense as an expression
   ;     of reasoning policy.  With just these two rules, all dancers
   ;     are presumably ballerinas.  The second condition of r13 serves
   ;     no purpose.  They would make a bit more sense if there were
   ;     some exceptions to r12, such as the following:
   
     
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
  
   
   ; The policy issue is whether it is better to presume that 
   ; someone is graceful, knowing that he or she is a dancer.  
   ; What are the risks and benefits of acting on the 
   ; presumption that someone is graceful when she is not, 
   ; and vice versa?  It is not possible to evaluate the quality
   ; of these rules without such information.
   
   )) ; end of rule base



(define dancer-facts 
  '((dancer Naomi)
    (dancer Mikhail)
    (rock-and-roller Norbert)
    (square-dancer Sally)))

(define e5 (engine 50 2 dancer-rulebase dancer-facts '(excluded)))
(check (some-in? '(dancer Sally) e5) => #t)
(check (some-in? '(ballerina Sally) e5) => #f)

(check-report)




