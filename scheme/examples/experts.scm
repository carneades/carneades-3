#!r6rs

(import (rnrs)
        (carneades argument)
        (carneades argument-diagram))


(define s0 "S0")

(define s1
  "S1: E1 says S is true.") 

(define s2
  "S2: E1 is an expert in 
the domain of S.")

(define s3
  "S3: E2 says S is true.")

(define s4
  "S4: E2 is an expert in 
the domain of S.")

(define s5
  "S5: E1 says E2 is not trustworthy 
because he is biased.")

(define s6
  "S6: E2 says E1 is not trustworthy 
because he is biased.")

(define s7
  "S7: E1 is not trustworthy.")

(define s8
  "S8: E2 is not trustworthy.")


(define-argument a1 (pro s0 (pr s1) (am s2) (ex s7)))
(define-argument a2 (pro s0 (pr s3) (am s4) (ex s8)))
(define-argument a3 (pro s7 (pr s6) (ex s8)))
(define-argument a4 (pro s8 (pr s5))) ; (ex s7))) causes a cycle

(define args (list a1 a2 a3 a4))

(define ag1 (assert-arguments empty-argument-graph args))
(define ag2 (accept ag1 (list s1 s2 s3 s4)))
(define ag3 (accept ag2 (list s5)))
(define ag4 (accept ag3 (list s6)))

; (view ag1)
; (view ag2)
; (view ag3)
; (view ag4)
