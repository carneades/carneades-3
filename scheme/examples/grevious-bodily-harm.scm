#!r6rs

(import (rnrs)
        (carneades argument)
        (carneades argument-diagram))


(define eight-years 
  "The accused is punishable
by up to 8 years
in imprisonment.")

(define bodily-harm-rule 
  "Inflicting grievous bodily
harm is punishable by up to 
8 years imprisonment.")

(define article-302 
  "According to article 302 of
the Dutch criminal code, inflicting
grievous bodily harm is punishable
by up to 8 years imprisonment.")

(define bodily-harm 
  "The accused has inflicted grievous 
bodily harm upon the victim.")

(define ten-witnesses
  "10 pub customers: 
the accused was involved 
in the fight.")

(define testimony-of-the-accused 
  "I was not involved 
in the fight.")

(define broken-ribs-not-sufficient
  "Several broken ribs do not 
amount to grievous bodily harm.")

(define precedent-1 
  "The rule that several broken 
ribs does not amount to grievous
bodily harm, explains precedent 1.")

(define lex-specialis
  "The rule explaining precedent 2 
is more specific than the rule 
explaining precedent 1.")

(define sufficient-with-complications
  "Several broken ribs with
complications amount to 
grievous bodily harm.")

(define precedent-2
  "The rule that several broken 
ribs with complications amount 
to grievous bodily harm 
explains precedent 2.")

(define hospital-report
  "The victim has several broken 
ribs, with complications.")

(define-argument a1 (pro eight-years (pr bodily-harm) (pr bodily-harm-rule)))
(define-argument a2 (pro bodily-harm-rule (am article-302)))
(define-argument a3 (pro bodily-harm (pr hospital-report)))
(define-argument a4 (con bodily-harm (pr testimony-of-the-accused)))
(define-argument a5 (con bodily-harm (pr broken-ribs-not-sufficient)))
(define-argument a6 (con testimony-of-the-accused (pr ten-witnesses)))
(define-argument a7 (con broken-ribs-not-sufficient (pr sufficient-with-complications)))
(define-argument a8 (pro sufficient-with-complications (am precedent-2)))
(define-argument a9 (pro broken-ribs-not-sufficient (am precedent-1) 
                         (ex lex-specialis)))

(define ag1 (assert-arguments empty-argument-graph (list a1 a2 a3 a4 a5 a6 a7 a8 a9)))
(define c1 (accept default-context  (list ten-witnesses hospital-report lex-specialis)))

; (view ag1 c1)
(diagram ag1 c1)
