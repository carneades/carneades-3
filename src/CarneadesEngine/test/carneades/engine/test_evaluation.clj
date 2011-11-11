;;; Copyright (c) 2011 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1


(ns carneades.engine.test-evaluation
  (:use clojure.test
        carneades.engine.statement
        carneades.engine.argument
        carneades.engine.scheme
        carneades.engine.argument-graph
        carneades.engine.argument-construction
        carneades.engine.argument-evaluation
        carneades.engine.caes
        carneades.engine.shell))


; Example argument graphs to test whether arguments are being evaluated properly.

; Benchmark 1. The Tandem example
; Source:
; Baroni, P., Caminada, M., and Giacomin, M. An introduction to argumentation semantics. 
; The Knowledge Engineering Review 00, 0 (2004), 1-24.

(def jw (make-statement :text {:en "John wants to ride on the tandem."}))
(def mw (make-statement :text {:en "Mary wants to ride on the tandem."}))
(def sw (make-statement :text {:en "Suzy wants to ride on the tandem."}))
(def jt (make-statement :text {:en "John is riding on the tandem."}))
(def mt (make-statement :text {:en "Mary is riding on the tandem."}))
(def st (make-statement :text {:en "Suzy is riding on the tandem."}))
(def bottom (make-statement :text {:en "The claims are inconsistent."}))


(def A1 (make-argument :conclusion bottom :premises [jt, mt, st]))   
(def A5 (make-argument :conclusion jt :premises [jw]))
(def A6 (make-argument :conclusion mt :premises [mw]))
(def A7 (make-argument :conclusion st :premises [sw]))

(def ag1 
  (-> (make-argument-graph)
      (assert-arguments [A1, A5, A6, A7])
      (assume [jw, mw, sw])))

; Using the CAES evaluator, bottom is in. Consistency
; can be restored by using abduction to find minimal
; changes to the argument graph which make bottom out.
(deftest test-tandem-carneades
   (is (in? (evaluate carneades-evaluator ag1) (:atom bottom))))


; The following examples are from:
; Prakken, H. An abstract framework for argumentation with structured arguments. 
; Argument & Computation 1, (2010), 93-124.

(def frisian (make-statement :text {:en "Wiebe is Frisian."}))
(def dutch (make-statement :text {:en "Wiebe is Dutch."}))
(def tall (make-statement :text {:en "Wiebe is Tall."}))

(def A1 (make-argument :strict true :conclusion frisian :premises [dutch]))
(def A2 (make-argument :conclusion tall :premises [dutch]))

(def ag2 
  (-> (make-argument-graph)
      (assert-arguments [A1, A2])
      (accept [frisian])))

(deftest test-frisian-carneades
   (is (in? (evaluate carneades-evaluator ag2) 
            (:atom tall))))

;; The next example shows how arguments can be constructed by instantiating schemes.
;; The scheme is instantiated manually and then used to construct arguments.
;; Schemes of this type, with a conclusion which is a schema variable ranging
;; over arbitrary literals, cannot be used effectively with a backwards
;; chaining, goal-directed strategy, since the conclusion matches every goal
;; literal.  Thus, in this example, the scheme is first instantiated in a
;; data-driven, forwards-chaining manner, to construct a custom version
;; which can be used to construct arguments via backwards chaining.  In
;; this example the scheme is fully instantiated, but this method can
;; also be used to partially instantiate schemes, so long as the predicate
;; of the conclusion of the scheme is instantiated before trying to use
;; backwards chaining to construct arguments.

(def expert-witness-scheme
  (make-scheme 
    :name "Expert Witness Testimony"
    :conclusions ['?P]
    :premises {:major '(expert ?E ?D), 
               :minor '(asserts ?E ?P)}
    :exceptions {:reliable '(not (reliable-as-source ?E)),
                 :consistent '(not (consistent-with-other-witnesses ?P))}
    :assumptions {:credible '(credible-expert ?E),
                  :backup-evidence '(based-on-evidence ?E)}))


(def expert-witness1
  (instantiate-scheme 
    expert-witness-scheme
    {'?P '(has-cavities Susan)
     '?E 'Joe
     '?D 'dentistry}))            

(def max-goals 10)

(def generators 
  (list (generate-arguments-from-scheme expert-witness1)))

(def case1-facts 
  '((expert Joe dentistry)
    (asserts Joe (has-cavities Susan))))

(def query '(has-cavities Susan))

(def ag3
  (construct-arguments query max-goals case1-facts generators))

(deftest test-expert-witness-carneades
   (is (in? (evaluate carneades-evaluator ag3) 
            '(has-cavities Susan))))




                    
    
  
  

