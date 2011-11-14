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


(def A1 (make-argument :conclusion bottom :premises [(pm jt), (pm mt), (pm st)]))   
(def A5 (make-argument :conclusion jt :premises [(pm jw)]))
(def A6 (make-argument :conclusion mt :premises [(pm mw)]))
(def A7 (make-argument :conclusion st :premises [(pm sw)]))

(def tandem-graph 
  (-> (make-argument-graph)
      (assert-arguments [A1, A5, A6, A7])
      (assume [jw, mw, sw])))

; Using the CAES evaluator, bottom is in. Consistency
; can be restored by using abduction to find minimal
; changes to the argument graph which make bottom out.
(deftest test-tandem-carneades
   (is (in? (evaluate carneades-evaluator tandem-graph) (:atom bottom))))


; The following examples are from:
; Prakken, H. An abstract framework for argumentation with structured arguments. 
; Argument & Computation 1, (2010), 93-124.

; The bachelor example, ibid., page 9
; This example illusrates both the distinction between strict 
; and defeasible rules and the problem of handling one kind of cycle
; in argument graphs.

(def bachelor (make-statement :text {:en "Fred is a bachelor."}))
(def wears-ring (make-statement :text {:en "Fred wears a ring."}))
(def party-animal (make-statement :text {:en "Fred is a party animal."}))
(def married (make-statement :text {:en "Fred is married."}))
(def A1 (make-argument :strict false :weight 0.8 :conclusion bachelor :premises [(pm party-animal)]))
(def A2 (make-argument :strict false :weight 0.7 :conclusion married :premises [(pm wears-ring)]))
(def A3 (make-argument :strict true :conclusion (neg married) :premises [(pm bachelor)]))
(def A4 (make-argument :strict true :conclusion (neg bachelor) :premises [(pm married)]))

; A5 and A6 manually add the contrapostives of A3 and A4. These could
; generated automatically when instantiating strict schemes.
(def A5 (make-argument :strict true :conclusion married :premises [(pm (neg bachelor))]))
(def A6 (make-argument :strict true :conclusion bachelor :premises [(pm (neg married))]))

(def bachelor-graph
  (-> (make-argument-graph)
      (assert-arguments [A1, A2, A3, A4, A5, A6])
      (accept [party-animal, wears-ring])))

; The AIJ version of Carneades couldn't handle this example,
; because it couldn't handle cycles and didn't support strict arguments.
; Notice how Carneades handles this example differently than ASPIC+, since
; the greater weight of A2 over A1 does not change the result here.
; See pp 17-18 of ibid for a discussion of this issue.

(deftest test-bachelor-carneades
   (let [ag (evaluate carneades-evaluator bachelor-graph)]
      (is (and (undecided? ag (:atom bachelor))
               (undecided? ag (:atom married))))))

; TO DO: maybe bachelor and married should both be undecided, 
; since out(P) should imply in(�P) and �bachelor and �married
; intuitively should not both be in. Perhaps the problem
; is underspecified, since Carneades is not strong enough to
; derive an inconsistency from �bachelor and �married. 

; The Frisian example, ibid., page 11

(def frisian (make-statement :text {:en "Wiebe is Frisian."}))
(def dutch (make-statement :text {:en "Wiebe is Dutch."}))
(def tall (make-statement :text {:en "Wiebe is Tall."}))

(def A1 (make-argument :strict true :conclusion dutch :premises [(pm frisian)]))
(def A2 (make-argument :conclusion tall :premises [(pm dutch)]))

(def frisian-graph 
  (-> (make-argument-graph)
      (assert-arguments [A1, A2])
      (accept [frisian])))

(deftest test-frisian-carneades
   (is (in? (evaluate carneades-evaluator frisian-graph) 
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
    :premises [(make-premise :role "major" :literal '(expert ?E ?D)), 
               (make-premise :role "minor" :literal '(asserts ?E ?P))]
    :exceptions [(make-premise 
                   :role "reliable" 
                   :literal '(not (reliable-as-source ?E))),
                 (make-premise 
                   :role "consistent" 
                   :literal '(not (consistent-with-other-witnesses ?P)))]
    :assumptions [(make-premise 
                    :role "credible"
                    :literal '(credible-expert ?E)),
                  (make-premise
                    :role "backup-evidence"
                    :literal '(based-on-evidence ?E))]))


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

(def expert-witness-graph
  (construct-arguments query max-goals case1-facts generators))

(deftest test-expert-witness-carneades
   (is (in? (evaluate carneades-evaluator expert-witness-graph) 
            '(has-cavities Susan))))


; The library example, ibid., page 17

(def snores (make-statement :text {:en "The person is snoring in the library."}))
(def professor (make-statement :text {:en "The person is a professor."}))
(def misbehaves (make-statement :text {:en "The person is misbehaving."}))
(def access-denied (make-statement :text {:en "The person is denied access to the library."}))

(def r1 (make-argument :weight 0.5 :conclusion misbehaves :premises [(pm snores)]))
(def r2 (make-argument :weight 0.7 :conclusion access-denied :premises [(pm misbehaves)]))
(def r3 (make-argument :weight 0.6 :conclusion '(not access-denied) :premises [(pm professor)]))

(def library-graph 
  (-> (make-argument-graph)
      (assert-arguments [r1, r2, r3])
      (accept [snores, professor])))

; Carneades applies the "last link" principle to order arguments, as can
; be seen below.

(deftest test-frisian-carneades
   (is (in? (evaluate carneades-evaluator library-graph) 
            (:atom access-denied))))

; Serial self defeat example, ibid., page 18

(def P  (make-statement :text {:en "Witness John says that he is unreliable."}))
(def Q  (make-statement :text {:en "Witness John is unreliable."}))

; The next argument is manually assigned an id, which can be used as 
; a constant term to refer to the argument in the undercutter, A3, below.

(def A2 (make-argument :id 'A2 :conclusion Q :premises [(pm P)]))

; The next argument illustrates how undercutters are now explicity 
; represented in Carneades.  

(def A3 (make-argument 
          :conclusion '(undercut A2)
          :premises [(pm Q)]))

(def self-defeat-graph 
  (-> (make-argument-graph)
      (assert-arguments [A2,A3])
      (accept [P])))

(deftest test-self-defeat
   (is (out? (evaluate carneades-evaluator self-defeat-graph) 
            (:atom Q))))

; TO DO: remaining examples in Henry's article, starting with the example
; or parallel self-defeat on page 18.


    
  
  

