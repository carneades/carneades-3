;;; Copyright (c) 2011 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1


(ns carneades.engine.test-evaluation
  (:use [clojure.test :exclude [function?]]
        carneades.engine.statement
        carneades.engine.argument
        carneades.engine.theory
        carneades.engine.argument-graph
        carneades.engine.argument-construction
        carneades.engine.argument-evaluation
        carneades.engine.aspic
        carneades.engine.shell
        carneades.maps.lacij)
  (:require [clojure.pprint :refer [pprint]]
            [midje.sweet :refer :all :exclude [facts]]
            [clojure.tools.logging :refer [spy]]))

;; Example argument graphs to test whether arguments are being evaluated properly.
;; Henry Prakken suggested the following sources:

;; - Henry's 2010 paper in Argument and Computation
;; - Caminada & Amgoud's AIJ-2007 paper
;; - V. Lifschitz, "Benchmark problems for formal nonmonotonic
;;   reasoning," in Proceedings of the Second international Workshop
;;   on Non-monotonic Reasoning, 1989, pp. 202-219.

; Benchmark 1. The Tandem example
; Source:
; Baroni, P., Caminada, M., and Giacomin, M. An introduction to argumentation semantics.
; The Knowledge Engineering Review 00, 0 (2004), 1-24.


(fact "The tandem example works."
         (let [jw (make-statement :text {:en "John wants to ride on the tandem."})
               mw (make-statement :text {:en "Mary wants to ride on the tandem."})
               sw (make-statement :text {:en "Suzy wants to ride on the tandem."})
               jt (make-statement :text {:en "John is riding on the tandem."})
               mt (make-statement :text {:en "Mary is riding on the tandem."})
               st (make-statement :text {:en "Suzy is riding on the tandem."})
               A5 (make-argument :conclusion jt :premises [(pm jw)])
               A6 (make-argument :conclusion mt :premises [(pm mw)])
               A7 (make-argument :conclusion st :premises [(pm sw)])
               A8 (make-argument :strict true :conclusion (neg jt) :premises [(pm mt) (pm st)])
               A9 (make-argument :strict true :conclusion (neg mt) :premises [(pm jt) (pm st)])
               A10 (make-argument :strict true :conclusion (neg st) :premises [(pm mt) (pm jt)])

               tandem-graph
               (-> (make-argument-graph)
                   (enter-arguments [A5, A6, A7, A8, A9, A10])
                   (accept [jw, mw, sw]))]
           (expect (in-statements (evaluate aspic-grounded tandem-graph)) =>
                   #{(:id jw) (:id mw) (:id sw)})))

; The following examples are from:
; Prakken, H. An abstract framework for argumentation with structured arguments.
; Argument & Computation 1, (2010), 93-124.

; The bachelor example, ibid., page 9
; This example illustrates both the distinction between strict
; and defeasible rules and the problem of handling one kind of cycle
; in argument graphs.


;; The AIJ version of Carneades couldn't handle this example, because it
;; couldn't handle cycles and didn't support strict arguments.
;;
;; See pp 17-18 of ibid for a discussion of the effect of changing the weight of
;; A2 to be greater than A1.

(fact "The bachelor example works."
         (let [bachelor (make-statement :text {:en "Fred is a bachelor."})
               wears-ring (make-statement :text {:en "Fred wears a ring."})
               party-animal (make-statement :text {:en "Fred is a party animal."})
               married (make-statement :text {:en "Fred is married."})
               A1 (make-argument :id 'A1 :conclusion bachelor :premises [(pm party-animal)])
               A2 (make-argument :id 'A2 :conclusion married :premises [(pm wears-ring)])
               A3 (make-argument :id 'A3 :strict true :conclusion (neg married)  :premises [(pm bachelor)])
               A4 (make-argument :id 'A4 :strict true :conclusion (neg bachelor) :premises [(pm married)])
               bachelor-graph
               (-> (make-argument-graph)
                   (enter-arguments [A2, A1, A4, A3])
                   (accept [wears-ring, party-animal]))
               ag (evaluate aspic-grounded bachelor-graph)]
           (expect (undecided? ag (literal-atom bachelor)) => true)
           (expect (undecided? ag (literal-atom married)) => true)))

; The Frisian example, ibid., page 11

(fact "The fristian example works"
         (let [frisian (make-statement :text {:en "Wiebe is Frisian."})
               dutch (make-statement :text {:en "Wiebe is Dutch."})
               tall (make-statement :text {:en "Wiebe is Tall."})

               A5 (make-argument :strict true :conclusion dutch :premises [(pm frisian)])
               A6 (make-argument :conclusion tall :premises [(pm dutch)])
               frisian-graph
               (-> (make-argument-graph)
                   (enter-arguments [A5, A6])
                   (accept [frisian]))]

           (expect (in? (evaluate aspic-grounded frisian-graph) (literal-atom tall)) => true)))

;; The next example shows how arguments can be constructed by instantiating schemes.
;; The scheme is instantiated manually and then used to construct arguments.
;; Schemes of this type, with a conclusion which is a scheme variable ranging
;; over arbitrary literals, cannot be used effectively with a backwards
;; chaining, goal-directed strategy, since the conclusion matches every goal
;; literal.  Thus, in this example, the scheme is first instantiated in a
;; data-driven, forwards-chaining manner, to construct a custom version
;; which can be used to construct arguments via backwards chaining.  In
;; this example the scheme is fully instantiated, but this method can
;; also be used to partially instantiate schemes, so long as the predicate
;; of the conclusion of the scheme is instantiated before trying to use
;; backwards chaining to construct arguments.


(fact "The expert example works"
         (let [expert-witness-scheme
               (make-scheme
                 :name "Expert Witness Testimony"
                 :conclusion '?P
                 :premises [(make-premise :role "major" :statement '(expert ?E ?D)),
                            (make-premise :role "minor" :statement '(asserts ?E ?P))]
                 :exceptions [(make-premise
                                :role "reliable"
                                :positive false
                                :statement '(reliable-as-source ?E)),
                              (make-premise
                                :role "consistent"
                                :statement '(not (consistent-with-other-witnesses ?P)))]
                 :assumptions [(make-premise
                                 :role "credible"
                                 :statement '(credible-expert ?E)),
                               (make-premise
                                 :role "backup-evidence"
                                 :statement '(based-on-evidence ?P))])


               expert-witness1  (specialize-scheme
                                  expert-witness-scheme
                                  {'?P '(has-cavities Susan)
                                   '?E 'Joe
                                   '?D 'dentistry})

               max-goals 10

               generators
               (list (generate-arguments-from-scheme expert-witness1))

               case1-facts  '((expert Joe dentistry)
                                      (asserts Joe (has-cavities Susan)))

               query '(has-cavities Susan)

               expert-witness-graph
               (construct-arguments query max-goals case1-facts generators)]
           (expect (in? (evaluate aspic-grounded expert-witness-graph)
                        '(has-cavities Susan))
                   => true)))

; The library example, ibid., page 17

(fact "The library example works."
         (let [snores (make-statement :text {:en "The person is snoring in the library."})
               professor (make-statement :text {:en "The person is a professor."})
               misbehaves (make-statement :text {:en "The person is misbehaving."})
               access-denied (make-statement :text {:en "The person is denied access to the library."})

               A1 (make-argument :weight 0.5 :conclusion misbehaves :premises [(pm snores)])
               A2 (make-argument :weight 0.7 :conclusion access-denied :premises [(pm misbehaves)])
               A3 (make-argument :weight 0.6 :conclusion (neg access-denied) :premises [(pm professor)])
               library-graph   (-> (make-argument-graph)
                                   (enter-arguments [A1, A2, A3])
                                   (accept [snores, professor]))]
           (expect  (in? (evaluate aspic-grounded library-graph)
                         (literal-atom access-denied))
                    => true)))

; Serial self defeat example, ibid., page 18

(fact "The self defeat example works."
         (let [P  (make-statement :text {:en "Witness John says that he is unreliable."})
               Q  (make-statement :text {:en "Witness John is unreliable."})

               ; The next argument is manually assigned an id, which can be used as
               ; a constant term to refer to the argument in the undercutter, A3, below.

               A7 (make-argument :id 'A7 :conclusion Q :premises [(pm P)])

               ; The next argument illustrates how undercutters are now explicity
               ; represented in Carneades.

               A8 (make-argument
                    :id 'A8
                    :conclusion '(not (valid A7))
                    :premises [(pm Q)])

               self-defeat-graph
               (-> (make-argument-graph)
                   (enter-arguments [A7,A8])
                   (accept [P]))]
           (expect (undecided? (evaluate aspic-grounded self-defeat-graph)
                               (literal-atom Q))
                   => true)))

;; TO DO: remaining examples in Henry's article, starting with the example
;; or parallel self-defeat on page 18.

;; The next example is from "Relating Carneades with abstract argumentation via the ASPIC+ framework for
;; structured argumentation", by Bas Gijzel and Henry Prakken.  It is the example they use to illustrate
;; the inability of Carneades to handle cycles

(fact "The vacation example works."
         (let [Italy (make-statement :text {:en "Let's go to Italy."})
               Greece (make-statement :text {:en "Let's go to Greece."})

               greece-arg  (make-argument
                             :id 'greece-arg
                             :conclusion Greece)

               greece-undercutter (make-argument
                                  :id 'greece-undercutter
                                  :conclusion '(not (valid greece-arg))
                                  :premises [(pm Italy)])

               greece-rebuttal  (make-argument
                                 :id 'greece-rebuttal
                                  :strict true
                                  :conclusion (neg Greece)
                                  :premises [(pm Italy)])

               italy-arg  (make-argument
                            :id 'italy-arg
                            :conclusion Italy)

               italy-rebuttal  (make-argument
                                 :id 'italy-rebuttal
                                 :conclusion (neg Italy)
                                 :premises [(pm Greece)])

               italy-undercutter (make-argument
                                  :id 'italy-undercutter
                                  :conclusion '(not (valid italy-arg))
                                  :premises [(pm Greece)])

               vacation-graph1  (-> (make-argument-graph)
                                    ; (assume [Italy, Greece])
                                    (enter-arguments [greece-arg, greece-undercutter,
                                                      italy-arg, italy-undercutter]))

               vacation-graph2 (accept vacation-graph1 [Italy])
               g1  (evaluate aspic-grounded vacation-graph1)
               g2  (evaluate aspic-grounded vacation-graph2)]
           (expect (undecided? g1 Italy) => true)
           (expect (undecided? g1 Greece) => true)
           (expect (in? g2 Italy) => true)
           (expect (undecided? g2 Greece) => true)))

;; This example illustrates the undermining of a supporting argument.

(fact "The undermining argument example works."
         (let [P (make-statement :atom 'P)
               Q (make-statement :atom 'Q)
               R (make-statement :atom 'R)
               S (make-statement :atom 'S)

               A1 (make-argument :id 'A1 :conclusion P :premises [(pm Q)])
               ; A2 supports A1
               A2 (make-argument :id 'A2 :conclusion Q :premises [(pm R)])
               ; A3 undermines A2
               A3 (make-argument :id 'A3 :conclusion (neg R) :premises [(pm S)])

               g (evaluate
                   aspic-grounded
                   (-> (make-argument-graph)
                       (assume [R])
                       (accept [S])
                       (enter-arguments [A1, A2, A3])))]
           (expect (undecided? g P) => true)))
