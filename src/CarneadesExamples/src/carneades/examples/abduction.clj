
(ns carneades.examples.abduction
  (:use carneades.engine.argument
        carneades.engine.statement
        carneades.engine.abduction))


; key list of statements
(def u "u")
(def v "v")
(def t "t")
(def w "w")
(def r "r")
(def s "s")
(def q "q")
(def p "p")

; arguments
(def a1 (argument :a1 false 0.6 :con p [(pm r)] nil))
(def a2 (argument :a2 false 0.4 :pro p [(pm s) (pm q)] nil))
(def a3 (argument :a3 false 0.6 :con r [(pm t)] nil))
(def a4 (argument :a4 false 0.5 :pro r [(pm u) (pm (not v))] nil))
(def a5 (argument :a5 false 0.4 :con r [(pm w)] nil))

; argument graph
(def ag (assoc-standard (assert-arguments
                          (reject
                            (accept *empty-argument-graph* [s w])
                            [q v t])
                          [a1 a2 a3 a4 a5])
          :pe
          [u v t w r s q p]))

; assumptions
(def asm #{(statement-complement q)
           (statement-complement v)
           (statement-complement t)
           w s})

(def in-label-p (statement-in-label ag asm p))
(def in-label-not-p (statement-in-label ag asm (statement-complement p)))
(def out-label-p (statement-out-label ag asm p))
(def out-label-not-p (statement-out-label ag asm (statement-complement p)))