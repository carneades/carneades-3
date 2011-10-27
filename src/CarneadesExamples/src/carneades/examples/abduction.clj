
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
(def a1 (make-argument 
          :id 'a1 
          :weight 0.6 
          :conclusion (¬ p) 
          :premises [r]))

(def a2 (argument 
          :id 'a2 
          :weight 0.4 
          :conclusion p 
          :premises [s, q]))

(def a3 (argument 
          :id 'a3 
          :weight 0.6 
          :conclusion (¬ r)
          :premises [t]))

(def a4 (argument 
          :id 'a4 
          :weight 0.5 
          :conclusion r 
          :premises [u, (¬ v)]))

(def a5 (argument 
          :id 'a5 
          :weight 0.4 
          :conclusion (¬ r)
          :premises [w]))

; argument graph
(def ag (-> (argument-graph)
            (accept [s w])
            (reject [q v t])
            (assert-arguments [a1 a2 a3 a4 a5])))

; assumptions
(def asm #{(¬ q) (¬ v) (¬ t) w s})

(def in-label-p (statement-in-label ag asm p))
(def in-label-not-p (statement-in-label ag asm (¬ p)))
(def out-label-p (statement-out-label ag asm p))
(def out-label-not-p (statement-out-label ag asm (¬ p)))



