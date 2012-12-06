;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.examples.abduction
  (:use carneades.engine.argument
        carneades.engine.statement
        carneades.engine.abduction))

; key list of statements
(def u (make-statement :text {:en "u"}))
(def v (make-statement :text {:en "v"}))
(def t (make-statement :text {:en "t"}))
(def w (make-statement :text {:en "w"}))
(def r (make-statement :text {:en "r"}))
(def s (make-statement :text {:en "s"}))
(def q (make-statement :text {:en "q"}))
(def p (make-statement :text {:en "p"}))

; arguments
(def a1 (make-argument 
          :id 'a1 
          :weight 0.6 
          :conclusion (neg p) 
          :premises [r]))

(def a2 (argument 
          :id 'a2 
          :weight 0.4 
          :conclusion p 
          :premises [s, q]))

(def a3 (argument 
          :id 'a3 
          :weight 0.6 
          :conclusion (neg r)
          :premises [t]))

(def a4 (argument 
          :id 'a4 
          :weight 0.5 
          :conclusion r 
          :premises [u, (neg v)]))

(def a5 (argument 
          :id 'a5 
          :weight 0.4 
          :conclusion (neg r)
          :premises [w]))

; argument graph
(def ag (-> (argument-graph)
            (accept [s w])
            (reject [q v t])
            (enter-arguments [a1 a2 a3 a4 a5])))

; assumptions
(def asm #{(neg q) (neg v) (neg t) w s})

(def in-label-p (statement-in-label ag asm p))
(def in-label-not-p (statement-in-label ag asm (neg p)))
(def out-label-p (statement-out-label ag asm p))
(def out-label-not-p (statement-out-label ag asm (neg p)))



