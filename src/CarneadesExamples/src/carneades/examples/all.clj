
(ns carneades.examples.all
  (:use carneades.engine.argument
        carneades.engine.rule
        carneades.engine.shell
        carneades.engine.argument-builtins
        [carneades.engine.search :only (depth-first)]
        carneades.ui.diagram.viewer))

(def rb
  (rulebase

   (rule r1 (if (or (= ?x 3)
                    (= ?x 4))
                (q ?x)))

;    (assertions facts
;         (u 2 5)
;         (u 3 4)
;         (u 4 3)
;         (u 5 2))
    ))

(def type-rb
  (rulebase

    (rule t1 (if (all x (t x) (u x ?y))
               (s ?y)))

   (assertions type-facts
         (t 2)
         (t 3)
         (t 4)
         (t 5)
         (t 6)
         (u 2 5)
         (u 3 4)
         (u 4 3)
         (u 5 2))))

(def type-gens (list (generate-arguments-from-rules type-rb)))

(def goal '(all x (t x) (q x)))
;(def goal '(exists x (t x) (u x ?y)))
;(def goal '(s ?y))

(def sols (construct-arguments goal
                               200
                               1
                               *empty-argument-graph*
                               depth-first
                               (list (generate-arguments-from-rules rb)
                                     (generate-arguments-from-rules type-rb)
                                     (builtins type-gens))))

(prn "sol =")
(prn sols)

(def ag1 (unite-solutions sols))
(def ag2 (unite-solutions-with-candidates sols))

(def e1 (make-engine 200
                          0
                          *empty-argument-graph*
                          (list (generate-arguments-from-rules rb)
                                (builtins type-gens))))

(def sol2 (e1 goal))
