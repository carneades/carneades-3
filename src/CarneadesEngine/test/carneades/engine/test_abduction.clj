;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1


(ns carneades.engine.test-abduction
  (:use clojure.test
        clojure.contrib.pprint
        clojure.contrib.trace
        carneades.engine.statement
        carneades.engine.argument
        carneades.engine.abduction))

(deftest test-in-label
  (let [u "u"
        v "v"
        t "t"
        w "w"
        r "r"
        s "s"
        q "q"
        p "p"
        a4 (make-arg :a4 (pro r
                              (pm u)
                              (ex v)))
        a3 (argument :a3 false 0.6 :con r [(pm t)] nil)
        a5 (argument :a5 false 0.4 :con r [(pm w)] nil)
        a1 (argument :a1 false 0.6 :con p [(pm r)] nil)
        a2 (argument :a2 false 0.4 :pro p [(pm s) (pm q)] nil)
        ag (assoc-standard (assert-arguments
                            (reject
                             (accept *empty-argument-graph* [s w])
                             [q v t])
                            [a1 a2 a3 a4 a5])
                           :ba
                           [u v t w r s q p])
        asm #{(statement-complement q)
              (statement-complement v)
              (statement-complement t)
              w s}
        in-label-p (statement-in-label ag asm p)
        in-label-not-p (statement-in-label ag asm (statement-complement p))
        out-label-p (statement-out-label ag asm p)
        out-label-not-p (statement-out-label ag asm (statement-complement p))]
    ;; (dotrace [combine-conjunction-of-dnf
    ;;           statement-in-label
    ;;           ba-in-label]
    ;;          (statement-in-label ag asm p))
    (is (= (count in-label-p) 2))
    (is (or (and (= (first in-label-p) #{p})
                 (= (second in-label-p) #{q}))
            (and (= (first in-label-p) #{q})
                 (= (second in-label-p) #{p}))))

    (is (= (count in-label-not-p)) 3)
    (is (contains? in-label-not-p #{(statement-complement p)}))
    (is (contains? in-label-not-p #{r}))
    (is (contains? in-label-not-p #{u}))

    (is (= (count out-label-p) 1))
    (is (= out-label-p *verum*))

    (is (= (count out-label-not-p) 1))
    (is (= out-label-not-p *verum*))

    ;; (let [x (statement-in-label ag asm p)]
    ;;   (printf "=============\n")
    ;;   (pprint x)
    ;;   (printf "=============\n"))
    ))

