;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1


(ns carneades.engine.test-rule
  (:use clojure.test
        carneades.engine.rule))

(deftest test-condition-statement
  (is (= '(x y z) (condition-statement '(assuming (x y z)))))
  (is (= '(x y z) (condition-statement '(unless (x y z)))))
  (is (= '(x y z) (condition-statement '(x y z)))))

(deftest test-predicate
  (is (= 'mother (predicate '(unless (mother Tom Garcia))))))

(deftest test-rule
  (is (= {:id 'r1,
          :strict false,
          :domains '(),
          :head '((goods ?c)),
          :body '(((movable ?c) (unless (money ?c))))}
         (rule r1 (if (and (movable ?c)
                           (unless (money ?c)))
                    (goods ?c)))))
  (is (= {:id 'r3, :strict false, :domains '(), :head '((money ?x)), :body '(((coins ?x)))}
         (rule r3 (if (coins ?x) (money ?x)))))
  (is (= {:id 'r4,
          :strict false,
          :domains '(),
          :head '((light ?x) (shipable ?x)),
          :body '(((movable ?x)))}
         (rule r4
               (if (movable ?x)
                 (and (light ?x)
                      (shipable ?x))))))
  (is (= {:id 'r5,
          :strict false,
          :domains '(),
          :head '((convenient ?x)),
          :body '(((light ?x) (shipable ?x)))}
         (rule r5
               (if (and (light ?x)
                        (shipable ?x))
                 (convenient ?x)))))
  (is (= {:id 'r6,
          :strict false,
          :domains '(),
          :head '((not (goods ?x))),
          :body '(((edible ?x)))}
         (rule r6 
               (if (edible ?x) 
                 (not (goods ?x)))))))
