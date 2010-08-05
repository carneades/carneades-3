(ns carneades.examples.maptest
  (:use carneades.engine.argument
        carneades.mapcomponent.viewer))

(defargument a1
  (pro "P"
       (pm "Q")
       (am "R")
       (ex "S")))

(defargument a2
  (con "P"
        (pm '(not "T"))
        (am '(not "U"))
        (ex '(not "V"))))

(defargument a3 
  (pro "R" 
       (pm "W")))

(defargument a4 
  (pro "V"
      (pm "X")))

(defargument a5
  (pro "Q"
       (am "Y")))

(def ag1 (argument-graph 'ag1 "argument graph test" "P"))
(def ag1 (assert-arguments ag1 (list a1 a2 a3 a4 a5)))
(def ag1 (accept ag1 (list "Q" '(not "S") "W" "X")))
(def ag1 (reject ag1 (list "T"))) ; to check that reject is same as accepting the complement
(def ag1 (question ag1 (list "R" "Y")))
(def ag1 (assoc-standard ag1 :se (list "P")))

; (define ld (make-lkif-data (lkif-data-sources kb1) (lkif-data-rulebase kb1) (list ag1)))
;; (def ld (make-lkif-data '() (rulebase) (list ag1)))
;; (lkif-export '() ld "argument-map-tests2.xml")

(view ag1)