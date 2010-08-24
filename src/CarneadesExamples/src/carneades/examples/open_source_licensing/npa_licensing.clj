;;; Copyright © 2010 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.examples.open-source-licensing.impact-licensing
  (:use clojure.contrib.pprint
        clojure.set
        carneades.engine.argument-builtins
	carneades.engine.argument-search
        carneades.engine.shell
        carneades.engine.rule
        carneades.engine.statement
	;carneades.engine.owl
        carneades.engine.pellet
        [carneades.engine.abduction :as abd]
	carneades.engine.lkif.import
        carneades.ui.diagram.viewer)
    (:require [carneades.engine.argument :as arg]))

(def oss-path "src/carneades/examples/open_source_licensing/npa-oss-rules.xml")
(def npa-prepath "src/carneades/examples/open_source_licensing/")
(def npa-path "npa-licensing.owl")

(def oss-kb (:rb (lkif-import oss-path '(transitive symmetric domain range equivalent))))

(def cqs '())

(defn c [n s] (symbol (str n s)))

(def npa "http://www.carneades.berlios.de/npa-licensing#")
(def oss "http://carneades.berlios.de/oss-licenses#")

;generate all possible licenses for MyCode
;(show1 `(,mayUseLicenseTemplate ,CarneadesEngine ?x) e1)


(def goal1 (list 'exists 'LT (list (c oss "CopyrightLicenseTemplate") 'LT) (list (c oss "mayUseLicenseTemplate") (c npa "NPA_NET") 'LT)))
;(def goal2 (list 'not goal1))
;(def goal1 `(~(c oss "mayUseLicenseTemplate") ~(c npa "NPA_NET") ~(c oss "EPL_Template")))

(def basic-generators
  (list
    (generate-arguments-from-rules oss-kb cqs)
    (generate-arguments-from-owl npa-path npa-prepath)))

(def generators  
  (cons
    (builtins basic-generators)
    basic-generators))

(def sols (construct-arguments
            goal1
            500
            1
            arg/*empty-argument-graph*
            generators))

(def ag0 (arg/accept arg/*empty-argument-graph*
           '((valid FSFTheoryOfLinking)
             (valid npa-licensing.owl))))

(def e1 (make-engine* 5000 1 ag0 generators))

(defn args [ag goal]
  (unite-solutions (construct-arguments goal
                       50
                       1
                       ag
                       generators
                      )))

(defn argswc [ag goal]
  (unite-solutions-with-candidates
   (construct-arguments goal
                        50
                        1
                        ag
                        generators)))

(defn get-con-goals [s ag]
  (let [assmptns (abd/assume-decided-statements ag)]
    (abd/statement-out-label ag assmptns s)))

(defn get-pro-goals [s ag]
  (let [assmptns (abd/assume-decided-statements ag)]
    (abd/statement-in-label ag assmptns s)))


(def ag1 (args ag0 goal1))
(view ag1)
(def con-goals (apply union (get-con-goals goal1 ag1)))
(println "con-goals: " con-goals)

(def ag2 (reduce (fn [ag g]
                   (println "goal:" (statement-formatted g))
                   (argswc ag g))
           ag1 con-goals))
(view ag2)
(def pro-goals (apply union (get-pro-goals goal1 ag2)))
(println "pro-goals: " pro-goals)

(def ag3 (reduce (fn [ag g]
                   (println "goal:" (statement-formatted g))
                   (argswc ag g))
           ag2 pro-goals))
(view ag3)