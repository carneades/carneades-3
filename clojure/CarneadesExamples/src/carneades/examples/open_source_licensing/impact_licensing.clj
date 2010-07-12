(ns carneades.examples.open-source-licensing.impact-licensing
  (:use clojure.contrib.pprint
        carneades.engine.argument-builtins
	carneades.engine.argument-search
        carneades.engine.shell
        carneades.engine.rule
	carneades.engine.owl
	carneades.engine.lkif.import
        carneades.mapcomponent.viewer)
    (:require [carneades.engine.argument :as arg]))

(def kb1 (lkif-import "src/carneades/examples/open_source_licensing/impact-kb.xml" '(transitive symmetric domain range equivalent)))
(def rb1 (:rb kb1))

(def cqs '())

(def CarneadesEngine (symbol "http://carneades.berlios.de/impact-licensing#CarneadesEngine"))
(def EPL (symbol "http://carneades.berlios.de/impact-licensing#EPL"))
(def mayUseLicenseTemplate  (symbol "http://carneades.berlios.de/oss-licenses#mayUseLicenseTemplate"))
(def implementedIn (symbol "http://carneades.berlios.de/oss-licenses#implementedIn"))
(defn c [ns n] (symbol (str ns n)))

(def il "http://carneades.berlios.de/impact-licensing#")
(def oss "http://carneades.berlios.de/oss-licenses#")

;generate all possible licenses for MyCode
;(show1 `(,mayUseLicenseTemplate ,CarneadesEngine ?x) e1)

;(def sols (construct-arguments `(not (,mayUseLicenseTemplate ,CarneadesEngine ,EPL))
;                                  5000
;                                  1
;                                  empty-argument-graph
;                                  (list (generate-arguments-from-rules rb1 cqs)
;                                        builtins)))

(def generators  (list (generate-arguments-from-rules rb1 cqs) builtins))

(def e1 (make-engine* 5000 10 arg/*empty-argument-graph* generators))

;(defn args [ag goal]
;  (unite-solutions (construct-arguments goal
;                       500
;                       4
;                       ag
;                       generators
;                      )))
;
;(defn argswc [ag goal]
;  (unite-solutions-with-candidates
;   (construct-arguments goal
;                        500
;                        4
;                        ag
;                        generators)))

(def goal1 `(~(c oss "mayUseLicenseTemplate") ~(c il "CarneadesEngine") ~(c oss "EPL_Template")))
(def goal2 `(not (~(c oss "mayUseLicenseTemplate") ~(c il "CarneadesEngine") ~(c oss "EPL_Template"))))
(def goal3 `(~(c oss "instanceOfTemplate") ~(c il "ClojureContribLicense") ~(c oss "EPL_Template")))

;(def ag1 (args arg/*empty-argument-graph* goal1))
;(def ag2 (args ag1 goal2))

(def ag1 (:arguments (first (e1 goal1))))

; (view ag2)
