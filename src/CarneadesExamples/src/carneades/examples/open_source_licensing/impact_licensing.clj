;;; Copyright © 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.examples.open-source-licensing.impact-licensing
  (:use clojure.contrib.pprint
        clojure.contrib.profile
        clojure.set
        carneades.engine.argument-builtins
	carneades.engine.argument-search
        carneades.engine.shell
        carneades.engine.rule
	carneades.engine.owl
        carneades.engine.statement
        [carneades.engine.abduction :as abd]
	carneades.engine.lkif.import
        carneades.engine.lkif.export
        carneades.ui.diagram.viewer)
    (:require [carneades.engine.argument :as arg]))

; defining pathes
(def oss-path "src/carneades/examples/open_source_licensing/oss-rules.xml")
(def prepath "src/carneades/examples/open_source_licensing/")
(def impact-path "impact-licensing.owl")

; loading ontology
(def impact-ont (load-ontology impact-path prepath))

; loading rules from lkif
(def oss-kb (:rb (lkif-import oss-path '(transitive symmetric domain range equivalent))))

; defining helper methods for handling uris
(defn c [n s] (symbol (str n s)))
(def il "http://carneades.berlios.de/impact-licensing#")
(def oss "http://carneades.berlios.de/oss-licenses#")

; do copyright licenses exist that we may use for the carneades engine?
(def goal1 (list 'exists 'LT (list (c oss "CopyrightLicenseTemplate") 'LT) (list (c oss "mayUseLicenseTemplate") (c il "CarneadesEngine") 'LT)))
;(def goal1 (list (c oss "mayUseLicenseTemplate") (c il "CarneadesEngine") (c oss "GPL_Template")))

; defining argument generators
(def basic-generators
  (list
    (generate-arguments-from-rules oss-kb '())
    (generate-arguments-from-owl impact-ont :reasoner)))

(def generators
  (cons
    (builtins basic-generators)
    basic-generators))

; argument construction function
(defn args [ag goal]
  (unite-solutions (construct-arguments goal
                       50
                       ag
                       generators
                      )))

; goal function
(defn get-con-goals [s ag]
  (let [assmptns (abd/assume-decided-statements ag)]
    (abd/statement-out-label ag assmptns s)))

(defn get-pro-goals [s ag]
  (let [assmptns (abd/assume-decided-statements ag)]
    (abd/statement-in-label ag assmptns s)))

; -------------------------------------------
; main
; -------------------------------------------

(defn main
  []

  ; accepting some facts in the initial argument graph
  (def ag0 (arg/accept arg/*empty-argument-graph*
             '((valid FSFTheoryOfLinking)
                (valid http://carneades.berlios.de/impact-licensing))))

  ; constructing arguments for initial goal
  (def ag1 (args ag0 goal1))
  (view ag1)

  ; getting con-goals
  (def con-goals (apply union (get-con-goals goal1 ag1)))
  (println "con-goals: " (count con-goals))

  ; constructing arguments for con-goals
  (def ag2 (reduce (fn [ag g]
                     (println "goal:" (statement-formatted g))
                     (args ag g))
             ag1 con-goals))
  (view ag2)

  ; getting pro-goals
  (def pro-goals (apply union (get-pro-goals goal1 ag2)))
  (println "pro-goals: " (count pro-goals))

  ; constructing arguments for pro-goals
  (def ag3 (reduce (fn [ag g]
                     (println "goal:" (statement-formatted g))
                     (args ag g))
             ag2 pro-goals))
  (view ag3)

  ; exporting results to lkif
  (lkif-export {:ag [ag1 ag2 ag3]} "C:\\Users\\stb\\Desktop\\impact-licensing.xml"))

(profile (main))
