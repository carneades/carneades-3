;;; Copyright (c) 2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Argument generator with arguments generated from a triplestore."}
  carneades.engine.triplestore
  (:require [clojure.walk :as w]
            edu.ucdenver.ccp.kr.sesame.kb
            [edu.ucdenver.ccp.kr.kb :as kb]
            [edu.ucdenver.ccp.kr.rdf :as rdf]
            [edu.ucdenver.ccp.kr.sparql :as sparql]
            [carneades.engine.statement :as stmt]
            [carneades.engine.argument-generator :as generator]
            [carneades.engine.argument :as argument]
            [carneades.engine.unify :as unify])
  (:import java.net.URL))

(defn- add-namespaces [kb]
  (rdf/update-namespaces kb
                         '(("ex" "http://www.example.org/") 
                           ("rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
                           ("rdfs" "http://www.w3.org/2000/01/rdf-schema#")
                           ("owl" "http://www.w3.org/2002/07/owl#")
                           ("foaf" "http://xmlns.com/foaf/0.1/")
                           ("xsd" "http://www.w3.org/2001/XMLSchema#")
                           
                           ("dbpedia-owl" "http://dbpedia.org/ontology/")
                           ("dbpedia" "http://dbpedia.org/resource/")
                           ("dbpedia2" "http://dbpedia.org/property/"))))
;;;; scratch

;; (defn sesame-remote-test-kb []
;;   (kb/open
;;    (edu.ucdenver.ccp.kr.sesame.kb/new-sesame-server
;;     :server "http://dbpedia.org/sparql"
;;     :repo "")))

;; (def test-kb (add-namespaces (sesame-remote-test-kb)))

;; (pprint (binding [sparql/*select-limit* 100] (sparql/query test-kb '((?/subject dbpedia2/starring dbpedia/Tom_Cruise) (?/subject dbpedia-owl/releaseDate ?/released)  (<= ?/released ("2002-01-01" xsd/date))))))

;; (pprint (binding [sparql/*select-limit* 50] (sparql/query-count test-kb '((?/x rdf/type dbpedia-owl/Philosopher)))))

;; (sparql/sparql-ask-query '(a b c))

;; (binding [sparql/*select-limit* 5] (sparql/ask test-kb '((ex/a b c))))
;; (binding [sparql/*select-limit* 45] (sparql/query test-kb '((dbpedia/Carneades rdf/type ?/x))))

;; (pprint (binding [sparql/*select-limit* 5] (sparql/ask test-kb '((A 'B 'C)))))

;;;;

(defn- make-sesame-conn
  [endpoint-url prefixes]
  (let [kb (kb/open
            (edu.ucdenver.ccp.kr.sesame.kb/new-sesame-server
             :server endpoint-url
             :repo ""))
        kb (add-namespaces kb)
        kb (rdf/update-namespaces kb prefixes)]
    kb))

(defn make-conn
  "Creates a connection map to a Sesame SPARQL Endpoint."
  [endpoint-url prefixes]
  {:kb (make-sesame-conn endpoint-url prefixes)
   :host (.getHost (URL. endpoint-url))})

(defn responses-from-ask
  "Generates responses for a grounded goal. Asks the triplestore if
the goal exists and builds a list containing one response with an
argument if is the case."
  [kbconn goal subs]
  (if (sparql/ask (:kb kbconn) [goal])
    (let [arg (argument/make-argument :conclusion goal
                                      :scheme (str "triplestore:" (:host kbconn))
                                      :strict true)]
      [(generator/make-response subs [] arg)])
    []))

(defn variable->sparqlvariable
  "Converts a Carneades variable to Clojure/SPARQL variable. 
Do nothing if v is not a variable."
  [v]
  (if (stmt/variable? v)
    (let [n (str v)]
      (symbol (str "?/" (subs n 1))))
    v))

(defn variables->sparqlvariables
  "Converts the Carneades variables in a statement Clojure/SPARQL variables."
  [stmt]
  (w/postwalk variable->sparqlvariable stmt))

(defn sparqlvariable->variable
  "Converts a Clojure/SPARQL variable to a Carneade variable"
  [v]
  (if (and (symbol? v) (= (.charAt (str v) 1) \/))
    (symbol (str "?" (subs (str v) 2)))
    v))

(defn sparqlvariables->variables
  "Converts the Clojure/SPARQL variables to Carneades variables."
  [stmt]
  (w/postwalk sparqlvariable->variable stmt))

(defn make-response-from-binding
  "Creates a response for a binding returned by the triplestore."
  [kbconn goal subs binding]
  (let [returned-subs (sparqlvariables->variables binding)
        new-subs (merge subs returned-subs)
        arg (argument/make-argument
             :conclusion (unify/apply-substitutions new-subs goal)
             :scheme (str "triplestore:" (:host kbconn))
             :strict true)]
   (generator/make-response new-subs [] arg)))

(defn responses-from-query
  "Generates responses from non-grounded goal. Asks the triplestore
  with the goal as a query, if some new bindings are returned we
  construct one argument for each binding."
  [kbconn goal subs]
  (let [query (variables->sparqlvariables goal)
        bindings (sparql/query (:kb kbconn) [query])]
    (map #(make-response-from-binding kbconn goal subs %) bindings)))

(defn responses-from-goal
  "Generates responses for a given goal."
  [kbconn goal subs]
  (if (stmt/ground? goal)
    (responses-from-ask kbconn goal subs)
    (responses-from-query kbconn goal subs)))

(defn generate-arguments-from-triplestore
  "Creates a generator generating arguments from facts in a triplestore.
Prefixes is a list of prefixes in the form (prefix namespace), 
for instance (\"fn:\" \"http://www.w3.org/2005/xpath-functions#\") "
  ([endpoint-url prefixes]
     (let [kbconn (make-conn endpoint-url prefixes)]
       (reify generator/ArgumentGenerator
         (generate [this goal subs]
           (prn "goal=" goal)
           (when (stmt/literal-pos? goal)
             (responses-from-goal kbconn goal subs))))))
  ([endpoint-url]
     (generate-arguments-from-triplestore endpoint-url [])))
