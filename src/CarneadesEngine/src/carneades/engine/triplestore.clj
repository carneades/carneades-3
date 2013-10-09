;;; Copyright (c) 2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns ^{:doc "Generation of arguments from a triplestore."}
  carneades.engine.triplestore
  (:use [clojure.pprint :only [pprint write]]
        [carneades.engine.utils :only [unserialize-atom]])
  (:require [clojure.walk :as w]
            [clojure.string :as s]
            [clojure.tools.logging :refer [info debug error]]
            edu.ucdenver.ccp.kr.sesame.kb
            [edu.ucdenver.ccp.kr.kb :as kb]
            [edu.ucdenver.ccp.kr.rdf :as rdf]
            [edu.ucdenver.ccp.kr.sparql :as sparql]
            [carneades.engine.statement :as stmt]
            [carneades.engine.argument-generator :as generator]
            [carneades.engine.argument :as argument]
            [carneades.engine.unify :as unify]
            [carneades.engine.theory.namespace :as namespace])
  (:import java.net.URL))

(def select-limit 1500)

(defn- add-namespaces [kb]
  (rdf/update-namespaces kb
                         (seq {"ex" "http://www.example.org/"
                               "rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                               "rdfs" "http://www.w3.org/2000/01/rdf-schema#"
                               "owl" "http://www.w3.org/2002/07/owl#"
                               "foaf" "http://xmlns.com/foaf/0.1/"
                               "xsd" "http://www.w3.org/2001/XMLSchema#"

                               "dbpedia-owl" "http://dbpedia.org/ontology/"
                               "dbpedia" "http://dbpedia.org/resource/"
                               "dbpedia2" "http://dbpedia.org/property/"})))
;;;; scratch


(comment
  ;; Graphical Web Interface to query/manage:
  ;; http://markos.man.poznan.pl/openrdf-workbench
  (require 'edu.ucdenver.ccp.kr.sesame.kb)
  (require '[edu.ucdenver.ccp.kr.kb :as kb])
  (require '[edu.ucdenver.ccp.kr.rdf :as rdf])
  (require '[edu.ucdenver.ccp.kr.sparql :as sparql])

  (def markos-conn (make-conn "http://markos.man.poznan.pl/openrdf-sesame"
                              "markos_test_26-07-2013"
                              [["top" "http://www.markosproject.eu/ontologies/top#"]
                               ["reif" "http://www.markosproject.eu/ontologies/reification#"]
                               ["soft" "http://www.markosproject.eu/ontologies/software#"]
                               ["lic" "http://www.markosproject.eu/ontologies/licenses#"]
                               ["kb" "http://markosproject.eu/kb/"]
                               ["package" "http://markosproject.eu/kb/Package/"]
                               ["directory" "http://markosproject.eu/kb/Directory/"]
                               ["api" "http://markosproject.eu/kb/API/"]
                               ["softwareproject" "http://markosproject.eu/kb/SoftwareProject/"]
                               ["softwarerelease" "http://markosproject.eu/kb/SoftwareRelease/"]
                               ["programminglanguage" "http://markosproject.eu/kb/ProgrammingLanguage/"]]))
  ;; Compile file with C-c C-k
  ;; then execute this sexpr by placing cursor at the end of the sexp
  ;; and type C-x C-e
  ;; outputs goes in to the *nrepl* buffer
  (pprint
   (binding [sparql/*select-limit* 100]
     (sparql/query (:kb markos-conn) '((?/x soft/name ?/z)))))

  (pprint
   (binding [sparql/*select-limit* 100]
     (sparql/query (:kb markos-conn) '((?/x soft/name ("org.apache.log4j" xsd/string))))))

  (pprint
   (binding [kb/*kb* (:kb markos-conn)]
     (sparql/sparql-query-body '((?/x http://www.markosproject.eu/ontologies//software#name ("org.apache.log4j" xsd/string))))))

  (pprint
   (sparql/query (:kb markos-conn) '((?/x http://www.markosproject.eu/ontologies//software#name ("org.apache.log4j" xsd/string)))))

  (pprint
   (binding [sparql/*select-limit* 100]
     (sparql/ask (:kb markos-conn) '((soft/x soft/name soft/z)))))


  ;; SELECT ?x {
  ;;            ?x <http://www.markosproject.eu/ontologies/software#name> "org.apache.log4j"^^xsd:string
  ;;            }

  ;; SELECT ?x {
  ;;            ?x soft:name "org.apache.log4j"^^xsd:string
  ;;            }
  ;; ASK {
  ;;      ?x soft:name "org.apache.log4j"^^xsd:string
  ;;      }

  ;; ASK  {
  ;;       <http://markosproject.eu/kb/Package/3> <http://www.markosproject.eu/ontologies/software#name> "org.apache.log4j"^^xsd:string
  ;;       }
  (pprint
   (let [query (unserialize-atom "((package/_3 soft/name (\"org.apache.log4j\" xsd/string)))")]
     ;; (prn "query=" query)
     (binding [sparql/*select-limit* 100]
       (sparql/ask (:kb markos-conn) query))))


  ;; example of returned value:
  ;; {?/x http://markosproject.eu/kb/SoftwareProject/1,
  ;; ?/y soft/name,
  ;; ?/z "Apache log4j"}
  )

;; (comment
;;   (defn sesame-remote-test-kb []
;;     (kb/open
;;      (edu.ucdenver.ccp.kr.sesame.kb/new-sesame-server
;;       :server "http://dbpedia.org/sparql"
;;       :repo-name "")))
;;   (def test-kb (add-namespaces (sesame-remote-test-kb)))
;;   (pprint (binding [sparql/*select-limit* 100] (sparql/query test-kb '((?/subject dbpedia2/starring dbpedia/Tom_Cruise) (?/subject dbpedia-owl/releaseDate ?/released)  (<= ?/released ("2002-01-01" xsd/date))))))
;;   )

;; (pprint (binding [sparql/*select-limit* 50] (sparql/query-count test-kb '((?/x rdf/type dbpedia-owl/Philosopher)))))

;; (sparql/sparql-ask-query '(a b c))

;; (binding [sparql/*select-limit* 5] (sparql/ask test-kb '((ex/a b c))))
;; (binding [sparql/*select-limit* 45] (sparql/query test-kb '((dbpedia/Carneades rdf/type ?/x))))

;; (pprint (binding [sparql/*select-limit* 5] (sparql/ask test-kb '((A 'B 'C)))))

;;;;

(defn- make-sesame-conn
  [endpoint-url repo-name namespaces]
  (let [kb (kb/open
            (edu.ucdenver.ccp.kr.sesame.kb/new-sesame-server
             :server endpoint-url
             :repo-name repo-name))
        kb (add-namespaces kb)
        kb (rdf/update-namespaces kb (seq namespaces))]
    kb))

(defn make-conn
  "Creates a connection map to a Sesame SPARQL Endpoint."
  [endpoint-url repo-name namespaces]
  {:kb (make-sesame-conn endpoint-url repo-name namespaces)
   :host (.getHost (URL. endpoint-url))})

(defn variable->sparqlvariable
  "Converts a Carneades variable to Clojure/SPARQL variable.
Do nothing if v is not a variable."
  [v]
  (if (stmt/variable? v)
    (let [n (str v)]
      (symbol (str "?/" (subs n 1))))
    v))

(defn iri->owllib-iri
  [sym]
  "Converts the IRI to make itcompatible with the Clojure OWL library.
The IRI is returned with its last slash doubled."
  (if (symbol? sym)
    (symbol (s/replace (str sym) #"(.+)://(.*)/(.*)" "$1://$2//$3"))
    sym))

(defn iris->owllib-iris
  "Converts the IRIs in an atom by IRIs compatible with the Clojure OWL library."
  [atom]
  (w/postwalk iri->owllib-iri atom))

(defn variables->sparqlvariables
  "Converts the Carneades variables in a statement Clojure/SPARQL variables."
  [stmt]
  (w/postwalk variable->sparqlvariable stmt))

(defn sparqlvariable->variable
  "Converts a Clojure/SPARQL variable to a Carneades variable"
  [v]
  (if (and (symbol? v) (= (.charAt (str v) 1) \/))
    (symbol (str "?" (subs (str v) 2)))
    v))

(defn sparqlvariables->variables
  "Converts the Clojure/SPARQL variables to Carneades variables."
  [stmt]
  (w/postwalk sparqlvariable->variable stmt))

(defn string->xsd-string
  "Converts the Clojure string to a Clojure/SPARQL xsd/string."
  [s]
  (if (string? s)
    (list s 'xsd/string)
    s))

(defn strings->xsd-strings
  "Converts the Clojure strings inside sexp to Clojure/SPARQL xsd/string."
  [sexp]
  (w/postwalk string->xsd-string sexp))

(defn transform-sexp
  "Transforms an sexp in the form (p s o) into (s p o) or or an sexp
  in the form (p s) into (s rdf/type p)"
  [sexp]
  (let [[p s o & args] sexp]
    (cond (not (empty? args))
          (throw (ex-info "Invalid query" {:sexp sexp}))

          (seq? p)
          (map transform-sexp sexp)

          (nil? o)
          (list s 'rdf/type p)

          :else
          (list s p o))))

(defn envelop-sexp
  "Transforms an sexp of the form (a b c) into ((a b c))."
  [sexp]
  (if (seq? (first sexp))
    sexp
    (list sexp)))

(defn sexp->sparqlquery
  "Converts a Carneades sexpression encoding a query to a Clojure/SPARQL query."
  [sexp]
  (let [sexp (iris->owllib-iris sexp)]
    (-> sexp
        (variables->sparqlvariables ,,,)
        (strings->xsd-strings ,,,)
        (transform-sexp ,,,)
        (envelop-sexp ,,,))))

(defn responses-from-ask
  "Generates responses for a grounded goal. Asks the triplestore if
the goal exists and builds a list containing one response with an
argument if is the case."
  [kbconn goal subs]
  (let [query (sexp->sparqlquery goal)]
    ;; (prn "issuing ask= " query)
    (debug "query= " query)
    (if (sparql/ask (:kb kbconn) query)
      (do
        ;; (prn "positive answer")
        (let [arg (argument/make-argument :conclusion goal
                                          :scheme (str "triplestore:" (:host kbconn))
                                          :strict true)]
          [(generator/make-response subs [] arg)]))
      (do
        ;; (prn "negative answer")
        []))))

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

(defn to-absolute-bindings
  "Converts the values of bindings to absolute literals."
  [bindings namespaces]
  (reduce-kv (fn [bindings k v]
               (assoc bindings k (namespace/to-absolute-literal v namespaces)))
             {}
             bindings))

(defn sparqlbindings->bindings
  "Converts the Clojure/SPARQL bindings to bindings suitable for Carneades"
  [bindings]
  (reduce-kv (fn [bindings k v]
               (let [n (str v)
                     n (s/replace n #"([^/]+)/(.+)" "$1:$2")
                     s (symbol n)]
                (assoc bindings k s)))
             {}
             bindings))

(defn sparql-query
  [kbconn sexp namespaces]
  (let [query (sexp->sparqlquery sexp)
        bindings (binding [sparql/*select-limit* select-limit]
                   (sparql/query (:kb kbconn) query))
        bindings (map sparqlbindings->bindings bindings)
        ;; _ (prn "[triplestore] relative bindings size= " (count bindings))
        ;; _ (pprint bindings)
        bindings (map #(to-absolute-bindings % namespaces) bindings)]
    ;; (prn "[triplestore] absolute bindings size= " (count bindings))
    ;; (pprint bindings)
    bindings))

(defn responses-from-query
  "Generates responses from non-grounded goal. Asks the triplestore
  with the goal as a query, if some new bindings are returned we
  construct one argument for each binding."
  [kbconn goal subs namespaces]
  (let [bindings (sparql-query kbconn goal namespaces)]
    (map #(make-response-from-binding kbconn goal subs %) bindings)))

(defn responses-from-goal
  "Generates responses for a given goal."
  [kbconn goal subs namespaces]
  (try
    (if (stmt/ground? goal)
      (responses-from-ask kbconn goal subs)
      (responses-from-query kbconn goal subs namespaces))
    (catch Exception e
      (prn "Invalid query " goal)
      (prn "Error:")
      (print (.getMessage e))
      ())))

(defn generate-arguments-from-triplestore
  "Creates a generator generating arguments from facts in a triplestore.
Prefixes is a list of prefixes in the form (prefix namespace),
for instance (\"fn:\" \"http://www.w3.org/2005/xpath-functions#\") "
  ([endpoint-url repo-name namespaces]
     (let [kbconn (make-conn endpoint-url repo-name namespaces)]
       (reify generator/ArgumentGenerator
         (generate [this goal subs]
           (when (stmt/literal-pos? goal)
             (let [res (responses-from-goal kbconn goal subs namespaces)]
               ;; (prn "responses from triplestore")
               ;; (pprint res)
               res))))))
  ([endpoint-url]
     (generate-arguments-from-triplestore endpoint-url "" [])))
