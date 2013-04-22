;;; Copyright (c) 2013 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns carneades.engine.triplestore
  (:require edu.ucdenver.ccp.kr.sesame.kb
            [edu.ucdenver.ccp.kr.kb :as kb]
            [edu.ucdenver.ccp.kr.rdf :as rdf]
            [edu.ucdenver.ccp.kr.sparql :as sparql]
            [carneades.engine.statement :as stmt]
            [carneades.engine.argument-generator :as generator]
            [carneades.engine.argument :as argument])
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

(defn sesame-remote-test-kb []
  (kb/open
   (edu.ucdenver.ccp.kr.sesame.kb/new-sesame-server
    :server "http://dbpedia.org/sparql"
    :repo "")))

(def test-kb (add-namespaces (sesame-remote-test-kb)))

;; (pprint (binding [sparql/*select-limit* 100] (sparql/query test-kb '((?/subject dbpedia2/starring dbpedia/Tom_Cruise) (?/subject dbpedia-owl/releaseDate ?/released)  (<= ?/released ("2002-01-01" xsd/date))))))

;; (pprint (binding [sparql/*select-limit* 50] (sparql/query test-kb '((?/x rdf/type dbpedia-owl/Philosopher)))))

;; (sparql/sparql-ask-query '(a b c))

;; (binding [sparql/*select-limit* 5] (sparql/ask test-kb '((ex/a b c))))
;; (binding [sparql/*select-limit* 5] (sparql/ask test-kb '((dbpedia/Far_and_Away dbpedia2/starring dbpedia/Tom_Cruise))))

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
  "Asks the triplestore if the goal exists. If yes a response
  containing one argument is build."
  [kbconn goal]
  (if (sparql/ask (:kb kbconn) (list goal))
    (let [arg (argument/make-argument :conclusion goal
                                      :scheme (str "triplestore:" (:host kbconn))
                                      :strict true)]
      [(generator/make-response subs [] arg)])
    []))

(defn responses-from-query
  [kbconn goal subs])

(defn responses-from-goal
  "Generates responses for a given goal."
  [kbconn goal subs]
  (if (stmt/ground? goal)
    (responses-from-ask kbconn goal)
    (responses-from-query kbconn goal subs)))

(defn generate-arguments-from-triplestore
  "Creates a generator generating arguments from facts in a triplestore.
Prefixes is a list of prefixes in the form (prefix namespace), 
for instance (\"fn:\" \"http://www.w3.org/2005/xpath-functions#\") "
  ([endpoint-url prefixes]
     (let [kbconn (make-conn endpoint-url prefixes)]
       (reify generator/ArgumentGenerator
         (generate [this goal subs]
           (responses-from-goal kbconn goal subs)))))
  ([endpoint-url]
     (generate-arguments-from-triplestore endpoint-url [])))
