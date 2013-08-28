;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns eu.markosproject.licensing.copyright-theory
  (:require  [carneades.engine.dublin-core :as dc]
             [carneades.engine.theory :as t]
             [carneades.engine.argument :as a]
             [carneades.owl.import :as owl]))


;; owl/import takes one or more pathnames or URLs of OWL files
;; and returns a record with two fields:
;; 1. owl/language: a map; and 2. owl/axioms: a sequence of schemes

(def copyright-ontology
  (owl/import-from-project "markos" "ontologies/MARKOS/markos-copyright.owl"))

(def copyright-law-theory
  (t/make-theory
   :header
   (dc/make-metadata
    :title "Theory of Copyright Law"
    :creator "Tom Gordon"
    :description {:en "An abstract, jurisdiction-independent theory of copyright law."})

   :namespaces
   { "" "http://www.markosproject.eu/ontologies/copyright#",
     "owl" "http://www.w3.org/2002/07/owl#",
     "rdf" "http://www.w3.org/1999/02/22-rdr-syntax-ns#"
     "rdfs" "http://www.w3.org/2000/01/rdf-schema#"
     "xsd" "http://www.w3.org/2001/XMLSchema#"
     "foaf" "http://xmlns.com/foaf/0.1/"
     "ml" "http://www.markosproject.eu/ontologies/licenses#"
     "ms" "http://www.markosproject.eu/ontologies/software#"
     "mt" "http://www.markosproject.eu/ontologies/top#"
     "dc" "http://purl.org/dc/terms/"
     "ec" "http://www.markosproject.eu/ontologies/markos-event-calculus#"}

   :language
   (:language copyright-ontology)

   :sections
   [(t/make-section
     :id 'ontology-axioms
     :schemes (:axioms copyright-ontology))

    (t/make-section
     :id 'basic-copyright-rules
     :header (dc/make-metadata :title "Basic Copyright Rules"
                               :description {:en ""})

     :schemes
     [(t/make-scheme
       :id 'permitted-1
       :conclusion '(Permitted ?A)
       :exceptions [(a/pm '(Prohibited ?A))])

      (t/make-scheme
       :id 'prohibited-1
       :conclusion '(Prohibited ?U)
       :premises [(a/pm '(workUsed ?U ?W))
                  (a/pm '(ProtectedWork ?W))
                  (a/pm '(exercise ?U ?R)) ;; to be defined on a case-by-case basis
                  (a/pm '(ExclusiveRight ?R))
                  (a/pm '(ec:holdsAt ?W ?T))
                  (a/pm '(ec:happens ?U ?T)) ]
       :exceptions []) ;; To do: unless one has an appropriate license or fair use or ...

      (t/make-scheme
       ;; copying exercises the reproduction right
       :id 'excerise-1
       :conclusion '(exercise ?U ReproductionRight)
       :premises [(a/pm '(Copy ?U))])

      (t/make-scheme
       ;; modifying the work is a clear case of exercising the adaptation right
       :id 'exercise-2
       :conclusion '(exercise ?U AdaptationRight)
       :premises [(a/pm '(Modify ?U))])

      (t/make-scheme
       :id 'protected-work-1
       :conclusion '(ProtectedWork ?W)
       :premises [(a/pm '(OriginalWork ?W)) ;; check these conditions
                  (a/pm '(workFixed ?F ?W))]
       :exceptions [(a/pm '(Expired ...))]) ;;  To do

      ;; (t/make-scheme
      ;;  :id 'derivedFrom-1
      ;;  :header (dc/make-metadata :description {:en "W1 is derived from W2"})
      ;;  :conclusion '(derivedFrom ?W1 ?W2)
      ;;  :premises [(a/pm '(Modify ?U)) ; inefficient, will iterate over
      ;;                                 ; all modifications
      ;;             (a/pm '(adaptation ?U ?W1))
      ;;             (a/pm '(usedWork ?U ?W2))])

      ] )]))
