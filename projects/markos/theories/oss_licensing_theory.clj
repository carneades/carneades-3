;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns eu.markosproject.licensing.oss-licensing-theory
  (:require  [carneades.engine.dublin-core :as dc]
             [carneades.engine.theory :as t]
             [carneades.engine.argument :as a]
             [carneades.owl.import :as owl]
             [eu.markosproject.licensing.copyright-theory :as c]))

(def oss-licensing-ontology
  (owl/import-from-project "markos" "ontologies/MARKOS/markos-licenses.owl"))

(def oss-licensing-theory
  (t/make-theory
   :header
   (dc/make-metadata
    :title "Theory of Open Source Software Licensing"
    :creator "Tom Gordon"
    :description {:en "A theory of open source licensing for the MARKOS
project."})

   :imports [c/copyright-law-theory] ;; to do: "imports" property of theories to be implemented

   :namespaces
   { ""   "http://www.markosproject.eu/ontologies/oss-licenses#",
     "cr" "http://www.markosproject.eu/ontologies/copyright#",
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
   (:language oss-licensing-ontology)

   :sections
   [(t/make-section
     :id 'ontology-axioms
     :schemes (:axioms oss-licensing-ontology))
    ;; Should we import the axioms from the ontology, or will the
    ;; OWL reasoning be done entirely by the repository and queried
    ;; using SPARQL?

    (t/make-section
     :id 'license-compatibility-rules
     :header (dc/make-metadata :title "License Compatibility Rules"
                               :description {:en ""})

     :schemes
     [(t/make-scheme
       :id 'default-licensing-rule
       :header (dc/make-metadata
                :description {:en "Presumably, a work may be licensed
                   using any license template."})
       :conclusion '(cr:mayBeLicensedUsing ?W ?T))

      (t/make-scheme
       :id 'reciprocity-rule
       :header (dc/make-metadata
                :description {:en "A work W1 may not use a license
                template T1 if the work is derived from a work W2
                licensed using a reciprocal license template T2,
                unless T1 is compatible with T2."})
       :pro false
       :conclusion '(cr:mayBeLicensedUsing ?W1 ?T1)
       :premises [(a/pm '(cr:derivedFrom ?W1 ?W2))
                  (a/pm '(cr:licenseTemplate ?W2 ?T2))
                  (a/pm '(ReciprocalLicenseTemplate ?T2))]
       :exceptions [(a/pm '(cr:compatibleWith ?T1 ?T2))])

      (t/make-scheme
       :id 'derivedFrom-1
       :header (dc/make-metadata :description {:en "W1 is derived from W2"})
       :conclusion '(derivedFrom ?W1 ?W2)
       :premises [(a/pm '(ms:previousVersion ?W1 ?W2))])

      ;; (t/make-scheme
      ;;  :id 'modify-1
      ;;  :header (dc/metadata
      ;;           :description {:en "A version of a software entity is a
      ;;              modification of the entity, and thus, according to
      ;;              the copyright theory, a derivative work. This is an
      ;;              example of a subsumption rule mapping relations in
      ;;              the software ontology to a concept in the copyright
      ;;              ontology. " })
      ;;  :conclusion '(cr:Modify (ms:previousVersion ?V2 ?V1))
      ;;  :premises [(a/pm '(ms:previousVersion ?V2 ?V1))])

      ;; (t/make-scheme
      ;;  :id 'workUsed-1
      ;;  :conclusion '(cr:workUsed (ms:previousVersion ?V2 ?V1) ?V1))

      ;; (t/make-scheme
      ;;  :id 'adaptation-1
      ;;  :conclusion '(cr:adaptation (ms:previousVersion ?V2 ?V1) ?V2))

      ;; (t/make-scheme
      ;;  :id 'adapt-1
      ;;  :header (dc/make-metadata
      ;;           :description {:en "Linking a software entity to a
      ;;              library causes the software entity to become an
      ;;              adaptation and thus a derivative work of the
      ;;              library. It may be necessary to distinguish dynamic
      ;;              and static linking. Some copyright experts claim
      ;;              that dynamic linking, unlike static linking, does
      ;;              not create a derivative work."})
      ;;  :conclusion '(cr:Adapt (ms:linkedLibrary ?SE ?L))
      ;;  :premises [(a/pm '(ms:linkedLibrary ?SE ?L))])

      ;; (t/make-scheme
      ;;  :id 'workUsed-2
      ;;  :conclusion '(cr:workUsed (ms:linkedLibrary ?SE ?L) ?L))

       (t/make-scheme
        :id 'fsf-theory-of-linking
        :header (dc/make-metadata :description {:en "The Free Software
       Foundation claims that linking creates derivative works."})
        :conclusion '(derivedFrom ?W1 ?W2)
        :premises [(a/pm '(ms:linkedLibrary ?W1 ?W2))])

       (t/make-scheme
        :id 'rose-theory-of-linking
        :header (dc/make-metadata :description {:en "Lawrence Rosen
        claims that linking does not create derivate works."})
        :pro false
        :conclusion '(derivedFrom ?W1 ?W2)
        :premises [(a/pm '(ms:linkedLibrary ?W1 ?W2))])

      ])]))
