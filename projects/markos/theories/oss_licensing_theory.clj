;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns eu.markosproject.licensing.oss-licensing-theory
  (:require  [carneades.engine.dublin-core :as dc]
             [carneades.engine.theory :as t]
             [carneades.engine.argument :as a]
             [carneades.owl.import :as owl]
             [carneades.project.admin :as project]))

(def oss-licensing-ontology
  (owl/import-from-project "markos" "ontologies/MARKOS/markos-licenses.owl"))

(def copyright-theory
  ;; require won't work here since the files are not in the CLASSPATH
  (deref (load-file (project/absolute-theory-path "markos" "copyright_theory"))))

(def oss-licensing-theory
  (t/make-theory
   :header
   (dc/make-metadata
    :title "Theory of Open Source Software Licensing"
    :creator "Tom Gordon"
    :description {:en "A theory of open source licensing for the MARKOS
project."})

   :imports [copyright-theory]

   :namespaces
   { ""   "http://www.markosproject.eu/ontologies/oss-licenses#",
     "copyright" "http://www.markosproject.eu/ontologies/copyright#",
     "owl" "http://www.w3.org/2002/07/owl#",
     "rdf" "http://www.w3.org/1999/02/22-rdr-syntax-ns#"
     "rdfs" "http://www.w3.org/2000/01/rdf-schema#"
     "xsd" "http://www.w3.org/2001/XMLSchema#"
     "foaf" "http://xmlns.com/foaf/0.1/"
     "lic" "http://www.markosproject.eu/ontologies/licenses#"
     "soft" "http://www.markosproject.eu/ontologies/software#"
     "top" "http://www.markosproject.eu/ontologies/top#"
     "dc" "http://purl.org/dc/terms/"
     "ec" "http://www.markosproject.eu/ontologies/markos-event-calculus#"}

   :language
   (into (t/make-language
          (t/make-role :symbol 'http://www.markosproject.eu/ontologies/software#linkedLibrary-mock
                       :forms {:en (t/make-form :positive "%s is linked to %s"
                                                :negative "%s is not linked to %s"
                                                :question "Is %s linked to %s?")})
          (t/make-role :symbol 'http://www.markosproject.eu/ontologies/copyright#licenseTemplate-mock
                       :forms {:en (t/make-form :positive "%s is licensed using %s"
                                                :negative "%s is not licensed using %s"
                                                :question "Is %s licensed using %s?")})
          (t/make-concept :symbol 'http://www.markosproject.eu/ontologies/oss-licenses#ReciprocalLicenseTemplate
                       :forms {:en (t/make-form :positive "%s is a reciprocal license"
                                                :negative "%s is not reciprocal license"
                                                :question "Is %s a reciprocal license?")}))
         (:language copyright-theory))

   :sections
   [(t/make-section
     :id 'ontology-axioms
     :schemes {};; (:axioms oss-licensing-ontology)
     )
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
       ;; ex:
       ;; (http://www.markosproject.eu/ontologies/copyright#mayBeLicensedUsing ?W http://www.markosproject.eu/ontologies/oss-licenses#GPL-2.0)
       :conclusion '(copyright:mayBeLicensedUsing ?W ?T))

      (t/make-scheme
       :id 'reciprocity-rule
       :header (dc/make-metadata
                :description {:en "A work W1 may not use a license
                template T1 if the work is derived from a work W2
                licensed using a reciprocal license template T2,
                unless T1 is compatible with T2."})
       :pro false
       :conclusion '(copyright:mayBeLicensedUsing ?W1 ?T1)
       :premises [(a/pm '(copyright:derivedFrom ?W1 ?W2))
                  (a/pm '(copyright:licenseTemplate-mock ?W2 ?T2))
                  (a/pm '(ReciprocalLicenseTemplate ?T2))
                  ]
       ;; :exceptions [(a/pm '(copyright:compatibleWith ?T1 ?T2))]
       )

      (t/make-scheme
       :id 'mock-license-template-rule
       :header (dc/make-metadata
                :description {:en ""})
       :conclusion '(copyright:licenseTemplate-mock ?W2 ?TPL)
       :premises [(a/pm '(lic:coveringLicense ?W2 ?L))
                  (a/pm '(lic:template ?L ?TPL))])

      (t/make-scheme
       :id 'derivedFrom-1
       :header (dc/make-metadata :description {:en "W1 is derived from W2"})
       :conclusion '(copyright:derivedFrom ?W1 ?W2)
       :premises [(a/pm '(soft:previousVersion ?W1 ?W2))])

      ;; (t/make-scheme
      ;;  :id 'modify-1
      ;;  :header (dc/metadata
      ;;           :description {:en "A version of a software entity is a
      ;;              modification of the entity, and thus, according to
      ;;              the copyright theory, a derivative work. This is an
      ;;              example of a subsumption rule mapping relations in
      ;;              the software ontology to a concept in the copyright
      ;;              ontology. " })
      ;;  :conclusion '(copyright:Modify (soft:previousVersion ?V2 ?V1))
      ;;  :premises [(a/pm '(soft:previousVersion ?V2 ?V1))])

      ;; (t/make-scheme
      ;;  :id 'workUsed-1
      ;;  :conclusion '(copyright:workUsed (soft:previousVersion ?V2 ?V1) ?V1))

      ;; (t/make-scheme
      ;;  :id 'adaptation-1
      ;;  :conclusion '(copyright:adaptation (soft:previousVersion ?V2 ?V1) ?V2))

      ;; (t/make-scheme
      ;;  :id 'adapt-1
      ;;  :header (dc/make-metadata
      ;;           :description {:en "Linking a software entity to a
      ;;              library causes the software entity to become an
      ;;              adaptation and thus a derivative work of the
      ;;              library. It may be necessary to distinguish dynamic
      ;;              and static linking. Some copyright experts claim
      ;;              that dynamic linking, unlike static linking, does
      ;;              not copyrighteate a derivative work."})
      ;;  :conclusion '(copyright:Adapt (soft:linkedLibrary ?SE ?L))
      ;;  :premises [(a/pm '(soft:linkedLibrary ?SE ?L))])

      ;; (t/make-scheme
      ;;  :id 'workUsed-2
      ;;  :conclusion '(copyright:workUsed (soft:linkedLibrary ?SE ?L) ?L))

       (t/make-scheme
        :id 'fsf-theory-of-linking
        :header (dc/make-metadata :description {:en "The Free Software
       Foundation claims that linking creates derivative works."})
        :conclusion '(copyright:derivedFrom ?W1 ?W2)
        :premises [(a/pm '(soft:linkedLibrary-mock ?W1 ?W2))])

       (t/make-scheme
        :id 'mock-linked-library
        :header (dc/make-metadata :description {:en ""})
        :conclusion '(soft:linkedLibrary-mock ?REL ?LIB)
        :premises [(a/pm '(soft:SoftwareRelease ?REL))
                   (a/pm '(top:containedEntity ?REL ?LIB))
                   (a/pm '(soft:Library ?LIB))

                   ;; pm '(lic:coveringLicense ?LIB ?LIC))
                   ;; (a/pm '(lic:template ?LIC GPL-3.0))
                   ])

       ;; (t/make-scheme
       ;;  :id 'rose-theory-of-linking
       ;;  :header (dc/make-metadata :description {:en "Lawrence Rosen
       ;;  claims that linking does not create derivate works."})
       ;;  :pro false
       ;;  :conclusion '(copyright:derivedFrom ?W1 ?W2)
       ;;  :premises [(a/pm '(soft:linkedLibrary ?W1 ?W2))])

      ])]))
