;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns eu.markosproject.licensing.oss-licensing-theory
  (:require  [carneades.engine.dublin-core :as dc]
             [carneades.engine.theory :as t]
             [carneades.engine.argument :as a]
             [carneades.owl.import :as owl]
             [carneades.project.admin :as project]
             [clojure.tools.logging :refer [error]]))

(def oss-licensing-ontology
  (try
    (owl/import-from-project "markos" "ontologies/MARKOS/markos-licenses.owl")
    (catch Exception e
      (error "Error loading markos-copyright.owl")
      (error "If you don't use the MARKOS project, ignore this.")
      (error "Error:" e)
      {})))

(def copyright-theory
  (project/load-theory "markos" "copyright_theory"))

(def oss-licensing-theory
  (t/make-theory
   :header
   (dc/make-metadata
    :title "Theory of Open Source Software Licensing"
    :creator "Tom Gordon"
    :description {:en "A theory of open source licensing for the MARKOS
project."})

   ;; :imports [copyright-theory]

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
          (t/make-role :symbol 'http://www.markosproject.eu/ontologies/oss-licenses#linked
                       :forms {:en (t/make-form :positive "%s is linked to %s."
                                                :negative "%s is not linked to %s."
                                                :question "Is %s linked to %s?")})
          (t/make-role :symbol 'http://www.markosproject.eu/ontologies/licenses#licenseTemplate
                       :forms {:en (t/make-form :positive "%s is licensed using %s."
                                                :negative "%s is not licensed using %s."
                                                :question "Is %s licensed using %s?")})
          (t/make-concept :symbol 'http://www.markosproject.eu/ontologies/oss-licenses#ReciprocalLicenseTemplate
                       :forms {:en (t/make-form :positive "%s is a reciprocal license."
                                                :negative "%s is not reciprocal license."
                                                :question "Is %s a reciprocal license?")})
          (t/make-role :symbol 'http://www.markosproject.eu/ontologies/licenses#coveringLicense
                          :forms {:en (t/make-form :positive "%s has a license: %s."
                                                   :negative "%s does not have a license: %s."
                                                   :question "")})
          (t/make-role :symbol 'http://www.markosproject.eu/ontologies/licenses#template
                          :forms {:en (t/make-form :positive "The template license of %s is %s."
                                                   :negative "The template license of %s is not %s."
                                                   :question "")})
          (t/make-role :symbol 'http://www.markosproject.eu/ontologies/top#containedEntity
                          :forms {:en (t/make-form :positive "%s contains %s."
                                                   :negative "%s does not contain %s."
                                                   :question "")})
          (t/make-concept :symbol 'http://www.markosproject.eu/ontologies/software#Library
                          :forms {:en (t/make-form :positive "%s is a library."
                                                   :negative "%s is not a library."
                                                   :question "")})
          (t/make-concept :symbol 'http://www.markosproject.eu/ontologies/software#SoftwareRelease
                          :forms {:en (t/make-form :positive "%s is a Software Release."
                                                   :negative "%s is not a Software Release."
                                                   :question "")})
          (t/make-concept :symbol 'http://www.markosproject.eu/ontologies/licenses#CopyrightLicenseTemplate
                          :forms {:en (t/make-form :positive "%s is a license template."
                                                   :negative "%s is not a license template."
                                                   :question "")})
          (t/make-role :symbol 'http://www.markosproject.eu/ontologies/copyright#compatibleWith
                          :forms {:en (t/make-form :positive "%s is compatible with %s."
                                                   :negative "%s is not compatible with %s."
                                                   :question "")}))
         (:language copyright-theory))

   :sections
   [;; (t/make-section
    ;;  :id 'ontology-axioms
    ;;  :header (dc/make-metadata :title "Open Source Software Ontology axioms"
    ;;                            :description {:en ""})
    ;;  :schemes {};; (:axioms oss-licensing-ontology)
    ;;  )

    (t/make-section
     :id 'license-compatibility-rules
     :header (dc/make-metadata :title "License Compatibility Rules"
                               :description {:en ""})

     :schemes
     [(t/make-scheme
       :id 'default-licensing-rule
       :weight 0.25
       :header (dc/make-metadata
                :title "Default licensing"
                :description {:en "Presumably, a work may be licensed
                   using any license template."})
       :conclusion '(copyright:mayBeLicensedUsing ?W ?T)
       :premises [(a/pm '(lic:CopyrightLicenseTemplate ?T))])

      (t/make-scheme
       :id 'reciprocity-rule
       :header (dc/make-metadata
                :title "Reciprocity"
                :description {:en "A work W1 may not use a license
                template T1 if the work is derived from a work W2
                licensed using a reciprocal license template T2,
                unless T1 is compatible with T2."})
       :pro false
       :conclusion '(copyright:mayBeLicensedUsing ?W1 ?T1)
       :premises [(a/pm '(lic:CopyrightLicenseTemplate ?T1))
                  (a/pm '(copyright:derivedFrom ?W1 ?W2))
                  (a/pm '(lic:licenseTemplate ?W2 ?T2))
                  (a/pm '(ReciprocalLicenseTemplate ?T2))
                  ]
       :exceptions [(a/pm '(copyright:compatibleWith ?T1 ?T2))]
       )

      (t/make-scheme
       :id 'compatible-reflexive-rule
       :header (dc/make-metadata
                :title "Compatible reflexive"
                :description {:en "A license template is compatible with itself."})
       :conclusion '(copyright:compatibleWith ?T1 ?T1)
       :premises [(a/pm '(lic:CopyrightLicenseTemplate ?T1))])

      ;; (t/make-scheme
      ;;  :id 'mock-license-template-rule
      ;;  :header (dc/make-metadata
      ;;           :title "License template"
      ;;           :description {:en ""})
      ;;  :conclusion '(lic:licenseTemplate-mock ?W2 ?TPL)
      ;;  :premises [(a/pm '(lic:coveringLicense ?W2 ?L))
      ;;             (a/pm '(lic:template ?L ?TPL))
      ;;             ])

      ;; (t/make-scheme
      ;;  :id 'derivedFrom-1
      ;;  :header (dc/make-metadata
      ;;           :title "Derived from"
      ;;           :description {:en "W1 is derived from W2"})
      ;;  :conclusion '(copyright:derivedFrom ?W1 ?W2)
      ;;  :premises [(a/pm '(soft:previousVersion ?W1 ?W2))])

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
        :header (dc/make-metadata
                 :title "FSF theory of linking"
                 :description {:en "The Free Software
       Foundation claims that linking creates derivative works."})
        :conclusion '(copyright:derivedFrom ?W1 ?W2)
        :premises [(a/pm '(linked ?W1 ?W2))])

       (t/make-scheme
        :id 'linked-library
        :header (dc/make-metadata
                 :title "Linked library"
                 :description {:en ""})
        :conclusion '(linked ?REL ?LIB)
        :premises [(a/pm '(soft:SoftwareRelease ?REL))
                   (a/pm '(top:containedEntity ?REL ?LIB))
                   (a/pm '(soft:Library ?LIB))])

       (t/make-scheme
        :id 'dynamically-linked-library
        :header (dc/make-metadata :title "Dynamically linked library")
        :conclusion '(linked ?REL ?LIB)
        :premises [(a/pm '(soft:dynamicallyLinkedEntity ?REL ?LIB))
                   (a/pm '(soft:Library ?LIB))])

       (t/make-scheme
        :id 'statically-linked-library
        :header (dc/make-metadata :title "Statically linked library")
        :conclusion '(linked ?REL ?LIB)
        :premises [(a/pm '(soft:staticallyLinkedEntity ?REL ?LIB))
                   (a/pm '(soft:Library ?LIB))])

       ;; (t/make-scheme
       ;;  :id 'rose-theory-of-linking
       ;;  :header (dc/make-metadata :description {:en "Lawrence Rosen
       ;;  claims that linking does not create derivate works."})
       ;;  :pro false
       ;;  :conclusion '(copyright:derivedFrom ?W1 ?W2)
       ;;  :premises [(a/pm '(soft:linkedLibrary ?W1 ?W2))])


       ;; (t/make-scheme
       ;;  :id 'compatible-software-work
       ;;  :header (dc/make-metadata :title "Compatible software work"
       ;;                            :description {:en ""})
       ;;  :conclusion '(permissibleUse (use4 ?U ?W1 ?W2C ?W2))
       ;;  :premises [(a/pm '(?W2C ?W2))
       ;;             (a/pm '(foo (use3 ?U ?W1 ?W2)))
       ;;             ])

       ;; (t/make-scheme
       ;;  :id 'foo-id
       ;;  :header (dc/make-metadata
       ;;           :title "Foo"
       ;;           :description {:en ""})
       ;;  :conclusion '(foo (use3 ?U ?W1 ?W2))
       ;;  :assumptions [(a/pm '(?U ?W1 ?W2))] ;; U is a subclass of usedSoftwareEntity
       ;;  :premises [(a/pm '(copyright:licenseTemplate-mock ?W1 ?T1))
       ;;             (a/pm '(copyright:mayBeLicensedUsing ?W1 ?T1))
       ;;             ])

      ])]))
