;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns eu.markosproject.licensing.oss-licensing-theory
  (:require  [carneades.engine.dublin-core :as dc]
             [carneades.engine.theory :as t]
             [carneades.engine.argument :as a]
             [carneades.owl.import :as owl]
             [carneades.project.fs :as project]
             [taoensso.timbre :as timbre :refer [debug error spy]]))

;; (def oss-licensing-ontology
;;   (try
;;     (owl/import-from-project "markos" "ontologies/MARKOS/markos-licenses.owl")
;;     (catch Exception e
;;       (error "Error loading markos-copyright.owl")
;;       (error "If you don't use the MARKOS project, ignore this.")
;;       (error "Error:" e)
;;       {})))

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
          (t/make-role 
           :symbol 'http://www.markosproject.eu/ontologies/oss-licenses#linked
           :forms {:en (t/make-form :positive "%s is linked to %s."
                                    :negative "%s is not linked to %s."
                                    :question "Is %s linked to %s?")})
          (t/make-role 
           :symbol 'http://www.markosproject.eu/ontologies/licenses#licenseTemplate
           :forms {:en (t/make-form :positive "%s is licensed using %s."
                                    :negative "%s is not licensed using %s."
                                    :question "Is %s licensed using %s?")})
          (t/make-concept 
           :symbol 'http://www.markosproject.eu/ontologies/oss-licenses#ReciprocalLicenseTemplate
           :forms {:en (t/make-form :positive "%s is a reciprocal license."
                                    :negative "%s is not reciprocal license."
                                    :question "Is %s a reciprocal license?")})
          (t/make-role 
           :symbol 'http://www.markosproject.eu/ontologies/licenses#coveringLicense
           :forms {:en (t/make-form :positive "%s has a license: %s."
                                    :negative "%s does not have a license: %s."
                                    :question "Does %s have %s as its license?")})
          (t/make-role 
           :symbol 'http://www.markosproject.eu/ontologies/licenses#template
           :forms {:en (t/make-form :positive "The template license of %s is %s."
                                    :negative "The template license of %s is not %s."
                                    :question "Is the template license of %s the %s?")})
          (t/make-role 
           :symbol 'http://www.markosproject.eu/ontologies/top#containedEntity
           :forms {:en (t/make-form :positive "%s contains %s."
                                    :negative "%s does not contain %s."
                                    :question "Does %s contain %s?")})
          (t/make-concept 
           :symbol 'http://www.markosproject.eu/ontologies/software#Library
           :forms {:en (t/make-form :positive "%s is a library."
                                    :negative "%s is not a library."
                                    :question "Is %s a library?")})
          (t/make-concept 
           :symbol 'http://www.markosproject.eu/ontologies/software#SoftwareRelease
           :forms {:en (t/make-form :positive "%s is a software release."
                                    :negative "%s is not a software release."
                                    :question "Is %s a software release?")})
          (t/make-concept 
           :symbol 'http://www.markosproject.eu/ontologies/licenses#CopyrightLicenseTemplate
           :forms {:en (t/make-form :positive "%s is a license."
                                    :negative "%s is not a license."
                                    :question "Is %s a license?")})
          (t/make-role 
           :symbol 'http://www.markosproject.eu/ontologies/copyright#compatibleWith
           :forms {:en (t/make-form :positive "%s is compatible with %s."
                                    :negative "%s is not compatible with %s."
                                    :question "Is %s compatible with %s?")}) 
          (t/make-role
           :symbol 'http://www.markosproject.eu/ontologies/software#provenanceRelease
           :forms {:en (t/make-form :positive "%s is a the provenance release of %s."
                                    :negative "%s is not the provenance releaes of %s."
                                    :question "Is %s the provenance release of %s?")})

          (t/make-role
           :symbol 'http://www.markosproject.eu/ontologies/software#releasedSoftware
           :forms {:en (t/make-form :positive "The project %s has released %s."
                                    :negative "The project %s has not released %s."
                                    :question "Did the project %s release %s?")})

          (t/make-role
           :symbol 'http://www.markosproject.eu/ontologies/copyright#derivedFrom
           :forms {:en (t/make-form :positive "%s is derived from %s."
                                    :negative "%s is not derived from %s."
                                    :question "Is %s derived from %s?")})

          (t/make-role
           :symbol 'http://www.markosproject.eu/ontologies/software#usedSoftwareEntity
           :forms {:en (t/make-form :positive "%s uses %s."
                                    :negative "%s does not use %s."
                                    :question "Does %s use %s?")})

          (t/make-role
           :symbol 'http://www.markosproject.eu/ontologies/software#dynamicallyLinkedEntity
           :forms {:en (t/make-form :positive "%s is dynamically linked to %s."
                                    :negative "%s is not dynamically linked to %s."
                                    :question "Is %s dynamically linked to  %s?")})

          (t/make-role
           :symbol 'http://www.markosproject.eu/ontologies/software#staticallyLinkedEntity
           :forms {:en (t/make-form :positive "%s is statically linked to %s."
                                    :negative "%s is not statically linked to %s."
                                    :question "Is %s statically linked to  %s?")})

          (t/make-role
           :symbol 'http://www.markosproject.eu/ontologies/software#implementingLanguage
           :forms {:en (t/make-form :positive "%s is implemented in the programming language %s."
                                    :negative "%s is not implemented in the programming language %s."
                                    :question "Is %s implemented in the programming langauge %s?")})

          (t/make-role
           :symbol 'http://www.markosproject.eu/ontologies/software#usedCompiler
           :forms {:en (t/make-form :positive "%s was compiled using %s."
                                    :negative "%s was not compiled using %s."
                                    :question "Was %s compiled using %s?")})

         (t/make-role
           :symbol 'http://www.markosproject.eu/ontologies/software#implementedAPI
           :forms {:en (t/make-form :positive "%s is an implementation of the %s API."
                                    :negative "%s is not an implementation of the %s API."
                                    :question "Is %s an implementation of the %s API?")})

          ) ; end of make-language

          
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
     [ 
      (t/make-scheme
       :id 'default-licensing-rule
       :weight 0.25
       :header (dc/make-metadata
                :title "Default licensing"
                :description {:en "Presumably, a work may be licensed
                   using any license template."})
       :conclusion '(copyright:mayBeLicensedUsing ?W ?T)
       :premises [;; (a/pm '(lic:CopyrightLicenseTemplate ?T))
                  ])

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
       :premises [ ;; (a/pm '(lic:CopyrightLicenseTemplate ?T1))
                  (a/pm '(copyright:derivedFrom ?W1 ?W2))
                 ; (a/pm '(lic:licenseTemplate ?W2 ?T2))
                                        ; (a/pm '(ReciprocalLicenseTemplate ?T2))
                  ]
      ;;  :exceptions [(a/pm '(copyright:compatibleWith ?T1 ?T2))]
       )

      
      (t/make-scheme
       :id 'protected-software
       :header (dc/make-metadata
                :title "Copyright protection of software"
                :description {:en "Software is work protected by copyright."})
       :conclusion '(copyright:ProtectedWork ?SE)
       :premises [(a/pm '(soft:SoftwareEntity ?SE))])

      (t/make-scheme
       :id 'use-by-derivation
       :conclusion '(copyright:workUsed (derivation ?W1 ?W2) ?w2)
       :premises [(a/pm '(soft:usedSoftwareEntity ?W1 ?W2))])

      (t/make-scheme
       :id 'software-entity-license-template-rule
       :header (dc/make-metadata
                :title "Software Entity License Template"
                :description {:en "Presumably, the license template
                applied to a software entity is the same as the license
                template applied to the project of the provenance
                release of the entity."})
       :conclusion '(lic:licenseTemplate ?SE ?T)
       :premises [(a/pm '(soft:provenanceRelease ?SE ?R))
                  (a/pm '(soft:releasedSoftware ?P ?R))
                  (a/pm '(lic:licenseTemplate ?P ?T))])

      (t/make-scheme
       :id 'software-release-license-template-rule
       :header (dc/make-metadata
                :title "Software Release License Template"
                :description {:en "Presumably, the license template
                applied to a software release is the same as the
                project of the release."})
       :conclusion '(lic:licenseTemplate ?R ?T)
       :premises [(a/pm '(soft:releasedSoftware ?P ?R))
                  (a/pm '(lic:licenseTemplate ?P ?T))])

      
      (t/make-scheme
       :id 'default-derivative-software-rule
       :weight 0.25
       :header (dc/make-metadata
                :title "Derivative Software Works"
                :description {:en "As a general rule, any use of a
                software entity, SE1, by another software entity, S2,
                causes S2 to be a derivative work of S1."})
       :conclusion '(copyright:derivedFrom ?SE2 ?SE1)
       :premises [(a/pm '(soft:usedSoftwareEntity ?SE2 ?SE1))])

      (t/make-scheme
       :id 'compatible-reflexive-rule
       :header (dc/make-metadata
                :title "Reflexivity of compatible"
                :description {:en "A license template is compatible
                with itself."})
       :conclusion '(copyright:compatibleWith ?T1 ?T1)
       :premises [(a/pm '(lic:CopyrightLicenseTemplate ?T1))])


      (t/make-scheme
       :id 'fsf-theory-of-linking
       :header (dc/make-metadata
                :title "FSF theory of linking"
                :description {:en "The Free Software Foundation claims
                that linking creates derivative works."})
       :conclusion '(copyright:derivedFrom ?W1 ?W2)
       :premises [(a/pm '(linked ?W1 ?W2))])

      (t/make-scheme
       :id 'rosen-theory-of-linking
       :header (dc/make-metadata 
                :title "Lawrence Rosen theory of linking"
                :description {:en "Lawrence Rosen claims that linking
                does not create derivate works."})
       :pro false
       :conclusion '(copyright:derivedFrom ?W1 ?W2)
       :premises [(a/pm '(linked ?W1 ?W2))])

      ;; (t/make-scheme
      ;;  :id 'linked-library-rule
      ;;  :header (dc/make-metadata 
      ;;           :title "Linking of libraries contained in software releases"
      ;;           :description {:en "Presumably libraries contained in a
      ;;           software release are linked to by the released
      ;;           software."})
      ;;  :conclusion '(linked ?REL ?LIB)
      ;;  :premises [(a/pm '(soft:SoftwareRelease ?REL))
      ;;             (a/pm '(top:containedEntity ?REL ?LIB))
      ;;             (a/pm '(soft:Library ?LIB))])

     (t/make-scheme
       :id 'contained-entity-rule
       :weight 0.25
       :header (dc/make-metadata 
                :title "Derivation from contained software entities"
                :description {:en "Presumably a container uses the
                software entities it contains."})
       :conclusion '(soft:usedSoftwareEntity ?C ?E)
       :premises [(a/pm '(top:containedEntity ?C ?E))])

     (t/make-scheme
       :id 'dynamically-linked-library-rule
       :header (dc/make-metadata 
                :title "Dynamic linking"
                :description {:en "Dynamic linking is a form of
                linking."})
       :conclusion '(linked ?REL ?LIB)
       :premises [(a/pm '(soft:dynamicallyLinkedEntity ?REL ?LIB))])

      (t/make-scheme
       :id 'statically-linked-library-rule
       :header (dc/make-metadata 
                :title "Static linking"
                :description {:en "Static linking is a form of
                linking"})
       :conclusion '(linked ?REL ?LIB)
       :premises [(a/pm '(soft:staticallyLinkedEntity ?REL ?LIB))])


      (t/make-scheme
       :id 'implementing-language-rule
       :header (dc/make-metadata 
                :title "Derivation from Implementation Language"
                :description {:en "Software is not a derivative work
                of the programming language used to implement the
                software."})
       :pro false
       :conclusion '(copyright:derivedFrom ?W1 ?W2)
       :premises [(a/pm '(soft:implementingLanguage ?W1 ?W2))])

      (t/make-scheme
       :id 'compiler-rule
       :header (dc/make-metadata 
                :title "Derivation from Compiler"
                :description {:en "Object code is not a derivative
                work of the complier used to create it."})
       :pro false
       :conclusion '(copyright:derivedFrom ?W1 ?W2)
       :premises [(a/pm '(soft:usedCompiler ?W1 ?W2))])

      (t/make-scheme
       :id 'sas-v-world-programming
       :pro false
       :header (dc/make-metadata 
                :title "SAS Institute v. World Programming"
                :description {:en "European Court of Justice (ECJ) in
                SAS Institute Inc. v World Programming Ltd., [2013]
                EWHC 69 (Ch)."})
       :conclusion '(copyright:derivedFrom ?W1 ?W2)
       :premises [(a/pm '(soft:implementedAPI ?W1 ?W2))])

      (t/make-scheme
       :id 'oracle-v-google
       :header (dc/make-metadata 
                :title "Oracle v. Google"
                :description {:en "Oracle America, Inc. v. Google,
                Inc., United States Court of Appeals for the Federal
                Circuit, 2013-1021, -1022, May 9, 2014"})
       :pro true
       :conclusion '(copyright:derivedFrom ?W1 ?W2)
       :premises [(a/pm '(soft:implementedAPI ?W1 ?W2))])
      
      ])]))
