;;; Copyright (c) 2012 Fraunhofer Gesellschaft
;;; Licensed under the EUPL V.1.1

(ns eu.markosproject.licensing.oss-licensing-theory-interactive
  (:require  [carneades.engine.dublin-core :as dc]
             [carneades.engine.theory :as t]
             [carneades.engine.argument :as a]
             [carneades.owl.import :as owl]
             [carneades.project.admin :as project]))

(def oss-licensing-theory
  (project/load-theory "markos" "oss_licensing_theory"))

(def oss-licensing-theory-interactive
  (t/make-theory
   :header
   (dc/make-metadata
    :title "Theory of Open Source Software Licensing"
    :creator "Tom Gordon"
    :description {:en "A theory of open source licensing for the MARKOS
project to be used in an interactive dialog with the user"})

   :imports [oss-licensing-theory]

   :namespaces (assoc (:namespaces oss-licensing-theory) "interactive" "stuff")

   :language
   (into (t/make-language
          ;;;; interactive part ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (t/make-individual :symbol 'yes :text {:en "Yes"
                                               :de "Ja"
                                               :fr "Oui"})
          (t/make-individual :symbol 'no :text {:en "No"
                                              :de "Nein"
                                              :fr "Non"})
          (t/make-individual :symbol 'maybe :text {:en "Maybe"
                                                 :de "Vielleicht"
                                                 :fr "Peut-être"})
          (t/make-individual :symbol 'linking
                             :text {:en "Linking"})
          (t/make-concept :symbol 'http://www.markosproject.eu/ontologies/oss-licenses#userAgreesFsfTheory
                          :forms {:en (t/make-form :positive "It is sensible to say that linking creates a derivative work."
                                                   :negative "It is not sensible to say that linking creates a derivative work."
                                                   :question "Is it sensible to say that linking creates a derivative work?")}
                          :category 'linking
                          :askable true
                          :type 'string)
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          )
         (:language oss-licensing-theory))

   :sections
   [(t/make-section
    :id 'license-compatibility-rules
    :header (dc/make-metadata :title "License Compatibility Rules"
                              :description {:en ""})
    :schemes
    [(t/make-scheme
      :id 'user-fsf-theory-of-linking
      :weight 0.6
      :header (dc/make-metadata :description {:en "The opinion of the user on the the Free Software
       Foundation claim that linking creates derivative works."})
      :conclusion '(copyright:derivedFrom ?W1 ?W2)
      :premises [(a/pm '(linked ?W1 ?W2))
                 (a/pm '(userAgreesFsfTheory))])

     (t/make-scheme
      :id 'user-fsf-theory-of-linking-not
      :weight 0.6
      :pro false
      :header (dc/make-metadata :description {:en "The opinion of the user on the the Free Software
       Foundation claim that linking creates derivative works."})
      :conclusion '(copyright:derivedFrom ?W1 ?W2)
      :premises [(a/pm '(linked ?W1 ?W2))
                 (a/pm '(not (userAgreesFsfTheory)))])])]))
