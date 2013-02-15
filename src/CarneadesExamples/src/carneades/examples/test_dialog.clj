;;; Copyright ? 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.examples.test-dialog
  (:use carneades.engine.dublin-core
        carneades.engine.scheme
        carneades.engine.argument))

(def test-dialog
  (make-theory
   :header 
   (make-metadata :title "Test for the dialog"
                  :description {:en ""}) ;; TODO add a description
   
   :language
   (make-language
    (make-individual :symbol 'yes :text {:en "Yes"
                                         :de "Ja"
                                         :fr "Oui"})
    (make-individual :symbol 'no :text {:en "No"
                                        :de "Nein"
                                        :fr "Non"})
    (make-individual :symbol 'maybe :text {:en "Maybe"
                                           :de "Vielleicht"
                                           :fr "Peut-être"})
    (make-individual :symbol 'the-person :text {:en "the person"
                                                :de "die Person"
                                                :fr "la personne"})
    (make-individual :symbol 'the-work :text {:en "the work"
                                              :de "das Werk"
                                              :fr "l'œuvre"})
    (make-individual :symbol 'commercial :text {:en "commercial"
                                                :fr "commercial"})
    (make-individual :symbol 'purpose :text {:en "Purpose"
                                             :fr "Objet"})
    (make-individual :symbol 'identifiers :text {:en "Identifiers"
                                                 :fr "Identifiants"})
    (make-individual :symbol 'non-commercial :text {:en "non-commercial"
                                                    :fr "non-commercial"})
    (make-individual :symbol 'none :text {:en "none"
                                          :fr "aucune"})
    (make-individual :symbol 'professional :text {:en "professional documented search"
                                                  :fr "recherche professionnelle documentée"})
    (make-individual :symbol 'search :text {:en "Search"
                                            :fr "Recherche"})
    (make-individual :symbol 'standard :text {:en "standard documented search"
                                              :fr "recherche standard documentée"})
    (make-individual :symbol 'license :text {:en "License"
                                             :fr "Licence"})

    (make-role
     :category 'license
     :symbol 'license-to-publish
     :min 0
     :max nil
     :type :symbol
     :askable true
     :forms {:en (make-form :positive "%s has a license to publish %s."
                            :negative "%s does not have a license to publish %s."
                            :question "Does %s have a license to publish %s?")
             :fr (make-form :positive "%s a une licence pour publier %s"
                            :negative "%s n'a pas de license pour publier %s"
                            :question "%s a-t-elle une licence pour publier %s?")}
     :hint {:en "Information about an existing license."
            :fr "Information sur une licence existante."})
    
    ;; (make-role
    ;;  :symbol 'may-publish
    ;;  :min 0
    ;;  :max nil
    ;;  :type :symbol
    ;;  :askable false
    ;;  :forms {:en (make-form :positive "%s may publish %s."
    ;;                         :negative "%s may not publish %s."
    ;;                         :question "May %s publish %s?")
    ;;          :fr (make-form :positive "%s peut publier %s."
    ;;                         :negative "%s ne peut pas publier %s."
    ;;                         :question "%s peut-elle publier %s?")})

    ;; (make-concept
    ;;  :symbol 'person
    ;;  :askable true
    ;;  :forms {:en (make-form :positive "%s is a person."
    ;;                         :negative "%s is not a person."
    ;;                         :question "Is %s a person?")
             
    ;;          :de (make-form :positive "%s ist ein Rechtsperson."
    ;;                         :negative "%s ist nicht ein Rechtsperson."
    ;;                         :question "Ist %s ein Rechtsperson?")

    ;;          :fr (make-form :positive "%s est une personne."
    ;;                         :negative "%s n'est pas une personne."
    ;;                         :question "Est-ce que %s est une personne?")}
    ;;  :category 'identifiers
    ;;  :hint {:en "Please provide an identifier for the person interested in publishing the work, such as P1."})

    ;; (make-concept
    ;;  :symbol 'work
    ;;  :askable true
    ;;  :forms {:de (make-form :positive "%s ist ein Werk."
    ;;                         :negative "%s is nicht ein Werk."
    ;;                         :question "Ist %s ein Werk?")
    ;;          :en (make-form :positive "%s is a work."
    ;;                         :negative "%s is not a work."
    ;;                         :question "Is %s a work?")
    ;;          :fr (make-form :positive "%s est une œuvre."
    ;;                         :negative "%s n'est pas une œuvre."
    ;;                         :question "Est-ce que %s est une œuvre?")}
    ;;  :hint {:en "Please provide an identifier for the orphaned work, such as W1."}
    ;;  :category 'identifiers)
    
    ;; (make-role
    ;;  :symbol 'type-of-use
    ;;  :askable true
    ;;  :min 1
    ;;  :max 2
    ;;  :type '#{non-commercial commercial}
    ;;  :forms {:de (make-form :positive "%s ist für folgender Zwecken: %s."
    ;;                         :negative "%s ist nicht für folgender Zwecken: %s."
    ;;                         :question "Ist %s für folgender Zwecken: %s?")
    ;;          :en (make-form :positive "%s is for %s purposes."
    ;;                         :negative "%s is not for %s purposes."
    ;;                         :question "Is %s for %s purposes?")
    ;;          :fr (make-form :positive "%s est pour un usage %s."
    ;;                         :negative "%s n'est pas pour un usage %s."
    ;;                         :question "%s est-il pour un usage %s?")}
    ;;  :hint {:en "Will the work be used for commercial or non-commercial purposes?"
    ;;         :fr "Est-ce que l'œuvre sera utilisée à des fins commerciales ou non-commerciales?"}
    ;;  :category 'purpose)

    ;; (make-role
    ;;  :symbol 'search-type
    ;;  :askable true
    ;;  :min 1
    ;;  :max 1
    ;;  :type '#{standard professional none}
    ;;  :forms {:en (make-form :positive "The type of %s was a %s."
    ;;                         :negative "The type of %s was not a %s."
    ;;                         :question "Was the type of %s a %s?")
    ;;          :fr (make-form :positive "Le type de %s était une %s."
    ;;                         :negative "Le type de %s n'était pas une %s."
    ;;                         :question "Est-ce que le type de %s était une %s")}
    ;;  :hint {:en "What type of search was performed to try to find the copyright owner?"
    ;;         :fr "Quel type de recherche a été effectuée pour trouver le propriétaire des droits d'auteur?"}
    ;;  :category 'search
    ;;  :next ['announcement])

    (make-concept
     :symbol 'announcement
     :askable true
     :hint {:en "Information about an announcement."
            :fr "Information sur une déclaration"}
     :forms {:en (make-form :positive "%s was publically announced."
                            :negative "%s was not publically announced."
                            :question "Was %s publically announced?")
             :fr (make-form :positive "%s a été publiquement déclarée."
                            :negative "%s n'a pas été publiquement déclarée."
                            :question "Est-ce que %s a été publiquement déclarée?")}
     :category 'search)
    
    (make-concept
     :symbol 'valid
     :askable false
     :form {:en (make-form :positive "%s is valid law."
                           :negative "%s is not valid law."
                           :question "Is %s valid law?")
            :de (make-form :positive "%s is gültiges Recht."
                           :negative "%s ist nicht gültiges Recht."
                           :question "Ist %s gültiges Recht?")}))

   :sections
   [(make-section
     :id 'Q12
     :main-issue '(may-publish the-person the-work)
     :header (make-metadata :title "Q12. Cross-Border Aspects of Orphaned Works"
                            :description {:en ""})
     
     ;; one section below for each policy proposed for this issue as well as for current polices on this issue.
     :sections  
     [(make-section
       :id 'Q12-Aktionsbundnis
       :header (make-metadata :title "Orphaned Works Policy Proposed by the Aktionsbündnisses ‟Urheberrecht für Bildung und Wissenschaft”"
                              :description {:en ""})
       :schemes
       [(make-scheme                            
         :id 'AB-52c-1-a
         :header (make-metadata :title "§ 52c (1) (a)"
                                :description {:de ""})
         :conclusion '(may-publish ?P ?W)
         :premises [(make-premise :statement '(license-to-publish John ?w))
                    ;; (make-premise :statement '(search-type (the-search ?P ?W) standard))
                    ;; (make-premise :statement '(valid AB-52c-1-a))
                    ])
        
        ;; (make-scheme
        ;;  :id 'AB-52c-2-a
        ;;  :header (make-metadata :title "§ 52c (2) (a)"
        ;;                         :description {:de ""})
        ;;  :conclusion '(may-publish ?P ?W)
        ;;  :premises [(make-premise :statement '(type-of-use (the-use ?P ?W) commercial))
        ;;             (make-premise :statement '(search-type (the-search ?P ?W) professional))
        ;;             (make-premise :statement '(announcement (the-search ?P ?W)))
        ;;             (make-premise :statement '(valid AB-52c-2-a))])
        ])])]))
