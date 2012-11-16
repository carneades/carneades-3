;;; Copyright ? 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.examples.copyright-policies2
  (:use carneades.engine.dublin-core
        carneades.engine.scheme
        carneades.engine.argument))

;; test

;; This example illustrates the use of schemes for policy modeling, using
;; policies proposed in response to the EU Green Paper on Copyright in the Knowledge Economy.


(def UrhG-31
  (make-scheme                            
   :id 'UrhG-31
   :header (make-metadata :title "§ 31 UrhG de lege lata"
                          :description {:de "> Einräumung von
                          Nutzungsrechten. (1) Der Urheber kann einem
                          anderen das Recht einräumen, das Werk auf
                          einzelne oder alle Nutzungsarten zu
                          nutzen (Nutzungsrecht) ... (3) Das
                          ausschließliche Nutzungsrecht berechtigt den
                          Inhaber, das Werk unter Ausschluss aller
                          anderen Personen auf die ihm erlaubte Art zu
                          nutzen *und Nutzungsrechte einzuräumen* [§
                          31 UrhG, Hervorhebung hinzugefügt] ..."
                                        :en "> Licensing
                          copyrights. (1) The author can grant a
                          simple or exclusive license to others to use
                          the work … (3) Exclusive rights to use a
                          work give the licensee the sole right to use
                          the work in the ways stated by the license,
                          *along with the right to grant licenses to
                          third parties* [§ 31 UrhG, emphasis added]
                          …"})
   :conclusion '(may-publish ?P ?W)
   :premises [(make-premise :statement '(person ?P))
              (make-premise :statement '(work ?W))
              (make-premise :statement '(license-to-publish ?P ?W))
              (make-premise :statement '(valid UrhG-31)) ]))


(def copyright-policies2
  (make-theory
   :header 
   (make-metadata :title "Copyright in the Knowledge Economy - v2"
                  :description {:en ""}) ;; TODO add a description
   
   :language
   (make-language 
    (make-individual :symbol 'announcement-category :text {:en "Announcement" :de "Bekanntmachung"})
    (make-individual :symbol 'commercial :text {:en "Commercial Use"})
    (make-individual :symbol 'purpose :text {:en "Purpose"})
    (make-individual :symbol 'identifiers :text {:en "Identifiers"})
    (make-individual :symbol 'non-commercial :text {:en "Non-commercial Use"})
    (make-individual :symbol 'none :text {:en "None"})
    (make-individual :symbol 'professional :text {:en "Professional Documented Search"})
    (make-individual :symbol 'search :text {:en "Search"})
    (make-individual :symbol 'standard :text {:en "Standard DocumentedSearch"})
    (make-individual :symbol 'license :text {:en "License"})

    (make-role
     :category 'license
     :symbol 'license-to-publish
     :min 0
     :max nil
     :type :symbol
     :askable true
     :forms {:en (make-form :positive "%s has a license to publish %s."
                            :negative "%s does not have a license to publish %s."
                            :question "Does %s have a license to publish %s?")}
     :hint {:en "Information about an existing license."}
     :followups '[])
    
    (make-role
     :symbol 'may-publish
     :min 0
     :max nil
     :type :symbol
     :askable false
     :forms {:en (make-form :positive "%s may publish  %s."
                            :negative "%s may not publish %s."
                            :question "May %s publish %s?")})

    (make-concept
     :symbol 'person
     :askable true
     :forms {:en (make-form :positive "%s is a person."
                            :negative "%s is not a person."
                            :question "Is %s a person?")
             
             :de (make-form :positive "%s ist ein Rechtsperson."
                            :negative "%s ist nicht ein Rechtsperson."
                            :question "Ist %s ein Rechtsperson?")}
     :category 'identifiers
     :hint {:en "Please provide an identifier for the person interested in publishing the work, such as P1."}
     :followups '[work])

    (make-concept
     :symbol 'work
     :askable true
     :forms {:de (make-form :positive "%s ist ein Werk."
                            :negative "%s is nicht ein Werk."
                            :question "Ist %s ein Werk?")
             :en (make-form :positive "%s is a work."
                            :negative "%s is not a work."
                            :question "Is %s a work?")}
     :hint {:en "Please provide an identifier for the orphaned work, such as W1."}
     :category 'identifiers)  
    
    (make-predicate
     :symbol 'type-of-use
     :arity 3
     :askable true
     :forms {:de (make-form :positive "%s nutzt %s für folgender Zwecken: %s."
                            :negative "%s nutzt %s nicht für folgender Zwecken: %s."
                            :question "Nutzt %s den Werk %s für folgender Zwecken: %s?")
             :en (make-form :positive "%s uses %s for %s purposes."
                            :negative "%s does not use %s for %s purposes."
                            :question "Does %s use %s for %s purposes?")}
     :hint {:en "Will the work be used for commercial or non-commercial purposes?"}
     :answers '[[] [] [commercial non-commercial]]
     :category 'purpose
     :widgets '[text text select])

    (make-predicate
     :symbol 'search-type
     :arity 3
     :askable true
     :forms {:en (make-form :positive "%s conducted a %3$s search for the copyright owner of %2$s."
                            :negative "%s did not conduct a %s search for the copyright owner of %s."
                            :question "Did %s conduct a %3$s search for the copyright owner of %2$s?")}
     :hint {:en "What type of search was performed to try to find the copyright owner?"}
     :category 'search
     :answers '[[] [] [standard professional none]]
     :widgets '[text text select]
     :followups ['announcement])

    (make-role
     :symbol 'announcement
     :askable true
     :forms {:en (make-form :positive "The search conducted by %s for the owner of %s was publically announced."
                            :negative "The search conducted by %s for the owner of %s was not publically announced."
                            :question "Was the search conducted by %s for the owner of %s publically announced?")}
     :category 'announcement-category)
    
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
     :main-issue '(may-publish ?Person ?Work)
     :header (make-metadata :title "Q12. Cross-Border Aspects of Orphaned Works"
                            :description {:en "Question 12 of the Green Paper on Copyright in the Knowledge Economy [@GreenPaper, p. 12] asks:

> (12) How should the cross-border aspects of the orphan works issue be tackled to ensure EU-wide recognition of
> the solutions adopted in different Member States?

This arguments pro and con the policy proposals for this issue can be browsed in the [argument map](http://localhost:8080/policymodellingtool/#/arguments/outline/copyright).

"})
     
     ;; one section below for each policy proposed for this issue as well as for current polices on this issue.

     :sections  
     [(make-section
       :id 'UrhG
       :header (make-metadata :title "Urheberrechtsgesetz"
                              :description {:de "Das deutsche Urheberrecht in der geltenden Fassung"
                                            :en "The current German copyright law"})
       :schemes
       [UrhG-31])

      (make-section
       :id 'Q12-Aktionsbundnis
       :header (make-metadata :title "Orphaned Works Policy Proposed by the Aktionsbündnisses ‟Urheberrecht für Bildung und Wissenschaft”"
                              :description {:en "The German “Action Alliance” on copyright for education and science proposes the following
policies for handling orphaned works [@Aktionsbündnis, pp. 6-7]."})


       :schemes
       [UrhG-31

        (make-scheme                            
         :id 'AB-52c-1-a
         :header (make-metadata :title "§ 52c (1) (a)"
                                :description {:de "(1) Öffentliche Zugänglichmachung für nicht-gewerbliche und private Zwecke, insbesondere durch Nutzer für Zwecke der Archivierung und für Forschung und Ausbildung ... (a)  Zulässig  ist  die  öffentliche  Zugänglichmachung  von  Werken, deren Urheber oder Rechteinhaber
nach einer dokumentierten Standardsuche [alternativ: einer zeitlich auf 30 Tage öffentlichen Bekanntmachung] nicht ermittelt werden können."})
         :conclusion '(may-publish ?P ?W)
         :premises [(make-premise :statement '(person ?P))
                    (make-premise :statement '(work ?W))
                    (make-premise :statement '(type-of-use ?P ?W non-commercial))
                    (make-premise :statement '(search-type ?P ?W standard))
                    (make-premise :statement '(valid AB-52c-1-a)) ])

        
        (make-scheme
         :id 'AB-52c-2-a
         :header (make-metadata :title "§ 52c (2) (a)"
                                :description {:de "(2) Öffentliche Zugänglichmachung für gewerbliche Zwecke ... (a)  Zulässig  ist  die  öffentliche
Zugänglichmachung  von  Werken, deren Urheber oder Rechteinhaber 
nach einer angemessenen professionellen und dokumentierten Suche und einer öffentlichen 
Bekanntmachung nicht ermittelt werden können."})
         :conclusion '(may-publish ?P ?W)
         :premises [(make-premise :statement '(person ?P))
                    (make-premise :statement '(work ?W))
                    (make-premise :statement '(type-of-use ?P ?W commercial))
                    (make-premise :statement '(search-type ?P ?W professional))
                    (make-premise :statement '(announcement ?P ?W))
                    (make-premise :statement '(valid AB-52c-2-a))])])])]))
