

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
   :premises [(make-premise :statement '(license-to-publish ?P ?W))
              (make-premise :statement '(valid UrhG-31)) ]))

(define german-copyright-law
  (make-section
   :id 'UrhG
   :header (make-metadata 
	    :title "Urheberrechtsgesetz"
	    :description {:de "Das deutsche Urheberrecht in der geltenden Fassung"
			  :en "The current German copyright law"})
   :schemes
   [UrhG-31]))