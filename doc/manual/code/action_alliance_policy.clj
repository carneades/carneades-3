(def action-alliance-policy
     (make-section
      :id 'Q12-Aktionsbundnis
      :header (make-metadata :title "Orphaned Works Policy Proposed by
the Aktionsbündnisses Urheberrecht für Bildung und Wissenschaft"
			     :description {:en "The German Action
Alliance on copyright for education and science proposes the
following policies for handling orphaned works [@Aktionsbündnis,
pp. 6-7]."})

      :schemes
      [UrhG-31

       (make-scheme                            
	:id 'AB-52c-1-a
	:header (make-metadata :title "§ 52c (1) (a)"
			       :description {:de "(1) Öffentliche
Zugänglichmachung für nicht-gewerbliche und private Zwecke,
insbesondere durch Nutzer für Zwecke der Archivierung und für
Forschung und Ausbildung ... (a) Zulässig ist die öffentliche
Zugänglichmachung von Werken, deren Urheber oder Rechteinhaber nach
einer dokumentierten Standardsuche [alternativ: einer zeitlich auf 30
Tage öffentlichen Bekanntmachung] nicht ermittelt werden können."})
	:conclusion '(may-publish ?P ?W)
	:premises 
        [(make-premise :statement '(type-of-use (the-use ?P ?W) non-commercial))
	 (make-premise :statement '(search-type (the-search ?P ?W) standard))
	 (make-premise :statement '(valid AB-52c-1-a)) ])
       
       (make-scheme
	:id 'AB-52c-2-a
	:header (make-metadata :title "§ 52c (2) (a)"
			       :description {:de "(2) Öffentliche
Zugänglichmachung für gewerbliche Zwecke ... (a) Zulässig ist die
öffentliche Zugänglichmachung von Werken, deren Urheber oder
Rechteinhaber nach einer angemessenen professionellen und
dokumentierten Suche und einer öffentlichen Bekanntmachung nicht
ermittelt werden können."})
	:conclusion '(may-publish ?P ?W)
	:premises 
        [(make-premise :statement '(type-of-use (the-use ?P ?W) commercial))
	 (make-premise :statement '(search-type (the-search ?P ?W) professional))
	 (make-premise :statement '(announcement (the-search ?P ?W)))
	 (make-premise :statement '(valid AB-52c-2-a))])]))

