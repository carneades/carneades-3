;;; Copyright ? 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.examples.copyright-arguments
  (:use carneades.engine.uuid
        carneades.engine.statement
        carneades.engine.argument
        carneades.engine.argument-graph
        carneades.engine.dublin-core
        carneades.database.import
        carneades.database.export 
        carneades.xml.caf.export
        carneades.maps.lacij)
  (:require [carneades.database.db :as db]))

;; This example illustrates: 
;; - Metadata describing the map as a whole, as well as each statement and argument, using
;;   the Dublin Core elements
;; - How to define one or more main issues and link these to questions raised in source documents.
;; - Support for representing statements and arguments in multiple natural languages
;; - References (citations) to source documents in the descriptions of statements and arguments
;;   and a list of the metadata of the references. 
;; - Undercutters and critical questions
;; - Labeling the scheme and premise roles of arguments
;; - The use of Universal Resource Names (URNs) in the UUID namespace as identifiers
;;   for statements and arguments


(def graph1 
  (make-argument-graph 
   :header (make-metadata 
            :title "Reconstruction of Comments on the EU Green Paper “Copyright in the Knowledge Economy” "
            :creator "IMPACT Project"
            :description {:en "The purpose of the [Green Paper](http://ec.europa.eu/internal_market/copyright/docs/copyright-infso/greenpaper_en.pdf) on “Copyright iin the Knowledge Economy” is to “foster a debate on how knowledge for research, science 
and education can best be disseminated in the online environment.” [@GreenPaper, p. 3].

The Green Paper has two parts.  The first deals with general issues and the second deals with “specific issues related to the exceptions
and limitations which are most relevant for the dissemination of knowledge and whether these exceptions should evolve
in the era of digital dissemination.” [@GreenPaper, p. 3].

Here, we present a reconstruction of some of the policies and arguments put forward in the comments submitted in reponse to the Green Paper.
Our aim is not to comprehensively model all the policies and arguments submitted, but rather to model a sufficient number of
representative arguments and policies for the purpose of illustrating features of the IMPACT argument toolbox.

The Corpus Selection Working Group of the IMPACT project has chosen 4 of the 25 questions raised in the Green Paper, as well as
12 of the 323 comments submitted, representing a wide range of stakeholders, to be used for the research and development purposes of the project.

The four questions covered by this model are listed below.  Click on an question for further information."}
            :date "2011")
   
   :references 
   {"GreenPaper"
    (make-metadata
     :title "Green Paper on Copyright in the Knowledge Economy"
     :creator "Commission of the European Communities"
     :identifier "http://ec.europa.eu/internal_market/copyright/docs/copyright-infso/greenpaper_en.pdf"
     :date "2008"
     :format "pdf"),

    "Aktionsbündnis"
    (make-metadata
     :title "Stellungnahme zum Grünbuch Urheberrechte in der wissensbestimmten Wirtschaft"
     :creator "Aktionsbündnisses Urheberrecht für Bildung und Wissenschaft"
     :date "November 25, 2008"
     :identifier "https://circabc.europa.eu/d/d/workspace/SpacesStore/3b68d963-7d0f-4a3a-8b89-4ecfae2557c0/aktionsbundnis_urheberrecht_fur_bildung_und_wissenschaft_de.pdf"),
    
    "Aston2008" 
    (make-metadata 
     :title "Comment on the EU Green Paper ‘Copyright in the Knowledge Economy’"
     :creator "Dr. Nick Smith, Director of of Library and Information Services, Aston University"
     :date "2008"
     :identifier "https://circabc.europa.eu/d/d/workspace/SpacesStore/c2a76b48-fb92-4a63-aab4-4acbeb419dbf/aston_university.pdf"),

    "CBS"
    (make-metadata
     :title "Comments on the Commission's Green Paper on Copyright in the Knowledge Economy"
     :creator "Copenhagen Business School"
     :date "2008"
     :identifier "https://circabc.europa.eu/d/d/workspace/SpacesStore/15ccd770-b60f-45ab-97c8-0ee2e59cc676/CBS-Copenhagen_Business_School.pdf"),

    "CENL"
    (make-metadata
     :title "Green Paper “Copyright in the Knowledge Economy” – Response of the Conference of European National Librarians"
     :creator "Conference of European National Librarians"
     :date "2008"
     :identifier "https://circabc.europa.eu/d/d/workspace/SpacesStore/a149d7ef-17f7-4dd8-b105-6d0b0055aefd/cenl_conference_of_european_national_librarians.pdf"),

    "EFJ"
    (make-metadata
     :title "Response to the Green Paper on Copyright in the Knowledge Economy"
     :creator "European Federation of Journalists"
     :date "2008"
     :identifier "https://circabc.europa.eu/d/d/workspace/SpacesStore/e25b57c6-ceef-40ca-af1a-58d218a22614/EFJ-%20European%20Federation%20of%20Journalists.pdf"),

    "FaberAndFaber"
    (make-metadata
     :title "Consultation on Copyright in the Knowledge Economy"
     :creator "Faber and Faber Ltd"
     :date "November, 2008"
     :identifier "https://circabc.europa.eu/d/d/workspace/SpacesStore/c0a2fb15-0db9-44b8-b92a-376852035cbe/faber_and_faber_ltd.pdf"),

    "GIART"
    (make-metadata
     :title "GIART Answer to the Commission's Green Paper on Copyright in the Knowledge Economy"
     :creator "GIART International Organisation of Performing Artists"
     :date "January 26, 2009"
     :identifier "https://circabc.europa.eu/d/d/workspace/SpacesStore/89cbf2e7-a9eb-4489-ad90-8061a87e4092/giart_international_organisation_of_performing_artists.pdf"),

    "Google"
    (make-metadata
     :title "Google's Contribution to the European Commission public consultation on “Copyright in the Knowledge Society”"
     :creator "Google"
     :date "November, 2008"
     :identifier "https://circabc.europa.eu/d/d/workspace/SpacesStore/fd166f67-d9bd-4e30-baa4-6867250341f8/google.pdf"),

    "LIBER"
    (make-metadata
     :title "Green Paper Copyright in the Knowledge Economy"
     :creator "Association of European Research Libraries"
     :date "2009"
     :identifier "https://circabc.europa.eu/d/d/workspace/SpacesStore/84235a12-f544-48e4-9a95-7b75565337aa/liber_ligue_des_bibliothtques_europuennes_de_recherche.pdf"),

    "MediaSet"
    (make-metadata
     :title "Green Paper on Copyright in the Knowledge Economy"
     :creator "Mediaset S.p.A."
     :date "November 28, 2008"
     :identifier "https://circabc.europa.eu/d/d/workspace/SpacesStore/96fb4b45-5ed9-400d-aa8f-bb618c6fd3d7/mediaset.pdf"),

    "NationalArchives"
    (make-metadata
     :title "European Commission Green Paper: Copyright in the Knowledge Economy"
     :creator "National Archives of the United Kingdom"
     :date "September 12, 2008"
     :identifier "https://circabc.europa.eu/d/d/workspace/SpacesStore/552bbafd-da1c-4c0e-9f37-f294b3e54acc/national_archives_of_the_united_kingdom.pdf")

    "SIIA"
    (make-metadata
     :title "Comments on the EC Green Paper on Copyright in the Knowledge Economy"
     :creator "Software and Information Industry Association"
     :date "November 24, 2008"
     :identifier "https://circabc.europa.eu/d/d/workspace/SpacesStore/1ef2b5d8-22ae-41c0-9b97-ac65b2d171fb/siia-software_and_information_industry_association.pdf"),

    "UKPA"
    (make-metadata
     :title "Consultation on Copyright in the Knowledge Economy European Commission Green Paper"
     :creator "UK Publishers Association"
     :date "November, 2008"
     :identifier "https://circabc.europa.eu/d/d/workspace/SpacesStore/1d65c2f1-5d99-4a10-8d97-0301e8e01e4c/uk_publishers_association.pdf")

    }))
       
(def Q4
  (make-statement 
    :main true
    :header (make-metadata 
              :description {:en "(4) Should certain categories of exceptions be made mandatory to ensure more legal certainty and better protection of beneficiaries of exceptions? [@GreenPaper, p. 6]"
                            :de "(4) Sollten bestimmte Ausnahmekategorien verbindlich ausgestaltet werden, um ein größeres Maß an Rechtssicherheit zu gewährleisten und die Nutznießer dieser Ausnahmen besser zu schützen? [@GreenPaper, p. 6]"})
    :text {:en "Should certain categories of exceptions be made mandatory to ensure more legal 
                certainty and better protection of beneficiaries of exceptions?"
           :de "Sollten bestimmte Kategorien von Ausnahmen zur Pflicht gemacht werden, um sicherzustellen, mehr Rechtssicherheit und einen besseren Schutz der Empfänger von Ausnahmen?"}))

(def Q9
  (make-statement
   :main true
   :header (make-metadata 
            :description {:en "(9) Should the law be clarified with respect to whether the scanning of works held in 
libraries for the purpose of making their content searchable on the Internet goes beyond the scope of current
exceptions to copyright? [@GreenPaper, p. 12]"})
   :text {:en "Should the law be clarified with respect to whether the scanning of works held in 
libraries for the purpose of making their content searchable on the Internet goes beyond the scope of current
exceptions to copyright?"}))

(def Q9-LIBER-Proposal
  (make-statement
   :text {:en "Yes. The exceptions should be clarified to allow works held in libraries to be scanned for the purpose of making their content searchable on the Internet."}))

(def Q12
  (make-statement
   :main true
   :header (make-metadata :description {:en "(12) How should the cross-border aspects of the orphan works issue be tackled to ensure EU-wide recognition of the solutions adopted in different Member States? [@GreenPaper, p. 12]"
                                        :de "(12) Wie sollten die grenzübergreifenden Aspekte, die sich im Zusammenhang mit verwaisten Werken stellen, in Angriff genommen werden, um die EU-weite Anerkennung der Regelungen der einzelnen Mitgliedstaaten zu gewährleisten?"})
   :text {:en "How should the cross-border aspects of the orphan works issue be tackled to 
ensure EU-wide recognition of the solutions adopted in different Member States?"}))

(def Q24
  (make-statement
   :main true
   :header (make-metadata :description {:en "(24) Should there be more precise rules regarding
what acts end users can or cannot do when making use of materials protected by
copyright? [@GreenPaper, p. 20]"})
   :text {:en "Should there be more precise rules regarding what acts end users can or
cannot do when making use of materials protected by copyright?"}))

(def Q24-LIBER-Proposal
  (make-statement
   :text {:en "No. Further restrictions on the end users of copyrighted materials should not be enacted."}))

(def permitted-exceptions-should-be-harmonized 
  (make-statement 
    :text {:en "The permitted exceptions should be harmonised so that they are available in all Member States."
           :de "Die zulässigen Ausnahmen sollten harmonisiert, so dass sie in allen Mitgliedstaaten sein."}))

(def community-laws-should-override-contracts 
  (make-statement 
    :text {:en "Community laws should render void any contractual term purporting to eliminate an exception to copyright law."
           :de "Gemeinschaftlichen Rechtsvorschriften sollte die Nichtigkeit einer Vertragsklausel, die angeblich eine Ausnahme im Urheberrecht zu beseitigen."}))

(def easier-to-work-in-several-states
  (make-statement 
    :text {:en "Performing the action of harmonizing the exceptions and giving precedence to community law over contracts would  achieve a state in which it easier for researchers and students to work in more than one Member State."
           :de "Performing the Aktion der Harmonisierung der Ausnahmen und den Vorrang zu geben Gemeinschaftsrechts über Verträge würde ein Zustand, in dem es einfacher für Forscher und Studenten in mehr als einem Mitgliedstaat zu arbeiten."}))

(def values-promoted
  (make-statement 
    :text {:en "Achieving the goal of making it easier for researchers and students to work in more than one Member State would promote the values of efficiency, legal certainty, scientific research and education."
           :de "Die Erreichung des Ziels der Erleichterung für Forscher und Studenten in mehr als einem Mitgliedstaat arbeiten würden die Werte Effizienz, Rechtssicherheit, wissenschaftliche Forschung und Bildung."}))

(def work-made-more-difficult
  (make-statement 
    :text {:en "In the circumstances: Researchers and students increasingly work in more than one Member State. The patchy availability of exceptions makes their work difficult, because what is lawful in one country is probably unlawful in another. The situation is made worse by the provision of most Member States that contracts, governing the use of digital material, automatically overrides statute law."  
           :de "Unter diesen Umständen: Forscher und Studenten zunehmend in mehr als einem Mitgliedstaat zu arbeiten Die lückenhafte Verfügbarkeit von Ausnahmen macht ihre Arbeit schwierig, weil das, was in einem Land rechtmäßig ist wahrscheinlich rechtswidrig in einem anderen die Situation noch schlimmer wird durch die Bereitstellung der meisten gemacht.. Mitgliedstaaten, dass die Verträge, die die Verwendung von digitalem Material, automatisch außer Kraft Gesetzesrecht."}))      

(def better-ways
  (make-statement  
    :text {:en "There are better ways to promote efficiency, legal certainty, research and education than making it easier for researchers and students to work in more than one Member State."
           :de "Es gibt bessere Wege, um die Effizienz, Rechtssicherheit, Forschung und Bildung als erleichtert es Forschern und Studenten in mehr als einem Mitgliedstaat Arbeit zu fördern."}))

(def harmonizing-exceptions-would-help-academics
  (make-statement
   :text {:en "Harmonizing the copyright exceptions would make it easier for researchers and students to work in more than one Member State."}))

(def action-alliance-Q12-proposal
  (make-statement
   :header (make-metadata :description {:en "The German Action Alliance for Copyright in Education and Science has proposed the following policy for orphaned works [@Aktionsbündnis, p. 6-7].  

- Orphaned works may be published for noncommercial and private purposes, especially for archiving, research and educational purposes, if the copyright owner was not found after a documented standard search.

- Orphaned works may be published for commercial purposes, if the copyright owner was not able to be found after a documented, professional search and a public announcement of the search.

This policy has been modeled and can be simulated using the Policy Modeling tool of the IMPACT system.  (Click [here]() to view the model of this policy and simulate the effects of this policy in test cases.)

    To do: add the URL pointing to the policy model in the above link."})
   :text {:en "The orphaned works policy proposed by the German Action Alliance."}))

(def a1 (make-argument
         :header (make-metadata :description {:en "In response to Question 4, yes, the permitted copyright exceptions should be harmonized."})
         :scheme "Position"
         :conclusion Q4
         :premises [(pm permitted-exceptions-should-be-harmonized)]))

(def a2-id (make-urn-symbol))

(def a2 (make-argument 
          :id a2-id
          :header (make-metadata 
                   :description {:en "Aston University argues that the permitted copyright exceptions should be harmonized:

> Researchers and students increasingly work in more than one Member State, successively or concurrently. The patchy availability of the exceptions makes their work difficult, because what is lawful in one country (for example copying a small portion of a work for critical illustration) is probably unlawful in another. This is particularly the case at Aston where there are opportunities for students to work in a variety of countries on placement and therefore could potentially experience a range of legislation. We also welcome students on exchange from various countries.The situation is made worse by the provision in most Member States that contracts, governing the use of digital material, automatically override statute law. The supremacy of contracts means that statutory exceptions are probably unavailable even in the State that enacted them. This is potentially confusing for users of copyright material as well as information professionals supporting them, where detailed knowledge of specific contracts is required. It is vital that the research and teaching community in Higher Education the right to use copyright material in the context of ‘fair dealing’ to ensure that academics, librarians and other information professionals are not tied up with recording and monitoring activity within a regulatory framework which is already complex. This situation of legal uncertainty needs legislative action by the European institutions. [@Aston2008, p. 1]"
                                 :de "Aston Universität argumentiert wie folgt:

> Forscher und Studenten zunehmend in mehr als einem Mitgliedstaat zu arbeiten, nacheinander oder gleichzeitig. Die lückenhafte Verfügbarkeit der Ausnahmen macht ihre Arbeit schwierig, weil, was in einem Land rechtmäßig (z. B. Kopieren von einem kleinen Teil eines Werkes für kritische Grafik) wahrscheinlich rechtswidrig in ein anderes. Dies ist insbesondere der Fall bei Aston in denen es Möglichkeiten für Studenten, um in einer Vielzahl von Ländern zur Platzierung der Arbeit und damit potenziell Erfahrung eine Reihe von Rechtsvorschriften. Wir begrüßen auch Schüler zum Austausch aus verschiedenen Ländern. Die Situation wird durch die Bereitstellung in den meisten Mitgliedstaaten, dass die Verträge, die die Verwendung von digitalem Material, automatisch überschreiben Gesetzesrecht gemacht. Die Überlegenheit der Verträge bedeutet, dass die gesetzlichen Ausnahmen wahrscheinlich nicht zur Verfügung stehen, auch in dem Staat, der sie erlassen. Das ist verwirrend für die Nutzer von urheberrechtlich geschütztem Material sowie Informationen Profis unterstützen sie, wo genaue Kenntnis der spezifischen Vertrügen erforderlich ist. Es ist wichtig, dass die Forschung und Lehre Gemeinde in Higher Education das Recht vor, urheberrechtlich geschütztes Material im Rahmen der ‘Fair Dealing’ verwenden, um die Wissenschaftler, Bibliothekare und andere Informationsspezialisten sicher nicht mit Aufzeichnung und Überwachung Aktivität in einem Rechtsrahmen gebunden, die ist schon komplex. Diese Situation der Rechtsunsicherheit muss legislative Maßnahmen der europäischen Institutionen. [@Aston2008, S. 1]" })
          :scheme "Practical Reasoning"
          :conclusion permitted-exceptions-should-be-harmonized 
          :premises [(make-premise :role "Goal" :statement easier-to-work-in-several-states),
                     (make-premise :role "Action" :statement harmonizing-exceptions-would-help-academics)
                     (make-premise :role "Values" :statement values-promoted),
                     (make-premise :role "Circumstances" :statement work-made-more-difficult)]))

(def a3 (make-argument
         :header (make-metadata :description {:en "There are better ways to help researchers and students to work in more than one Member
State than harmonizing copyright exceptions."})
         :conclusion (make-statement :atom `(~'undercut ~a2-id))
         :scheme "CQ1. Better Alternatives"
         :premises [(make-premise :statement better-ways)]))

(def a4 (make-argument
         :header (make-metadata :description {:en "In response to Question 12, No. Further restrictions on the end users of copyrighted materials should not be enacted."})
         :scheme "Position"
         :conclusion Q12
         :premises [(pm action-alliance-Q12-proposal)]))

(def a5 (make-argument
         :header (make-metadata :description {:en "In response to Question 9, Yes. The exceptions should be clarified to allow works held in libraries to be scanned for the purpose of making their content searchable on the Internet."})
         :scheme "Position"
         :conclusion Q9
         :premises [(pm Q9-LIBER-Proposal)]))

(def a6 (make-argument
         :header (make-metadata :description {:en "The Association of European Research Libraries (LIBER), in response to Question 9 of the Green Paper, argues that, yes, the law should the law be clarified with respect to whether the scanning of works held in libraries for the purpose of making their content searchable on the Internet goes beyond the scope of current exceptions to copyright, as follows:

> Not all the material digitised by publishers is scanned with OCR (Optical Character Recognition) with the purpose of making the resulting content searchable. If the rights holders will not do this, libraries should be able to offer this service. It would have a transformative effect on research, learning and teaching by opening up a mass of content to users which can be searched using search engines. The interests of copyright holders will not be harmed, because the resulting output will act as marketing material for their materials. [@LIBER, p. 3]"})
         :conclusion Q9-LIBER-Proposal
         :scheme "Practical Reasoning"                                     
         :premises [(make-premise :role "Circumstances" :statement (make-statement :text {:en "Not all the material digitised by publishers is scanned with OCR (Optical Character Recognition) with the purpose of making the resulting content searchable."}))
                    (make-premise :role "Action" :statement (make-statement :text {:en "Clarifying the law to allow works held in libraries for the purpose of making the resulting content searchable on the Internet would have a transformative effect on research, learning and teaching."}))
                    (make-premise :role "Goal" :statement (make-statement :text {:en "Realizing a transformative effect on research, learning and teaching is an important social goal."}))]))

(def a7 (make-argument
         :header (make-metadata :description {:en "In response to Question 24, No. Further restrictions on the end users of copyrighted materials should not be enacted."})
         :scheme "Position"
         :conclusion Q24
         :premises [(pm Q24-LIBER-Proposal)]))

(def a8 (make-argument
         :header (make-metadata :description {:en "The Association of European Research Libraries (LIBER), in response to Question 24 of the Green Paper, argues that, no, the rules regarding what acts end users can or cannot do when making use of materials protected by copyright should not be made more precise:

> The essence of copyright legislation is the maintenance of a balance between the rights of the rights holders and the legitimate needs of users.The introduction of more restrictions will blur this distinction and make it more difficult for users of copyright material in their legitimate pursuits in teaching, learning and research in the European Community. [@LIBER, p. 5]"})
         :conclusion Q24-LIBER-Proposal
         :scheme "Practical Reasoning"
         :premises [(make-premise :role "Circumstances" :statement (make-statement :text {:en "The essence of copyright legislation is the maintenance of a balance between the rights of the rights holders and the legitimate needs of users."}))
                    (make-premise :role "Action" :statement (make-statement :text {:en "The introduction of more restrictions on the end users would create an imbalance between the interests of copyright owners and end users and make it more difficult for users of copyright material in their legitimate pursuits in teaching, learning and research in the European Community."}))
                    (make-premise :role "Goal" :statement (make-statement :text {:en "Creating an imbalance between the interests of copyright owners and end users and making it more difficult for users of copyright material in their legitimate pursuits in teaching, learning and research in the European Community should be avoided."}))]))

(def copyright1
  (enter-arguments graph1 [a1, a2, a3, a4, a5, a6, a7, a8]))

(defn -main []
  (let [dbname "copyright"  ; (str "db-" (make-uuid))
        db (db/make-database-connection dbname "root" "pw1")]
    (db/create-argument-database 
      dbname 
      "root" 
      "pw1" 
      (make-metadata))
    (import-from-argument-graph db copyright1 true)))


  


  