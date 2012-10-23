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
            :description {:en "The purpose of the [Green Paper](http://ec.europa.eu/internal_market/copyright/docs/copyright-infso/greenpaper_en.pdf) on “Copyright in the Knowledge Economy” is to “foster a debate on how knowledge for research, science 
and education can best be disseminated in the online environment.” [@GreenPaper, p. 3].

The Green Paper has two parts.  The first deals with general issues and the second deals with “specific issues related to the exceptions
and limitations which are most relevant for the dissemination of knowledge and whether these exceptions should evolve
in the era of digital dissemination.” [@GreenPaper, p. 3].

Here, we present a reconstruction of some of the policies and arguments put forward in the comments submitted in reponse to the Green Paper.
Our aim is not to comprehensively model all the policies and arguments submitted, but rather to model a sufficient number of
representative arguments and policies for the purpose of illustrating features of the IMPACT argument toolbox.

The Corpus Selection Working Group of the IMPACT project has chosen 4 of the 25 questions raised in the Green Paper, as well as
12 of the 323 comments submitted, representing a wide range of stakeholders, to be used for the research and development purposes of the project.

The four questions covered by this model are listed below.  Click on a question for further information."}
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
   :text {:en "Q4. Should certain categories of exceptions be made mandatory to ensure more legal 
                certainty and better protection of beneficiaries of exceptions?"
          :de "Q4. Sollten bestimmte Kategorien von Ausnahmen zur Pflicht gemacht werden, um sicherzustellen, mehr Rechtssicherheit und einen besseren Schutz der Empfänger von Ausnahmen?"}))

(def Q9
  (make-statement
   :main true
   :header (make-metadata 
            :description {:en "(9) Should the law be clarified with respect to whether the scanning of works held in 
libraries for the purpose of making their content searchable on the Internet goes beyond the scope of current
exceptions to copyright? [@GreenPaper, p. 12]"})
   :text {:en "Q9. Should the law be clarified with respect to whether the scanning of works held in 
libraries for the purpose of making their content searchable on the Internet goes beyond the scope of current
exceptions to copyright?"}))

(def Q9-LIBER-Proposal
  (make-statement
   :text {:en "Yes. The exceptions should be clarified to allow works held in libraries to be scanned for the purpose of making their content searchable on the Internet."}))

(def Q9-is-irrelevant
  (make-statement :text {:en "Question 9 is irrelevant"}))

(def scanning-by-libraries-should-be-obligatory
  (make-statement :text {:en "Public libraries have the right, indeed the responsibility, to publish digital copies of their collections for the purpose of preserving and distributing online the cultural heritage of Europe. [Aktionsbündnis, p. 6]"}))

(def Q12
  (make-statement
   :main true
   :header (make-metadata :description {:en "(12) How should the cross-border aspects of the orphan works issue be tackled to ensure EU-wide recognition of the solutions adopted in different Member States? [@GreenPaper, p. 12]"
                                        :de "(12) Wie sollten die grenzübergreifenden Aspekte, die sich im Zusammenhang mit verwaisten Werken stellen, in Angriff genommen werden, um die EU-weite Anerkennung der Regelungen der einzelnen Mitgliedstaaten zu gewährleisten?"})
   :text {:en "Q12. How should the cross-border aspects of the orphan works issue be tackled to 
ensure EU-wide recognition of the solutions adopted in different Member States?"}))

(def Q12-action-alliance-proposal
  (make-statement
   :header (make-metadata :description {:en "The German Action Alliance for Copyright in Education and Science has proposed the following policy for orphaned works [@Aktionsbündnis, p. 6-7].  

- Orphaned works may be published for noncommercial and private purposes, especially for archiving, research and educational purposes, if the copyright owner was not found after a documented standard search.

- Orphaned works may be published for commercial purposes, if the copyright owner was not able to be found after a documented, professional search and a public announcement of the search.

This policy has been modeled and can be simulated using the Policy Modeling tool of the IMPACT system.  (Click [here]() to view the model of this policy and simulate the effects of this policy in test cases.)

    To do: add the URL pointing to the policy model in the above link."})
   :text {:en "The policy proposed by the German Action Alliance should be adopted to address the cross-border aspects of orphaned works."}))

(def Q12-UKPA-proposal
  (make-statement
   :header (make-metadata :description {:en "The cross-border aspects of the orphaned works issue are already provided for by the proposal of
High Level Expert group, which recommends mutual recognition by Member States of each other's copryight exceptions. [@UKPA pg.12]"})
   :text {:en "The cross-border aspects of the orphaned works issue are already provided for by the proposal of
High Level Expert group, which recommends mutual recognition by Member States of each other's copryight exceptions."}))

(def Q12-Google-proposal
  (make-statement
   :header (make-metadata :description {:en "The cross-border aspects of the orphaned works should be handled through an amendment to Directive 2011/29/EC [@Google, pg. 14]."})
   :text {:en "The cross-border aspects of the orphaned works should be handled through an amendment to Directive 2011/29/EC."}))

(def Q12-SIIA-Proposal
  (make-statement
   :text {:en "While we do not support such an approach, to the extent that an orphan works standard is 
adopted throughout the EU, we recommend that a Community statutory instrument dealing with 
the problem of orphan works should be a stand-alone instrument. [@SIIA, p. 9]"}))

(def Q24
  (make-statement
   :main true
   :header (make-metadata :description {:en "(24) Should there be more precise rules regarding
what acts end users can or cannot do when making use of materials protected by
copyright? [@GreenPaper, p. 20]"})
   :text {:en "Q24. Should there be more precise rules regarding what acts end users can or
cannot do when making use of materials protected by copyright?"}))

(def Q24-LIBER-Proposal
  (make-statement
   :text {:en "No. Further restrictions on the end users of copyrighted materials should not be enacted."}))

(def Q24-SIIA-Proposal
  (make-statement
   :text {:en "No. The rules about which uses of copyrighted materials create derivate works, and thus require a license from the copyright owner, are already precise."}))

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

(def values-promoted-by-facilitating-research
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

(def preserving-freedom-of-contract
  (make-statement
   :text {:en "It is essential that the basic principle of freedom of contract be recognized and preserved by any 
copyright legislation."}))

(def harmonization-would-impair-freedom-of-contract
  (make-statement
   :text {:en "Harmonizing copyright exceptions would impair the freedom of contract."}))

(def impairing-freedom-of-contract-would-demote-innovation-and-dissemination
  (make-statement
   :text {:en "Impairing the freedom of contract would demote the values of innovation and the dissemination of knowledge and information."}))

(def lack-of-harmonization-facilitates-freedom-of-contract
  (make-statement
   :text {:en "Currently, the lack of harmonization of copyright exceptions faciliates the freedom of contract."}))

(def  we-would-know-if-the-scanning-rules-were-unclear
  (make-statement
   :text {:en "If the rules regulating the scanning of works held in libraries were unclear, this would be known."}))

(def we-do-not-know-the-scanning-rules-to-be-unclear
  (make-statement
   :text {:en "It is not known that the rules regulating the scanning of works in libraries are unclear."}))

(def directive-relates-to-right-and-exceptions
  (make-statement
   :text {:en "The 2001 EU Copyright Directive regards copyrights and exceptions, but not remedies for violations of copyrights."}))

(def stand-alone-instrument-would-regulate-separate-topics-with-separate-instruments
  (make-statement
   :text {:en "Using a stand-alone instrument to regulate orphaned works, instead of amending the 2001 Copyright Directive, would cause the separate topic of remedies for copyright violations to be regulated by a separate instrument."}))

(def separate-topics-should-be-regulated-by-separate-instruments
  (make-statement
   :text {:en "Separate legal topics should be regulated by separate instruments."}))

(def regulating-separate-topics-with-separate-instruments-promotes-legal-clarity
  (make-statement
   :text {:en "Regulating separate legal topics with separate instruments promotes the value of legal clarity."}))

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

> Researchers and students increasingly work in more than one Member State, successively or concurrently. The patchy availability of the exceptions makes their work difficult, because what is lawful in one country (for example copying a small portion of a work for critical illustration) is probably unlawful in another. This is particularly the case at Aston where there are opportunities for students to work in a variety of countries on placement and therefore could potentially experience a range of legislation. We also welcome students on exchange from various countries.The situation is made worse by the provision in most Member States that contracts, governing the use of digital material, automatically override statute law. The supremacy of contracts means that statutory exceptions are probably unavailable even in the State that enacted them. This is potentially confusing for users of copyright material as well as information professionals supporting them, where detailed knowledge of specific contracts is required. It is vital that the research and teaching community in Higher Education the right to use copyright material in the context of ‘fair dealing’ to ensure that academics, librarians and other information professionals are not tied up with recording and monitoring activity within a regulatory framework which is already complex. This situation of legal uncertainty needs legislative action by the European institutions. [@Aston2008, p. 1]

The Action Alliance for Copyright in Education and Science formulated this argument as follows:

> Different copyright exceptions have always been a hindrance for international, cross-border science and resarch. Binding harmoninzed copryight exceptions for all Member States are therefore desirable. [@Aktionsbündnis, p. 5]"
                                :de "Aston Universität argumentiert wie folgt:

> Forscher und Studenten zunehmend in mehr als einem Mitgliedstaat zu arbeiten, nacheinander oder gleichzeitig. Die lückenhafte Verfügbarkeit der Ausnahmen macht ihre Arbeit schwierig, weil, was in einem Land rechtmäßig (z. B. Kopieren von einem kleinen Teil eines Werkes für kritische Grafik) wahrscheinlich rechtswidrig in ein anderes. Dies ist insbesondere der Fall bei Aston in denen es Möglichkeiten für Studenten, um in einer Vielzahl von Ländern zur Platzierung der Arbeit und damit potenziell Erfahrung eine Reihe von Rechtsvorschriften. Wir begrüßen auch Schüler zum Austausch aus verschiedenen Ländern. Die Situation wird durch die Bereitstellung in den meisten Mitgliedstaaten, dass die Verträge, die die Verwendung von digitalem Material, automatisch überschreiben Gesetzesrecht gemacht. Die Überlegenheit der Verträge bedeutet, dass die gesetzlichen Ausnahmen wahrscheinlich nicht zur Verfügung stehen, auch in dem Staat, der sie erlassen. Das ist verwirrend für die Nutzer von urheberrechtlich geschütztem Material sowie Informationen Profis unterstützen sie, wo genaue Kenntnis der spezifischen Vertrügen erforderlich ist. Es ist wichtig, dass die Forschung und Lehre Gemeinde in Higher Education das Recht vor, urheberrechtlich geschütztes Material im Rahmen der ‘Fair Dealing’ verwenden, um die Wissenschaftler, Bibliothekare und andere Informationsspezialisten sicher nicht mit Aufzeichnung und Überwachung Aktivität in einem Rechtsrahmen gebunden, die ist schon komplex. Diese Situation der Rechtsunsicherheit muss legislative Maßnahmen der europäischen Institutionen. [@Aston2008, S. 1]

Das Aktionsbündnis „Urheberrecht für Bildung und Wissenschaft“ in Deutschland hat die gleiche Argument so formuliert:

>Es ist für die immer schon grenzüberschreitend international arbeitende Wissenschaft hinderlich, wenn Ausnahmen, z.B. für die Nutzung urheberrechtsgeschützten Materials in international verteilten Arbeitsgruppen, in verschiedenen Ländern unterschiedlich geregelt sind. Eine für alle Mitgliedsländer verbindliche Ausgestaltung zentraler Ausnahmekategorien ist daher erwünscht. [@Aktionsbündnis, p. 5]" }
                  :source "Aktionsbündnis;Aston2008")
         :scheme "Practical Reasoning"
         :conclusion permitted-exceptions-should-be-harmonized 
         :premises [(make-premise :role "Goal" :statement easier-to-work-in-several-states),
                    (make-premise :role "Action" :statement harmonizing-exceptions-would-help-academics)
                    (make-premise :role "Values Promoted" :statement values-promoted-by-facilitating-research),
                    (make-premise :role "Circumstances" :statement work-made-more-difficult)]))

(def a3 (make-argument
         :header (make-metadata :description {:en "There are better ways to help researchers and students to work in more than one Member
State than harmonizing copyright exceptions."})
         :conclusion (make-statement :atom `(~'undercut ~a2-id))
         :scheme "CQ1. Better Alternatives"
         :premises [(make-premise :statement better-ways)]))

(def a4 (make-argument
         :header (make-metadata :description {:en "In response to Question 12, the policy proposed by the Action Alliance of Germany should be adopted to handle the the cross-border aspects of orphaned works.."})
         :scheme "Position"
         :conclusion Q12
         :premises [(pm Q12-action-alliance-proposal)]))

(def a5 (make-argument
         :header (make-metadata :description {:en "In response to Question 9, Yes. The exceptions should be clarified to allow works held in libraries to be scanned for the purpose of making their content searchable on the Internet."})
         :scheme "Position"
         :conclusion Q9
         :premises [(pm Q9-LIBER-Proposal)]))

(def a6 (make-argument
         :header (make-metadata :description {:en "The Association of European Research Libraries (LIBER), in response to Question 9 of the Green Paper, argues that, yes, the law should the law be clarified with respect to whether the scanning of works held in libraries for the purpose of making their content searchable on the Internet goes beyond the scope of current exceptions to copyright, as follows:

> Not all the material digitised by publishers is scanned with OCR (Optical Character Recognition) with the purpose of making the resulting content searchable. If the rights holders will not do this, libraries should be able to offer this service. It would have a transformative effect on research, learning and teaching by opening up a mass of content to users which can be searched using search engines. The interests of copyright holders will not be harmed, because the resulting output will act as marketing material for their materials. [@LIBER, p. 3]"}
                                :source "LIBER")
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

> The essence of copyright legislation is the maintenance of a balance between the rights of the rights holders and the legitimate needs of users.The introduction of more restrictions will blur this distinction and make it more difficult for users of copyright material in their legitimate pursuits in teaching, learning and research in the European Community. [@LIBER, p. 5]"}
                                :source "LIBER")
         :conclusion Q24-LIBER-Proposal
         :scheme "Practical Reasoning"
         :premises [(make-premise :role "Circumstances" :statement (make-statement :text {:en "The essence of copyright legislation is the maintenance of a balance between the rights of the rights holders and the legitimate needs of users."}))
                    (make-premise :role "Action" :statement (make-statement :text {:en "The introduction of more restrictions on the end users would create an imbalance between the interests of copyright owners and end users and make it more difficult for users of copyright material in their legitimate pursuits in teaching, learning and research in the European Community."}))
                    (make-premise :role "Goal" :statement (make-statement :text {:en "Creating an imbalance between the interests of copyright owners and end users and making it more difficult for users of copyright material in their legitimate pursuits in teaching, learning and research in the European Community should be avoided."}))]))

(def a9 (make-argument
         :header (make-metadata :description {:en "Question 9 is irrelevant."})
         :scheme "Position"
         :conclusion Q9
         :premises [(pm Q9-is-irrelevant)]))

(def a10 (make-argument
          :header (make-metadata :description {:en "Question 9 is irrelevant if, as proposed by the Action Alliance in Germany, public libraries have the right, indeed the responsibility, to publish digital copies of their collections for the purpose of preserving and distributing online the cultural heritage of Europe. [Aktionsbündnis, p. 6]"
                                               :de "> Ein weiterer Klärungsbedarf, bei Sicherung der unter (6) und (7) generellen Forderung einer informationellen Grundversorgung mit den ursprünglichen Werken, wird nicht gesehen. Das Einscannen ganzer Bibliotheksbestände und deren öffentliche Bereitstellung sollten dann selbstverständlich sein. [Aktionsbündnis, p. 6]"}
                                 :source "Aktionsbündnis")
          :conclusion Q9-is-irrelevant
          :premises [(pm scanning-by-libraries-should-be-obligatory)]))

(def a11 (make-argument
          :header (make-metadata :description {:en "In response to Question 12, the policy proposed by the High Level Exprt Group of Germany should be adopted to handle the the cross-border aspects of orphaned works."})
          :scheme "Position"
          :conclusion Q12
          :premises [(pm Q12-UKPA-proposal)]))

(def a12 (make-argument
          :header (make-metadata :description {:en "In response to Question 12, the cross-border aspects of the orphaned works should be handled through an amendment to Directive 2011/29/EC."})
          :scheme "Position"
          :conclusion Q12
          :premises [(pm Q12-Google-proposal)]))

(def a13 (make-argument
          :header (make-metadata :description {:en "The Software & Information Industry Association (SIIA) opposes making certain categories of copyright exceptions mandatory:

> In sum, we are of the strong belief that government involvement in licensing, whether it be through guidelines or model agreements or through legislative mandates, would hinder innovation and dissemination.  Accordingly, we do not believe that such government involvement is not appropriate. [@SIIA, pp. 3-4]"}
                                 :source "SIIA")
          :scheme "Negative Practical Reasoning"
          :conclusion  (neg permitted-exceptions-should-be-harmonized)
          :premises [(make-premise :role "Goal" :statement preserving-freedom-of-contract),
                      (make-premise :role "Action" :statement harmonization-would-impair-freedom-of-contract),
                      (make-premise :role "Values Demoted" :statement impairing-freedom-of-contract-would-demote-innovation-and-dissemination),
                      (make-premise :role "Circumstances" :statement lack-of-harmonization-facilitates-freedom-of-contract)]))

(def a14 (make-argument
          :header (make-metadata :description {:en "There is no need to clarify copyright law regarding whether the scanning of works held in 
libraries for the purpose of making their content searchable on the Internet goes beyond the scope of current exceptions to copyright, since the law is already clear on this issue. Scanning is a form of copying which requires the authorization of the copyright owner.  No court has ever held otherwise.

The Software and Information Industry (SIIA) formulated this argument as follows in its comment:

> Scanning of copyright works is a form of copying and as such is generally prohibited under the Berne Convention and copyright laws of countries around the globe unless the copier has first 
obtained the copyright owner’s authorization to scan the work(s).  The ultimate purpose of the 
scanning  -- e.g., for indexing, cataloguing, searching or some other purpose -- should have no 
bearing on the ultimate determination that a copy is being made and that such activity requires 
the authorization of the copyright owner.  As a result, any public or private initiative to scan 
entire collections of works must require that the copyright owner opt-in, rather than putting the 
onus on the copyright owner to opt-out of the initiative.  We do not believe that there needs to be 
any further clarification in the law in this area.  To the best of our knowledge no court has ever 
held that such large-scale scanning activities are not prohibited under copyright law. [@SIIA, p. 7]"}
                                 :source "SIIA")
          :scheme "Argument from Ignorance"
          :conclusion  (neg Q9-LIBER-Proposal)
          :premises [(pm we-would-know-if-the-scanning-rules-were-unclear)
                     (pm we-do-not-know-the-scanning-rules-to-be-unclear)]))

(def a15  (make-argument
           :header (make-metadata :description {:en "In response to Question 12, a Community statutory instrument dealing with 
the problem of orphan works should be a stand-alone instrument."})
           :scheme "Position"
           :conclusion Q12
           :premises [(pm Q12-SIIA-Proposal)]))

(def a16 (make-argument
          :header (make-metadata :description {:en "The Software and Information Industry Association expressed this argument as follows:

> Because the 2001 Copyright Directive relates to rights and exceptions, but not remedies, it would be inappropriate for the 
Directive to be amended to include a provision relating to orphan works. [@SIIA, p. 9]"}
                                 :source "SIIA")
          :conclusion Q12-SIIA-Proposal
          :scheme "Practical Reasoning"
          :premises [(make-premise :role "Circumstances" :statement directive-relates-to-right-and-exceptions)
                     (make-premise :role "Action" :statement stand-alone-instrument-would-regulate-separate-topics-with-separate-instruments)
                     (make-premise :role "Goal" :statement separate-topics-should-be-regulated-by-separate-instruments)
                     (make-premise :role "Values Promoted" :statement regulating-separate-topics-with-separate-instruments-promotes-legal-clarity)]))

Q24-SIIA-Proposal

(def a17 (make-argument
         :header (make-metadata :description {:en "In response to Question 24, No. The rules about which uses of copyrighted materials create derivate works, and thus require a license from the copyright owner, are already precise.

This position was formulated by the Sofware and Information Industry Association (SIIA) as follows:

> The issue of user-created content is primarily one of education.  Until recently,
most people did not have the capability to use someone else’s creation to create a new work.
Copying was too difficult or expensive.  As a result, your average person did not need to have
even a rudimentary understanding of copyright rules.  But now, anyone can be a publisher.
It’s easy and inexpensive.  While this presents great new possibilities, it also poses great
challenges and risks for copyright owners.  Governments and educators need to do a better job
educating the public on what they can and cannot do with copyrighted works.  The public needs to better understand the purpose and goals of the copyright law and the sanctions for violating it.
In short, the rules of the road do not need to be changed, the people who drive on the road
need to better understand the rules. [@SIIA, pp. 15-16.]"}
                                :source "SIIA")
         :scheme "Position"
         :conclusion Q24
         :premises [(pm Q24-SIIA-Proposal)]))

(def copyright1
  (enter-arguments graph1 [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17]))

(defn -main []
  (let [dbname "copyright"  ; (str "db-" (make-uuid))
        db (db/make-database-connection dbname "root" "pw1")]
    (db/create-argument-database 
     dbname 
     "root" 
     "pw1" 
     (make-metadata))
    (import-from-argument-graph db copyright1 true)))
