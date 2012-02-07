;;; Copyright ? 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.examples.aston
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
              :creator "Martin Löhe"
              :publisher "Fraunhofer FOKUS, Berlin"
              :description {:en "The [Green Paper](http://ec.europa.eu/internal_market/copyright/docs/copyright-infso/greenpaper_en.pdf) focuses on the exceptions to  copyright which are most relevant for the 
dissemination of knowledge, namely:

- The exception for the benefit of libraries and archives;  
- The exception allowing dissemination of works for teaching and research purposes;  
- The exception for the benefit of people with a disability; 
- A possible exception for user-created content

Note: This example illustrates how descriptions can include paragraphs, hyperlinks, lists and other elements, using the Markdown wiki language."}
              :date "2011")
    
    :references 
    {"GreenPaper"
     (make-metadata
       :title "Green Paper on copyright in the knowledge economy"
       :creator "Commission of the European Communities"
       :identifier "http://ec.europa.eu/internal_market/copyright/docs/copyright-infso/greenpaper_en.pdf"
       :date "2008"
       :format "pdf"),
     
     "Aston2008" 
     (make-metadata 
       :title "Comment on the EU Green Paper ‘Copyright in the Knowledge Economy’"
       :creator "Dr. Nick Smith, Director of of Library and Information Services, Aston University"
       :date "2008"
       :identifier "https://circabc.europa.eu/d/d/workspace/SpacesStore/c2a76b48-fb92-4a63-aab4-4acbeb419dbf/aston_university.pdf" 
       )}))
       
(def should-some-exceptions-be-mandatory?
  (make-statement 
    :main true
    :header (make-metadata 
              :description {:en "(4) Should certain categories of exceptions be made mandatory to ensure more legal certainty and better protection of beneficiaries of exceptions? [@GreenPaper, p. 6]"
                            :de "(4) Sollten bestimmte Ausnahmekategorien verbindlich ausgestaltet werden, um ein größeres Maß an Rechtssicherheit zu gewährleisten und die Nutznießer dieser Ausnahmen besser zu schützen? [@GreenPaper, p. 6]"})
    :text {:en "Should certain categories of exceptions be made mandatory to ensure more legal 
                certainty and better protection of beneficiaries of exceptions?"
           :de "Sollten bestimmte Kategorien von Ausnahmen zur Pflicht gemacht werden, um sicherzustellen, mehr Rechtssicherheit und einen besseren Schutz der Empfänger von Ausnahmen?"}))

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

(def a1 (make-argument
          :conclusion should-some-exceptions-be-mandatory?
          :premises [(pm permitted-exceptions-should-be-harmonized)]))

(def a2-id (make-urn-symbol))

(def a2 (make-argument 
          :id a2-id
          :header (make-metadata 
                    :description {:en "Researchers and students increasingly work in more than one Member State, successively or concurrently. The patchy availability of the exceptions makes their work difficult, because what is lawful in one country (for example copying a small portion of a work for critical illustration) is probably unlawful in another. This is particularly the case at Aston where there are opportunities for students to work in a variety of countries on placement and therefore could potentially experience a range of legislation. We also welcome students on exchange from various countries.The situation is made worse by the provision in most Member States that contracts, governing the use of digital material, automatically override statute law. The supremacy of contracts means that statutory exceptions are probably unavailable even in the State that enacted them. This is potentially confusing for users of copyright material as well as information professionals supporting them, where detailed knowledge of specific contracts is required. It is vital that the research and teaching community in Higher Education the right to use copyright material in the context of ‘fair dealing’ to ensure that academics, librarians and other information professionals are not tied up with recording and monitoring activity within a regulatory framework which is already complex. This situation of legal uncertainty needs legislative action by the European institutions. [@Aston2008, p. 1]"
                                  :de "Forscher und Studenten zunehmend in mehr als einem Mitgliedstaat zu arbeiten, nacheinander oder gleichzeitig. Die lückenhafte Verfügbarkeit der Ausnahmen macht ihre Arbeit schwierig, weil, was in einem Land rechtmäßig (z. B. Kopieren von einem kleinen Teil eines Werkes für kritische Grafik) wahrscheinlich rechtswidrig in ein anderes. Dies ist insbesondere der Fall bei Aston in denen es Möglichkeiten für Studenten, um in einer Vielzahl von Ländern zur Platzierung der Arbeit und damit potenziell Erfahrung eine Reihe von Rechtsvorschriften. Wir begrüßen auch Schüler zum Austausch aus verschiedenen Ländern. Die Situation wird durch die Bereitstellung in den meisten Mitgliedstaaten, dass die Verträge, die die Verwendung von digitalem Material, automatisch überschreiben Gesetzesrecht gemacht. Die Überlegenheit der Verträge bedeutet, dass die gesetzlichen Ausnahmen wahrscheinlich nicht zur Verfügung stehen, auch in dem Staat, der sie erlassen. Das ist verwirrend für die Nutzer von urheberrechtlich geschütztem Material sowie Informationen Profis unterstützen sie, wo genaue Kenntnis der spezifischen Vertrügen erforderlich ist. Es ist wichtig, dass die Forschung und Lehre Gemeinde in Higher Education das Recht vor, urheberrechtlich geschütztes Material im Rahmen der ‘Fair Dealing’ verwenden, um die Wissenschaftler, Bibliothekare und andere Informationsspezialisten sicher nicht mit Aufzeichnung und Überwachung Aktivität in einem Rechtsrahmen gebunden, die ist schon komplex. Diese Situation der Rechtsunsicherheit muss legislative Maßnahmen der europäischen Institutionen. [@Aston2008, S. 1]" })
          :scheme "Argument from Practical Reasoning"
          :conclusion permitted-exceptions-should-be-harmonized 
          :premises [(make-premise :role "Goal" :statement easier-to-work-in-several-states), 
                     (make-premise :role "Values" :statement values-promoted),
                     (make-premise :role "Situation" :statement work-made-more-difficult)]))

(def a3 (make-argument 
          :conclusion (make-statement :atom `(~'undercut ~a2-id))
          :scheme "CQ1. Better Alternatives"
          :premises [(make-premise :statement better-ways)]))

(def aston1
  (enter-arguments graph1 [a1 a2 a3]))
  
; (argument-graph->xml aston)

; 
;
 
; (def db (db/make-database-connection "aston" "root" "pw1"))
; (import-from-argument-graph db aston true)

(defn -main []
  (let [dbname "aston"  ; (str "db-" (make-uuid))
        db (db/make-database-connection dbname "root" "pw1")]
    (db/create-argument-database 
      dbname 
      "root" 
      "pw1" 
      (make-metadata))
    (import-from-argument-graph db aston1 true)
    (let [aston2 (export-to-argument-graph db)]
      (println "(= aston1 aston2): " (= aston1 aston2))
      (argument-graph->xml aston1)
      (println "\n\n---------------------------\n\n")
      (argument-graph->xml aston2)))
  )
  


  