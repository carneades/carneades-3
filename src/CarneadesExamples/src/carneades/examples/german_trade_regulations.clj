;;; Copyright (c) 2012 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.examples.german-trade-regulations
   ^{:doc "A model of §13b of the German Gewerbeordung (GeWo), which regulates the recognition of foreign certificates and documents when foreign companies apply for permission to conduct business in Germany."}
  
  (:use carneades.engine.dublin-core
        carneades.engine.scheme
        carneades.engine.argument))

(def german-trade-regulations
  (make-theory
   :header 
   (make-metadata :title "Recognition of Foreign Certificates and Documents"
                  :description {:en "A model of §13b GeWo, which  which regulates the recognition of foreign certificates and documents whdn foreign companies apply for permission to conduct business in Germany."})
   
   :language
   {'world-wide (make-individual :symbol 'world-wide
                                 :text {:en "world-wide"})

    'scope-of-activities
    (make-predicate
     :symbol 'scope-of-activities
     :arity 2
     :forms {:en (make-form
                  :positive "The activities of the tour operator, %s, has %s scope."
                  :negative "The activities of the tour operator, %s, does not have %s scope."
                  :question "Do the activities of the tour operator, %s, have %s scope?")}
     :hint {:en "What is the scope of the tour operator's commercial activities?"}
     :widgets '[text select]
     :answers '[[] [world-wide European Polish]]
     :category 'scope)

    }

   :sections
   [(make-section
     :main-issue '(minimum-guarantee ?O ?G)
     :id 'minimum-guarantee
     :header (make-metadata :title "Recognition of foreign certificates and documents"
                            :description {:en "Recognition of foreign certificates and documents" })
     :sections
     [(make-section
       :header (make-metadata :title "§13b GeWo"
                              :description {:de
 "§ 13b Anerkennung ausländischer Unterlagen und Bescheinigungen

1. Soweit nach diesem Gesetz oder einer auf Grund dieses Gesetzes erlassenen Rechtsverordnung die Zuverlässigkeit oder die Vermögensverhältnisse einer Person zu prüfen sind, sind als Nachweis für die Zuverlässigkeit und für geordnete Vermögensverhältnisse von Gewerbetreibenden aus einem anderen Mitgliedstaat der Europäischen Union oder einem anderen Vertragsstaat des Abkommens über den Europäischen Wirtschaftsraum Unterlagen als ausreichend anzuerkennen, die im Herkunftsstaat ausgestellt wurden und die belegen, dass die Anforderungen an die Zuverlässigkeit und die geordneten Vermögensverhältnisse des Gewerbetreibenden erfüllt werden. Dabei kann verlangt werden, dass die Unterlagen in beglaubigter Kopie und beglaubigter deutscher Übersetzung vorgelegt werden. Werden im Herkunftsstaat solche Unterlagen nicht ausgestellt, so können sie durch eine Versicherung an Eides statt des Gewerbetreibenden oder nach dem Recht des Herkunftsstaats vergleichbare Handlungen ersetzt werden.

2. Soweit in diesem Gesetz oder einer auf Grund dieses Gesetzes erlassenen Rechtsverordnung ein Nachweis darüber verlangt wird, dass ein Gewerbetreibender gegen die finanziellen Risiken seiner beruflichen Tätigkeit haftpflichtversichert ist, ist von Gewerbetreibenden aus einem anderen Mitgliedstaat der Europäischen Union oder einem anderen Vertragsstaat des Abkommens über den Europäischen Wirtschaftsraum als Nachweis eine Bescheinigung über den Abschluss einer Berufshaftpflichtversicherung als hinreichend anzuerkennen, die von einem Kreditinstitut oder einem Versicherungsunternehmen in einem anderen Mitgliedstaat oder Vertragsstaat ausgestellt wurde, sofern die in diesem Staat abgeschlossene Berufshaftpflichtversicherung im Wesentlichen vergleichbar ist zu der, die von Inländern verlangt wird, und zwar hinsichtlich der Zweckbestimmung, der vorgesehenen Deckung bezüglich des versicherten Risikos, der Versicherungssumme und möglicher Ausnahmen von der Deckung. Bei nur teilweiser Gleichwertigkeit kann eine zusätzliche Sicherheit verlangt werden, die die nicht gedeckten Risiken absichert.

3. Absatz 2 gilt nicht, soweit Tätigkeiten nach den §§ 30, 33c, 33d, 34, 34a, 34c Absatz 1 Satz 1 Nummer 1a bis 3, den §§ 34d, 34e oder nach § 60a ausgeübt werden."

                                            :en
"§ 13b recognition of foreign certificates and documents

1. Where under this Act or issued under this Act ordinance on the reliability or the financial circumstances should be considered a person, as evidence for the reliability and overall financial situation of traders from other Member States of the European Union or another signatory to the Agreement on the European Economic Area recognized as sufficient documentation issued in their home state and to demonstrate that the requirements for reliability and overall financial condition of the traders are. They can ask that the documents be submitted as certified copies and certified German translation. In the country of origin, such documents are not issued, it may by insurance under penalty of perjury under the laws of the trader or the home state similar acts to be replaced.

2. Where in this Act or issued under this Act ordinance, proof is required to sign that a business is insured against the financial risks of his profession is, of trader from another Member State of the European Union or another signatory to the Agreement on the European Economic Area must provide a certificate on completion of professional liability insurance to be sufficiently recognized, which was issued by a bank or an insurance company in another Member State or State Party, provided that the completed in this state professional liability insurance is essentially comparable to that of residents is required, in terms of purpose, which provided coverage with respect to the risk insured, the insured amount and possible exclusions from coverage. With only partial equivalence, additional security may be required, which shall cover the uninsured risks.

3. Paragraph 2 shall not apply where activities are carried out in accordance with § § 30, 33c, 33d, 34, 34a, 34c, paragraph 1, sentence 1, paragraph 1a to 3, with § § 34d, 34e or § 60a."})
       
       :schemes
       [(make-scheme
         :id 'pos-1584-0-1
         :header (make-metadata :title "Poz. 1584.0.1"
                                :description {:en ""})
         :conclusion '(low-or-early-advance-payment ?O)
         :premises [(pm '(advance-payment-percent ?O ?P))
                    (pm '(eval true (<= ?P 10)))])  ; less than 10%


        ])])]))

