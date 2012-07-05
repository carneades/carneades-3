;;; Copyright (c) 2012 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.examples.german-family-law
  (:use carneades.engine.dublin-core
        carneades.engine.scheme
        carneades.engine.argument))

(def german-family-law
  (make-theory
   :header 
   (make-metadata :title "German Family Law"
                  :description {:en "Does a person have an obligation to a family member to provide support?"})
   
   :language
   {'world-wide (make-individual :symbol 'world-wide
                                 :text {:en "world-wide"})
    'european (make-individual :symbol 'european
                               :text {:en "European"})
    'Polish (make-individual :symbol 'Polish
                             :text {:en "Polish"})

    'scope (make-individual :symbol 'scope
                            :text {:en "scope"})

    'income (make-individual :symbol 'income :text {:en "income"})
    'advance-payments (make-individual :symbol 'advance-payments :text {:en "advance payments"})
    'payment-time (make-individual :symbol 'payment-time :text {:en "payment time"})

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

    'annual-income
    (make-predicate
     :symbol 'annual-income
     :arity 2
     :forms {:en (make-form
                  :positive "The tour operator, %s, has an annual income of %s euros."
                  :negative "The tour operator, %s, does not have an annual income of %s euros."
                  :question "Does the tour operator, %s, have an annual income of %s euros?")}
     :hint {:en "What is the annual income of the tour operator?"}
     :widgets '[text text]
     :category 'income)

    'advance-payment-percent
    (make-predicate
     :symbol 'advance-payment-percent
     :arity 2
     :forms {:en (make-form
                  :positive "The tour operator, %s, receives advance payments of %s percent."
                  :negative "The tour operator, %s, does not receive advance payments of %s percent."
                  :question "Does the tour operator, %s, receive advance payments of %s percent?")}
     :hint {:en "What is the percent of the price of tours is paid in advance?"}
     :widgets '[text text]
     :category 'advance-payments)

    'advance-payment-time
    (make-predicate
     :symbol 'advance-payment-time
     :arity 2
     :forms {:en (make-form
                  :positive "The tour operator, %s, receives advance payments %s days before the start of the package holiday or travel services."
                  :negative "The tour operator, %s, does not receive advance payments %s days before the start of the package holiday or travel services."
                  :question "Does the tour operator, %s, receive advance payments %s days before the start of the package holiday or travel services?")}
     :hint {:en "When does the tour operator receive advance payments?"}
     :widgets '[[text][text]]
     :category 'payment-time)

    'minimum-guarantee
    (make-predicate
     :symbol 'minimum-guarantee
     :arity 2
     :forms {:en (make-form
                  :positive "The minimal guarantee sum for the tour operator, %s, is %s euros."
                  :negative "The minimal guarantee sum for the tour operator, %s, is not %s euros."
                  :question "Is the minimal guarantee sum for the tour operator, %s, %s euros?")})

    ;; TODO: missing widgets definition?
    'low-or-early-advance-payment
    (make-predicate
     :symbol 'low-or-early-advance-payment
     :arity 1
     :forms {:en (make-form
                  :positive "The tour operator, %s, receives low or early advance payments."
                  :negative "The tour operator, %s, does not receive early or low advance payments."
                  :question "Does the tour operator, %s, receive low or early advance payments?")})}

   :sections
   [(make-section
     :main-issue '(minimum-guarantee ?O ?G)
     :id 'minimum-guarantee
     :header (make-metadata :title "Minimum Guarantee Requirements for Tour Operators"
                            :description {:en "Minimum Guarantee Requirements for Tour Operator"})
     :sections
     [(make-section
       :header (make-metadata :title "Polish Minimum Guarantee Requirements for Tour Operators"
                              :description {:en "Ministry of Finance on 16 December  2010 at act 1584 published regulation for minimum sum of a bank guarantee or insurance required in respect of activities
exercised by tour operators and travel agents. The level of minimum sum of
guarantee depends on three different scope of activities:

1. Whole world organization of tourist events and liaising on behalf of
clients in contracting for the provision of tourism services in the territory
of non-European countries regardless of the mode and the territory of the
European countries with the use of air transport in the transport, charter,
with the exception of point 3

2. Europe organization of tourist events and liaising on behalf of clients in
contracting for the provision of tourism services in the territory of the
European countries with the use of other means of transport other than
air transport in the transport, charter, with the exception of point 3;

3. Within Polish land borders organization of tourist events and liaising
on behalf of clients for the provision of tourism services in the territory
of countries with a land border with the Polish Republic and the Russian
Federation, within the area of the Kaliningrad region, and to organize
domestic tours and liaising on behalf of clients in the conclusion of
national provision of tourism services, including the events of foreign
tourism.

Regulation introduce annual income for tour operator as:  there is
only income for the organization of tourist events for customers in the
closed financial year preceding the year of submission of the successive
contracts of a bank guarantee or insurance.  The minimum sum  of guarantee
for tour operator does not receiving payments or receiving advance payment
within 30 days before the start of package holiday or travel services or
receiving advance payment of up to 10% of the holiday or travel services,
are:

1. 12.0% of annual income from the activity referred to point 1, but not
less than the equivalent of 40,000 EUR;

2. 7.0% of annual income from the activity referred to point 2, but not
less than the equivalent of 7,500 EUR;

3. 3.0% of annual income from the activity referred to point 3, but not less
than the equivalent of 4,500 EUR.

The minimum guarantee sum for tour operator receiving advance payment
before the start of package holiday or travel services or receiving advance
payment of above 10% to 30% of the holiday or travel services, are:

1. Over 30 days to 90 days before the start of your holiday or travel
services, are:

    a. 12.5% of annual income from the activity, but not less than the
equivalent of 42,000 EUR,

    b.	7.5% of annual income from the activity, but not less than the
equivalent of 8,000 EUR,

    c.	3.5% of annual income from the activity, but not less than the
equivalent of 5,500 EUR;

2. Over 90 days to 180 days before the start of your holiday or travel
services, are:

    a.	13.0% of annual income from the activity, but not less than the
equivalent of 44,000 EUR,

    b.	8.0% of annual income from the activity, but not less than the
equivalent of 8,500 EUR,

    c.	3.5% of annual income from the activity, but not less than the
equivalent of 5,500 EUR;

3. Over 180 days before the start of your holiday or travel services,
are:

    a.	14.0% of annual income from the activity, but not less than the
equivalent of 47,000 EUR,

    b.	9.0% of annual income from the activity, but not less than the
equivalent of 10,000 EUR,

    c.	4.0% of annual income from the activity, but not less than the
equivalent of 6,000 EUR.
"})
       
       :schemes
       [(make-scheme
         :id 'pos-1584-0-1
         :header (make-metadata :title "Poz. 1584.0.1"
                                :description {:en ""})
         :conclusion '(low-or-early-advance-payment ?O)
         :premises [(pm '(advance-payment-percent ?O ?P))
                    (pm '(eval true (<= ?P 10)))])  ; less than 10%

        (make-scheme
         :id 'pos-1584-0-2
         :header (make-metadata :title "Poz. 1584.0.2"
                                :description {:en ""})
         :conclusion '(low-or-early-advance-payment ?O)
         :premises [(pm '(advance-payment-time ?O ?T))
                    (pm '(eval true (<= ?T 30)))])  ; within 30 days
        
        
        (make-scheme                            
         :id 'poz-1584-1-1
         :header (make-metadata :title "Poz. 1584.1.1"
                                :description {:en ""})
         :conclusion '(minimum-guarantee ?O ?G)
         :premises [(pm '(low-or-early-advance-payment ?O))
                    (pm '(scope-of-activities ?O world-wide))
                    (pm '(annual-income ?O ?I))
                    (pm '(eval ?G (let [x (float (* ?I 0.12)) ; 12%
                                        min 40000] ; 12%
                                    (if (< x min) min x))))])

        (make-scheme                            
         :id 'poz-1584-1-2
         :header (make-metadata :title "Poz. 1584.1.2"
                                :description {:en ""})
         :conclusion '(minimum-guarantee ?O ?G)
         :premises [(pm '(low-or-early-advance-payment ?O))
                    (pm '(scope-of-activities ?O European))
                    (pm '(annual-income ?O ?I))
                    (pm '(eval ?G (let [x (float (* ?I 0.07)) ; 7%
                                        min 7500] 
                                    (if (< x min) min x))))
                    ])

        (make-scheme                            
         :id 'poz-1584-1-3
         :header (make-metadata :title "Poz. 1584.1.3"
                                :description {:en ""})
         :conclusion '(minimum-guarantee ?O ?G)
         :premises [(pm '(low-or-early-advance-payment ?O))
                    (pm '(scope-of-activities ?O Polish))
                    (pm '(annual-income ?O ?I))
                    (pm '(eval ?G (let [x (float (* ?I 0.3)) ; 3%
                                        min 4500] 
                                    (if (< x min) min x))))])

        (make-scheme                            
         :id 'poz-1584-2-1-a
         :header (make-metadata :title "Poz. 1584.2.1a"
                                :description {:en ""})
         :conclusion '(minimum-guarantee ?O ?G)
         :premises [(pm '(advance-payment-percent ?O ?P))
                    (pm '(eval true (and (> ?P 10) (<= ?P 30))))
                    (pm '(advance-payment-time ?O ?T))
                    (pm '(eval true (and (> ?T 30) (<= ?T 90))))
                    (pm '(scope-of-activities ?O world-wide))
                    (pm '(annual-income ?O ?I))
                    (pm '(eval ?G (let [x (float (* ?I 0.125)) ; 12.5%
                                        min 42000] 
                                    (if (< x min) min x))))])

        (make-scheme                            
         :id 'poz-1584-2-1-b
         :header (make-metadata :title "Poz. 1584.2.1b"
                                :description {:en ""})
         :conclusion '(minimum-guarantee ?O ?G)
         :premises [(pm '(advance-payment-percent ?O ?P))
                    (pm '(eval true (and (> ?P 10) (<= ?P 30))))
                    (pm '(advance-payment-time ?O ?T))
                    (pm '(eval true (and (> ?T 30) (<= ?T 90))))
                    (pm '(scope-of-activities ?O European))
                    (pm '(annual-income ?O ?I))
                    (pm '(eval ?G (let [x (float (* ?I 0.075)) ; 7.5%
                                        min 8000] 
                                    (if (< x min) min x))))])

        (make-scheme                            
         :id 'poz-1584-2-1-c
         :header (make-metadata :title "Poz. 1584.2.1c"
                                :description {:en ""})
         :conclusion '(minimum-guarantee ?O ?G)
         :premises [(pm '(advance-payment-percent ?O ?P))
                    (pm '(eval true (and (> ?P 10) (<= ?P 30))))
                    (pm '(advance-payment-time ?O ?T))
                    (pm '(eval true (and (> ?T 30) (<= ?T 90))))
                    (pm '(scope-of-activities ?O Polish))
                    (pm '(annual-income ?O ?I))
                    (pm '(eval ?G (let [x (float (* ?I 0.035)) ; 3.5%
                                        min 5500] 
                                    (if (< x min) min x))))])

        (make-scheme                            
         :id 'poz-1584-2-2-a
         :header (make-metadata :title "Poz. 1584.2.2a"
                                :description {:en ""})
         :conclusion '(minimum-guarantee ?O ?G)
         :premises [(pm '(advance-payment-percent ?O ?P))
                    (pm '(eval true (and (> ?P 10) (<= ?P 30))))
                    (pm '(advance-payment-time ?O ?T))
                    (pm '(eval true (and (> ?T 90) (<= ?T 180))))
                    (pm '(scope-of-activities ?O world-wide))
                    (pm '(annual-income ?O ?I))
                    (pm '(eval ?G (let [x (float (* ?I 0.13)) ; 13%
                                        min 44000] 
                                    (if (< x min) min x))))])

        (make-scheme                            
         :id 'poz-1584-2-2-b
         :header (make-metadata :title "Poz. 1584.2.2b"
                                :description {:en ""})
         :conclusion '(minimum-guarantee ?O ?G)
         :premises [(pm '(advance-payment-percent ?O ?P))
                    (pm '(eval true (and (> ?P 10) (<= ?P 30))))
                    (pm '(advance-payment-time ?O ?T))
                    (pm '(eval true (and (> ?T 90) (<= ?T 180))))
                    (pm '(scope-of-activities ?O European))
                    (pm '(annual-income ?O ?I))
                    (pm '(eval ?G (let [x (float (* ?I 0.08)) ; 8%
                                        min 8500] 
                                    (if (< x min) min x))))])

        
        (make-scheme                            
         :id 'poz-1584-2-2-c
         :header (make-metadata :title "Poz. 1584.2.2c"
                                :description {:en ""})
         :conclusion '(minimum-guarantee ?O ?G)
         :premises [(pm '(advance-payment-percent ?O ?P))
                    (pm '(eval true (and (> ?P 10) (<= ?P 30))))
                    (pm '(advance-payment-time ?O ?T))
                    (pm '(eval true (and (> ?T 90) (<= ?T 180))))
                    (pm '(scope-of-activities ?O Polish))
                    (pm '(annual-income ?O ?I))
                    (pm '(eval ?G (let [x (float (* ?I 0.035)) ; 3.5%
                                        min 5500] 
                                    (if (< x min) min x))))])

        (make-scheme                            
         :id 'poz-1584-2-3-a
         :header (make-metadata :title "Poz. 1584.2.3a"
                                :description {:en ""})
         :conclusion '(minimum-guarantee ?O ?G)
         :premises [(pm '(advance-payment-percent ?O ?P))
                    (pm '(eval true (and (> ?P 10) (<= ?P 30))))
                    (pm '(advance-payment-time ?O ?T))
                    (pm '(eval true (> ?T 180)))
                    (pm '(scope-of-activities ?O world-wide))
                    (pm '(annual-income ?O ?I))
                    (pm '(eval ?G (let [x (float (* ?I 0.14)) ; 14%
                                        min 47000] 
                                    (if (< x min) min x))))])

        (make-scheme                            
         :id 'poz-1584-2-3-b
         :header (make-metadata :title "Poz. 1584.2.3b"
                                :description {:en ""})
         :conclusion '(minimum-guarantee ?O ?G)
         :premises [(pm '(advance-payment-percent ?O ?P))
                    (pm '(eval true (and (> ?P 10) (<= ?P 30))))
                    (pm '(advance-payment-time ?O ?T))
                    (pm '(eval true (> ?T 180)))
                    (pm '(scope-of-activities ?O European))
                    (pm '(annual-income ?O ?I))
                    (pm '(eval ?G (let [x (float (* ?I 0.09)) ; 9%
                                        min 10000] 
                                    (if (< x min) min x))))])

        (make-scheme                            
         :id 'poz-1584-2-3-b
         :header (make-metadata :title "Poz. 1584.2.3b"
                                :description {:en ""})
         :conclusion '(minimum-guarantee ?O ?G)
         :premises [(pm '(advance-payment-percent ?O ?P))
                    (pm '(eval true (and (> ?P 10) (<= ?P 30))))
                    (pm '(advance-payment-time ?O ?T))
                    (pm '(eval true (> ?T 180)))
                    (pm '(scope-of-activities ?O Polish))
                    (pm '(annual-income ?O ?I))
                    (pm '(eval ?G (let [x (float (* ?I 0.04)) ; 4%
                                        min 6000] 
                                    (if (< x min) min x))))]) 



        ])])]))

