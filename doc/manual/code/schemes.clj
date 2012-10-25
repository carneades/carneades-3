(def S 
  [(make-scheme                            
    :id 'pras1
    :header (make-metadata :title "Practical Reasoning Scheme")
    :conclusion '(should-do ?S1 ?A)
    :premises
    [(make-premise :role "circumstances"
                   :statement '(circumstances ?S1))
     (make-premise :role "action" 
                   :statement '(results-in ?S1 ?A ?S2))
     (make-premise :role "goal" :statement '(realizes ?S2 ?G))
     (make-premise :role "value" :statement '(promotes ?G ?V))])

   (make-scheme                            
    :id 'pras2
    :header
    (make-metadata :title "Negative Practical Reasoning Scheme")
    :conclusion '(not (should-do ?S1 ?A))
    :premises
    [(make-premise :role "circumstances"
                   :statement '(circumstances ?S1))
     (make-premise :role "action" :statement '(avoids ?S1 ?A ?S2))
     (make-premise :role "goal" :statement '(realizes ?S2 ?G))
     (make-premise :role "value" :statement '(demotes ?G ?V))])
   ])
