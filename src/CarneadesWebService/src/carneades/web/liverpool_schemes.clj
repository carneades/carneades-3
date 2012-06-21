;;; Copyright ? 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.web.liverpool-schemes
  (:use carneades.engine.dublin-core
        carneades.engine.argument
        carneades.engine.scheme))


(def liverpool-schemes 
    (make-theory
      :header 
      (make-metadata :title "Liverpool Argumentation Schemes"
                     :creator "Adam Wyner, Katie Atkinson, Trevor Bench-Capon"
                     :publisher "University of Liverpool"
                     :date "2011")
      
      :language 
      {'compelling-reason
       (make-predicate
         :symbol 'compelling-reason
         :arity 2
         :forms {:en (make-form 
                       :positive "Doing %s would promote a value more important than %s."
                       :negative "Doing %s would not promote a value more important than %s."
                       :question "Would doing %s promote a value more important than %s?")})
       
       'consequence
       (make-predicate
         :symbol 'consequence
         :arity 2
         :forms {:en (make-form :positive "%s is a consequence of doing %s."
                                :negative "%s is not a consequence of doing %s."
                                :question "Is %s a consequence of doing %s?")})
       
       'demotes
       (make-predicate 
         :symbol 'demotes
         :arity 2
         :forms {:en (make-form :positive "Achieving %s would demote the value %s."
                                :negative "Achieving %s would not demote the value %s."
                                :question "Would achieving %s demote the value %s?")}),
       
       'has-goal
       (make-predicate
         :symbol 'has-goal
         :arity 2
         :forms {:en (make-form :positive "%s has goal %s."
                                :negative "%s does not have goal %s."
                                :question "Does %s have goal %s?")}),
       
      'has-value
       (make-predicate
         :symbol 'has-value
         :arity 2
         :forms {:en (make-form :positive "%s values %s."
                                :negative "%s does not value %s."
                                :question "Does %s value %s?")}),
       
       'preconditions-satisfied
       (make-predicate 
         :symbol 'preconditions-satisfied 
         :arity 1
         :forms {:en (make-form :positive "The preconditions of action %s are satisfied."
                                :negative "The preconditions of action %s are not satisfied."
                                :question "Are the preconditions of action %s satisfied?")}),
       
       
       'prefers-method
       (make-predicate
         :symbol 'prefers-method
         :arity 3
         :forms {:en (make-form :positive "%s prefers method %s to method %s."
                                :negative "%s does not prefer method %s to method %s."
                                :question "Does %s prefer mothod %s to method %s?")}),
       
       'prefers-state
       (make-predicate
         :symbol 'prefers-state
         :arity 3
         :forms {:en (make-form :positive "%s prefers state %s to state %s."
                                :negative "%s does not prefer state %s to state %s."
                                :question "Does %s prefer state %s to state %s?")}),
       
       'prevents
       (make-predicate
         :symbol 'prevents
         :arity 2
         :forms {:en (make-form :positive "Agent %s prevents consequence %s."
                                :negative "Agent %s does not prevent consequence %s."
                                :question "Does agent %s prevent consequence %s?")})
                                
     
       'promotes 
       (make-predicate 
         :symbol 'promotes
         :arity 2
         :forms {:en (make-form :positive "Achieving %s would promote the value %s."
                                :negative "Achieving %s would not promote the value %s."
                                :question "Would achieving %s promote the value %s?")}),
       
       'realises 
       (make-predicate 
         :symbol 'realises
         :arity 2
         :forms {:en (make-form :positive "Doing %s realises the consequences %s."
                                :negative "Doing %s does not realise %s."
                                :question "Would doing %s realise %s?")}),
       
       'should-do
       (make-predicate
         :symbol 'should-do
         :arity 2
         :forms {:en (make-form :positive "%s should do %s."
                                :negative "%s should not do %s."
                                :question "Should %s do %s?")}),
       
        'unacceptable
       (make-predicate
         :symbol 'unacceptable
         :arity 2
         :forms {:en (make-form :positive "%s has unacceptable side effect %s."
                                :negative "%s does not have unacceptable side effect %s."
                                :question "Does %s have unacceptable side effects %s?")}),
                

       'would-achieve
       (make-predicate
         :symbol 'would-achieve
         :arity 2
         :forms {:en (make-form :positive "Doing %s would achieve %s."
                                :negative "Doing %s would not achieve %s."
                                :question "Would doing %s achieve %s?")})
       
      } ; end of language
      
      :schemes 
      [(make-scheme                            
         :id 'argument-from-goal
         :header (make-metadata :title "Argument from Goal")
         :conclusion '(should-do ?Ag ?A)
         :premises [(make-premise :role "goal" :statement '(has-goal ?Ag ?G))
                    (make-premise :role "solution" :statement '(would-achieve ?A ?G))]
         :exceptions [(make-premise :role "unacceptable side effect" 
                                    :statement '(unacceptable ?A ?E))
                      (make-premise :role "preferred action" 
                                    :statement '(prefers-method ?Ag ?A2 ?A))
                      (make-premise :role "preferred goal" 
                                    :statement '(prefers-state ?Ag ?G2 ?G))])
       
       (make-scheme
         :id 'argument-from-value
         :header (make-metadata :title "Argument from Value")
         :conclusion '(has-goal ?Ag ?G)
         :premises [(make-premise :role "value" :statement '(has-value ?Ag ?V))
                    (make-premise :role "promotion" :statement '(promotes ?G ?V))])
       
       (make-scheme
         :id 'argument-from-consequence
         :header (make-metadata :title "Argument from Consequence")
         :conclusion '(would-achieve ?A ?G)
         :premises [(make-premise :role "preconditions" 
                                  :statement '(preconditions-satisfied ?A))
                    (make-premise :role "consequences" 
                                  :statement '(consequence ?G ?A))]
         :exceptions [(make-premise :role "other agents" 
                                   :statement '(prevents ?Ag ?G))])
       
       (make-scheme
         :id 'negative-practical-argument
         :header (make-metadata :title "Negative Practical Argument")
         :conclusion '(unacceptable ?A ?E)
         :premises [(make-premise :role "consequences" 
                                  :statement '(consequence ?G ?A)),
                    (make-premise :role "demotion"
                                  :statement '(demotes ?G ?V)),
                    (make-premise :role "value"
                                  :statement '(has-value ?Ag ?V))]
         :exceptions [(make-premise :role "other agents" 
                                    :statement '(prevents ?Ag ?G)),
                      (make-premise :role "compelling reason" 
                                    :statement '(compelling-reason ?A ?V))])
                    
                                                                                      
      ] ; end of schemes
      
      :references 
      {"Atkinson2007"
       (make-metadata :creator "Katie Atkinson, Trevor J.M. Bench-Capon"
                      :title "Practical reasoning as presumptive argumentation 
                              using action based alternating transition systems"
                      :publisher "Elsevier"
                      :date "2007"
                      :description {:en "Katie Atkinson, Trevor J. M. Bench-Capon: 
                                         Practical reasoning as presumptive argumentation
                                         using action based alternating transition systems.
                                         Artificial Intelligence 171(10-15): 855-874 (2007)"})
                                
       "Walton2008"
       (make-metadata :creator "Douglas Walton, Chris Reed, Fabrizio Macagno"
                      :title "Argumentation Schemes"
                      :publisher "Cambridge University Press"
                      :date "2008")}))
    
(def liverpool-schemes-by-predicate
  (create-scheme-predicate-index {} liverpool-schemes))

(def liverpool-schemes-by-id
  (create-scheme-id-index {} liverpool-schemes))
