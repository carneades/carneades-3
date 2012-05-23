;;; Copyright 2012 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.examples.liverpool
    (:use carneades.engine.dublin-core
        carneades.engine.argument
        carneades.engine.scheme))

(def liverpool-schemes 
    (make-theory
      :header 
      (make-metadata :title "Reconstruction of Liverpool Argumentation Schemes"
                     :creator "Tom Gordon"
                     :publisher "Fraunhofer FOKUS"
                     :date "20121"
                     :description {:en "This is a reconstruction of the version of the Liverpool schemes in [@Atkison2012a]."})
      
      :language 
      {'should-do
       (make-predicate
         :symbol 'should-do
         :arity 2
         :forms {:en (make-form :positive "In cirumstances %s, we should do %s."
                                :negative "In circumstances %s, we should not do %s."
                                :question "In circumstances %s, should we do %s?")}),

       'results-in
       (make-predicate
         :symbol 'results-in
         :arity 3
         :forms {:en (make-form :positive "In circumstances %s doing %s would result in circumstances %s."
                                :negative "In circumstances %s doing %s would not result in circumstances %s."
                                :question "In circumstances %s would doing %s result in circumstances %s?")})

       'avoids
       (make-predicate
        :symbol 'avoids
        :arity 3
        :forms {:en (make-form :positive "In circumstances %s not doing %s would avoid circumstances %s."
                               :negative "In circumstances %s not doing %s would not avoid circumstances %s."
                               :question "In circumstances %s would not doing %s avoid circumstances %s?")})
                
    
       'realises
       (make-predicate
        :symbol 'realises
        :arity 2
        :forms {:en (make-form :positive "Circumstances %s would realize goal %s."
                               :negative "Circumstances %s would not realize goal %s."
                               :question "Would circumstances %s realize goal %s?")})

       'promotes 
       (make-predicate 
         :symbol 'promotes
         :arity 2
         :forms {:en (make-form :positive "Achieving %s would promote the value %s."
                                :negative "Achieving %s would not promote the value %s."
                                :question "Would achieving %s promote the value %s?")}),
       
       'demotes
       (make-predicate 
         :symbol 'demotes
         :arity 2
         :forms {:en (make-form :positive "Achieving goal %s would demote the value %s."
                                :negative "Achieving  goal %s would not demote the value %s."
                                :question "Would achieving goal %s demote the value %s?")}),


       ;;; Check whether the predicates below are still needed in this version of the model.
       
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
         :id 'pras1
         :header (make-metadata :title "Practical Reasoning Scheme")
         :conclusion '(should-do ?S1 ?A)
         :premises [(make-premise :role "action" :statement '(results-in ?S1 ?A ?S2))
                    (make-premise :role "goal" :statement '(realizes ?S2 ?G))
                    (make-premise :role "value" :statement '(promotes ?G ?V))])

       (make-scheme                            
         :id 'pras2
         :header (make-metadata :title "Negative Practical Reasoning Scheme")
         :conclusion '(not (should-do ?S1 ?A))
         :premises [(make-premise :role "action" :statement '(avoids ?S1 ?A ?S2))
                    (make-premise :role "goal" :statement '(realizes ?S2 ?G))
                    (make-premise :role "value" :statement '(demotes ?G ?V))])
       ] ; end of schemes
      
      :references 
      {"Atkison2012a"
       (make-metadata :creator "Katie Atkinson, Adam Wyner and Trevor Bench-Capon"
                      :title "Report No. D5.2 -- Report on Prototype 1 of Structured Consultation Tool"
                      :publisher "IMPACT Project"
                      :date "2012")}))
    
(def liverpool-schemes-by-predicate
  (create-scheme-predicate-index {} liverpool-schemes))

(def liverpool-schemes-by-id
  (create-scheme-id-index {} liverpool-schemes))


