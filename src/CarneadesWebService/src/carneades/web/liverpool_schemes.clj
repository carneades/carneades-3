;;; Copyright ? 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.web.liverpool-schemes
  (:use carneades.engine.dublin-core
        carneades.engine.argument
        carneades.engine.scheme))


(def liverpool-schemes 
    (make-theory
      :header 
      (make-metadata :title "Liverpool Argumentation Schemes")
      
      :language 
      {'circumstances
       (make-predicate 
               :symbol 'circumstances 
               :arity 1
               :forms {:en (make-form :positive "The current circumstances are %s."
                                      :negative "The current circumstances are not %s."
                                      :question "Is %s true in the current circumstances?")}),
       'realises (make-predicate 
                :symbol 'realises
                :arity 2
                :forms {:en (make-form :positive "Doing %s realises the consequences %s."
                                       :negative "Doing %s does not realise %s."
                                       :question "Would doing %s realise %s?")}),
       'promotes (make-predicate 
                  :symbol 'promotes
                  :arity 2
                  :forms {:en (make-form :positive "%s would promote the value %s."
                                         :negative "%s would not promote the value %s."
                                         :question "Would %s promote the value %s?")})}
      
      :schemes 
      [(make-scheme                            
         :id 'argument-for-action
         :header (make-metadata :title "Argument for Action")
         :conclusion '(should-do ?A)
         :premises [(make-premise :role "circumstances" :statement '(circumstances ?R))
                    (make-premise :role "goal" :statement '(realizes ?A ?G))
                    (make-premise :role "value" :statement '(promotes ?G ?V))])]))
    
(def liverpool-schemes-by-predicate
  (create-scheme-predicate-index {} liverpool-schemes))

(def liverpool-schemes-by-id
  (create-scheme-id-index {} liverpool-schemes))
