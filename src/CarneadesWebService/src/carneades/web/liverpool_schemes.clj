;;; Copyright ? 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.web.liverpool-schemes
  (:use carneades.engine.dublin-core
        carneades.engine.argument
        carneades.engine.scheme))

;; To Do: Replace with Liverpool's argumentation schemes

(def liverpool-schemes 
    (make-theory
      :header 
      (make-metadata :title "Argumentation Schemes")
      
      :language 
      {'Tweety (make-individual :symbol 'Tweety :text {:en "Tweety"})
       'bird (make-predicate 
               :symbol 'bird 
               :arity 1
               :forms {:en (make-form :positive "%s is a bird."
                                      :negative "%s is not a bird."
                                      :question "Is %s a bird?")}),
       'flies (make-predicate 
                :symbol 'flies
                :arity 1
                :forms {:en (make-form :positive "%s flies."
                                       :negative "%s does not fly."
                                       :question "Does %s fly?")}),
       'penguin (make-predicate 
                  :symbol 'penguin
                  :arity 1
                  :forms {:en (make-form :positive "%s is a penguin."
                                         :negative "%s is not a penguin."
                                         :question "Is %s a penguin?")})}
      
      :schemes 
      [(make-scheme                            
         :id 'a
         :conclusion '(flies ?x)
         :premises [(make-premise :role "minor" :statement '(bird ?x))]
         :exceptions [(pm '(penguin ?x))])]))

(def liverpool-schemes-by-predicate
  (create-scheme-predicate-index {} liverpool-schemes))

(def liverpool-schemes-by-id
  (create-scheme-id-index {} liverpool-schemes))
