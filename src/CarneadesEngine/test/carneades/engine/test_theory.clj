;;; Copyright ? 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1


(ns carneades.engine.test-theory
  (:use clojure.test
        carneades.engine.theory))

(def theory1 
  (make-theory
    :name "Theory of Birds"
    :language {'Tweety (make-individual :symbol 'Tweety :text {:en "Tweety"})
               'bird (make-predicate :symbol 'bird 
                                     :arity 1
                                     :forms {:en (make-form :positive "%s is a bird."
                                                            :negative "%s is not a bird."
                                                            :question "Is %s a bird?")}),
               'flies (make-predicate :symbol 'flies
                                      :arity 1
                                      :forms {:en (make-form :positive "%s flies."
                                                             :negative "%s does not fly."
                                                             :question "Does %s fly?")}),
               'penguin (make-predicate :symbol 'penguin
                                        :arity 1
                                        :forms {:en (make-form :positive "%s is a penguin."
                                                               :negative "%s is not a penguin."
                                                               :question "Is %s a penguin?")})}
    
    :schemes [(make-scheme :name "Birds Fly"
                           :id 's1
                           :clauses (make-clause :id 'a
                                                 :conclusions ['(flies ?x)]
                                                 :premises ['(bird ?x)]
                                                 :exceptions ['(penguin ?x)]))]))
                                
                                
                              
  
           