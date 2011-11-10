;;; Copyright ? 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1


(ns carneades.engine.test-theory
  (:use clojure.test
        carneades.engine.argument-generator
        carneades.engine.scheme))

(def theory1 
  (make-theory
    :name "Theory of Animals"
    :language {'Tweety (make-individual :symbol 'Tweety :text {:en "Tweety"})
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
    
    :sections [(make-section 
                 :name "Birds"
                 :id 's1
                 :schemes [(make-scheme                            
                            :id 'a
                            :name "Birds Fly"
                            :conclusions ['(flies ?x)]
                            :premises {"minor" '(bird ?x)}
                            :exceptions ['(penguin ?x)])])]))
                                
(def g (generate-arguments-from-theory theory1))
(generate g '(flies Tweety) {})
                              
  
           