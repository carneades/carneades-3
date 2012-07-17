;;; Copyright ? 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1


(ns carneades.engine.test-theory
  (:use clojure.test
        carneades.engine.dublin-core
        carneades.engine.argument
        carneades.engine.argument-generator
        carneades.engine.scheme
        carneades.engine.shell
        carneades.engine.aspic))

(def theory1 
  (make-theory
    :header 
    (make-metadata :title "Theory of Animals")
    
    :language
    (make-language
     (make-individual :symbol 'Tweety :text {:en "Tweety"})
     (make-predicate 
      :symbol 'bird 
      :arity 1
      :forms {:en (make-form :positive "%s is a bird."
                             :negative "%s is not a bird."
                             :question "Is %s a bird?")})
     (make-predicate 
      :symbol 'flies
      :arity 1
      :forms {:en (make-form :positive "%s flies."
                             :negative "%s does not fly."
                             :question "Does %s fly?")})
     (make-predicate 
      :symbol 'penguin
      :arity 1
      :forms {:en (make-form :positive "%s is a penguin."
                             :negative "%s is not a penguin."
                             :question "Is %s a penguin?")}))
    
    :schemes 
    [(make-scheme                            
       :id 'a
       :conclusion '(flies ?x)
       :premises [(make-premise :role "minor" :statement '(bird ?x))]
       :exceptions [(pm '(penguin ?x))])]))
                                
(def max-goals 10)  
(def generators (list (generate-arguments-from-theory theory1)))                  
(def case1 ['(bird Tweety)])

(defn ag [facts query]  ;
  "(seq-of literal) literal -> argument-graph
   construct and evaluate an argument graph"
  (argue (make-engine max-goals facts generators)
         aspic-grounded
         query))
                                   
(deftest test-theory
         (let [facts case1
               query '(flies ?x)]
           (is (in? (ag facts query) '(flies Tweety)))))

