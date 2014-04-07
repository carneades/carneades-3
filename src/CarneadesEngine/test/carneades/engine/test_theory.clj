;; Copyright (c) 2010 Fraunhofer Gesellschaft
;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.


(ns carneades.engine.test-theory
  (:use carneades.engine.dublin-core
        carneades.engine.argument
        carneades.engine.argument-generator
        carneades.engine.theory
        carneades.engine.shell
        carneades.engine.aspic)
  (:require [midje.sweet :refer :all]))

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

(fact "Arguments can be constructed from a theory."
         (let [facts case1
               query '(flies ?x)]
           (expect (in? (ag facts query) '(flies Tweety)) => true)))
