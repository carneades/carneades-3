;;; Copyright ? 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.examples.practical-reasoning
  (:use carneades.engine.dublin-core
        carneades.engine.statement
        carneades.engine.argument
        carneades.engine.argument-generator
        carneades.engine.scheme
        carneades.engine.argument-graph
        carneades.engine.argument-construction
        carneades.maps.lacij
        clojure.pprint))

(def liverpool-schemes 
  (make-theory
   :header 
   (make-metadata 
    :title "Reconstruction of Liverpool Argumentation Schemes"
    :creator "Tom Gordon"
    :publisher "Fraunhofer FOKUS"
    :date "2012"
    :description {:en "This is a reconstruction of the version of 
		       the Liverpool schemes in [@Atkison2012a]."})

   
   :language 
   {'circumstances
    (make-predicate
     :symbol 'circumstances
     :arity 1
     :forms
     {:en (make-form
           :positive "The circumstances are %s."
           :negative "The circumstances are not %s."
           :question "Are the circumstances %s?")}) 
    
    'should-do
    (make-predicate
     :symbol 'should-do
     :arity 2
     :forms
     {:en (make-form 
           :positive "In cirumstances %s, we should do %s."
           :negative "In circumstances %s, we should not do %s."
           :question "In circumstances %s, should we do %s?")}),

    'results-in
    (make-predicate
     :symbol 'results-in
     :arity 3
     :forms
     {:en (make-form 
           :positive "In circumstances %s doing %s would
  		     result in circumstances %s."
           :negative "In circumstances %s doing %s would
                     not result in circumstances %s."
           :question "In circumstances %s would doing %s
                     result in circumstances %s?")})

    'avoids
    (make-predicate
     :symbol 'avoids
     :arity 3
     :forms
     {:en (make-form 
           :positive "In circumstances %s not doing %s
                     would avoid circumstances %s."
           :negative "In circumstances %s not doing %s
                     would not avoid circumstances %s."
           :question "In circumstances %s would not doing %s
                     avoid circumstances %s?")})

    'realises
    (make-predicate
     :symbol 'realises
     :arity 2
     :forms
     {:en (make-form 
           :positive "Circumstances %s would realize goal %s."
           :negative "Circumstances %s would not realize goal %s."
           :question "Would circumstances %s realize goal %s?")})

    'promotes 
    (make-predicate 
     :symbol 'promotes
     :arity 2
     :forms
     {:en (make-form 
           :positive "Achieving %s would promote the value %s."
           :negative "Achieving %s would not promote the value %s."
           :question "Would achieving %s promote the value %s?")}),

    'demotes
    (make-predicate 
     :symbol 'demotes
     :arity 2
     :forms
     {:en (make-form 
           :positive "Achieving goal %s would demote the
                     value %s."
           :negative "Achieving  goal %s would not demote the
                     value %s."
           :question "Would achieving goal %s demote the
                     value %s?")})

    }
   
   :schemes 
   [(make-scheme                            
     :id 'pras1
     :header (make-metadata :title "Practical Reasoning Scheme")
     :conclusion '(should-do ?S1 ?A)
     :premises
     [(make-premise :role "circumstances" :statement '(circumstances ?S1))
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
     [(make-premise :role "circumstances" :statement '(circumstances ?S1))
      (make-premise :role "action" :statement '(avoids ?S1 ?A ?S2))
      (make-premise :role "goal" :statement '(realizes ?S2 ?G))
      (make-premise :role "value" :statement '(demotes ?G ?V))])
    ]

   :references 
   {"Atkison2012a"
    (make-metadata 
     :creator "Katie Atkinson, Adam Wyner and Trevor Bench-Capon"
     :title "Report No. D5.2 -- Report on Prototype 1 of 
	     Structured Consultation Tool"
     :publisher "IMPACT"
     :date "2012")}))

(def case1
  [(make-statement 
    :atom 'a
    :text {:en "The law is clear as to whether libraries are legally 
  	       allowed to digitize and make Internet searchable
   	       those materials in their collections that the
	       copyright holders do not digitize and make searchable."})

   (make-statement
    :atom 'b
    :text {:en "Increased marketing for copyright holders' material."})

   ;; The next statement, c, is the goal of the argument

   (make-statement
    :atom 'c
    :text {:en "All material is digitised and made Internet searchable."})


   (make-statement 
    :atom '(circumstances (and (not a) (not b) (not c)))
    :text {:en "a) the law is unclear as to whether libraries are legally
	       allowed to digitise and make Internet searchable 
	       those materials in their collections that the 
	       copyright holders do not digitise and make searchable; 
	       b) No increased marketing for copyright holders' 
	       materials; and c) not all material is digitised and
	       made Internet searchable."}) 


   (make-statement
    :atom '(results-in (and (not a) (not b) (not c))
                       j1
                       (and a b c))
    :text {:en "If legislators clarify the law so that libraries are 
     	       able to digitise works they hold for the
	       purpose of making content Internet searchable, and
	       libraries digitise the works they hold and make them
	       Internet searchable, then a) the law will be clearer,
	       b) the copyright owner's would benefit from increased
	       marketing of their works, and c) more content would be
	       searchable on the Internet."})


   (make-statement
    :atom '(realizes (and a b c) c)
    :text {:en "If a) the law is clearer, b) copyright owners benefit
   	       from increased marketing of their works and c) more 
	       content is searchable on then Internet, then the goal
	       of making more content searchable on the Internet will
  	       have been achieved."})


   (make-statement
    :atom '(promotes c (and vs vi))
    :text {:en "Realizing the goal of making more content 
	       searchable on the Internet would promote the values
	       vs) open access to research and vi)  
	       balancing stakeholder interests."})])

(def ag1 (make-argument-graph))
(def issue '(should-do (and (not a) (not b) (not c)) ?A))

(def max-goals 100)  
(def generators  (list (generate-arguments-from-theory liverpool-schemes)))

(def ag2 (construct-arguments ag1 issue max-goals case1 generators))

(pprint ag2)
; (view ag2)

