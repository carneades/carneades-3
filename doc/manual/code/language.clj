(def L
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

   })