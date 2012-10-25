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