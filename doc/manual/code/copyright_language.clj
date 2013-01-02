(def L
     (make-language 
      (make-individual :symbol 'the-person 
		       :text {:en "the person" :de "die Person"})
      (make-individual :symbol 'the-work 
		       :text {:en "the work" :de "das Werk"})
      (make-individual :symbol 'commercial 
		       :text {:en "commercial"})
      (make-individual :symbol 'purpose 
		       :text {:en "Purpose"})
      (make-individual :symbol 'identifiers 
		       :text {:en "Identifiers"})
      (make-individual :symbol 'non-commercial 
		       :text {:en "non-commercial"})
      (make-individual :symbol 'none 
		       :text {:en "None"})
      (make-individual :symbol 'professional 
		       :text {:en "professional documented search"})
      (make-individual :symbol 'search 
		       :text {:en "Search"})
      (make-individual :symbol 'standard 
		       :text {:en "standard documented search"})
      (make-individual :symbol 'license 
		       :text {:en "License"})

      (make-function
       :symbol 'the-search
       :arity 2
       :text {:en "the search by %s for the owner of %s"})

      (make-function
       :symbol 'the-use
       :arity 2
       :text {:en "the use by %s of %s"})
      
      (make-role
       :category 'license
       :symbol 'license-to-publish
       :min 0
       :max nil
       :type :symbol
       :askable true
       :forms {:en (make-form :positive "%s has a license to publish %s."
			      :negative "%s does not have a license to publish %s."
			      :question "Does %s have a license to publish %s?")}
       :hint {:en "Information about an existing license."}
       :followups '[])
      
      (make-role
       :symbol 'may-publish
       :min 0
       :max nil
       :type :symbol
       :askable false
       :forms {:en (make-form :positive "%s may publish  %s."
			      :negative "%s may not publish %s."
			      :question "May %s publish %s?")})

      (make-concept
       :symbol 'person
       :askable true
       :forms {:en (make-form :positive "%s is a person."
			      :negative "%s is not a person."
			      :question "Is %s a person?")
	       
	       :de (make-form :positive "%s ist ein Rechtsperson."
			      :negative "%s ist nicht ein Rechtsperson."
			      :question "Ist %s ein Rechtsperson?")}
       :category 'identifiers
       :hint {:en "Please provide an identifier for the person 
                   interested in publishing the work, such as P1."}
       :followups '[work])

      (make-concept
       :symbol 'work
       :askable true
       :forms {:de (make-form :positive "%s ist ein Werk."
			      :negative "%s is nicht ein Werk."
			      :question "Ist %s ein Werk?")
	       :en (make-form :positive "%s is a work."
			      :negative "%s is not a work."
			      :question "Is %s a work?")}
       :hint {:en "Please provide an identifier for the 
                   orphaned work, such as W1."}
       :category 'identifiers)
      
      (make-role
       :symbol 'type-of-use
       :askable true
       :min 1
       :max 1
       :type '#{non-commercial commercial}
       :forms {:de (make-form :positive "%s ist für folgender Zwecken: %s."
			      :negative "%s ist nicht für folgender Zwecken: %s."
			      :question "Ist %s für folgender Zwecken: %s?")
	       :en (make-form :positive "%s is for %s purposes."
			      :negative "%s is not for %s purposes."
			      :question "Is %s for %s purposes?")}
       :hint {:en "Will the work be used for commercial or 
                   non-commercial purposes?"}
       :category 'purpose)

      (make-role
       :symbol 'search-type
       :askable true
       :min 1
       :max 1
       :type '#{standard professional none}
       :forms {:en (make-form :positive "The type of %s was a %s."
			      :negative "The type of %s was not a %s."
			      :question "Was the type of %s a %s?")}
       :hint {:en "What type of search was performed to 
                   try to find the copyright owner?"}
       :category 'search
       :next ['announcement])

      (make-concept
       :symbol 'announcement
       :askable true
       :hint {:en "Information about an announcement."}
       :forms {:en (make-form :positive "The %s was publically announced."
			      :negative "The %s was not publically announced."
			      :question "Was the %s publically announced?")}
       :category 'search)
      
      (make-concept
       :symbol 'valid
       :askable false
       :form {:en (make-form :positive "%s is valid law."
			     :negative "%s is not valid law."
			     :question "Is %s valid law?")
	      :de (make-form :positive "%s is gültiges Recht."
			     :negative "%s ist nicht gültiges Recht."
			     :question "Ist %s gültiges Recht?")})))