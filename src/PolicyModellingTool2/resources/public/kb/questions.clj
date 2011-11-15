(ns resources.public.kb.questions
  (:use carneades.engine.scheme
        impact.web.logic.question))

(def questions
  {'hatAnspruchOeffentlicheZugaenglichmachung
   (make-question
    :symbol 'hatAnspruchOeffentlicheZugaenglichmachung
    :arity 2
    :forms {:en (make-form :positive "%s has the right to make %s available to the public."
                           :negative "%s has not the right to make %s available to the public."
                           :question "Has %s the right to make %s available to the public?")})

   'hatName
   (make-question
    :symbol 'hatName
    :arity 1
    :forms {:en (make-form :positive "The right owner is called %s."
                           :negative "The right owner is not called %s."
                           :question "What is the name of the right owner?")
            
            :de (make-form :positive "Der Reichtsinhaber heißt %s."
                           :negative "Der Reichtsinhaber heißt nicht %s."
                           :question "Wie heißt der Rechtsinhaber?")}
    :category {:en "Name"
               :machine "Name"
               }
    :hint {:en "Please enter the name of the rights owner."}
    :type "text"
    :followups ['betrifftWerk])

   'betrifftWerk
   (make-question
    :symbol 'betrifftWerk
    :arity 1
    :forms {:de (make-form :positive "Das betreffende Werk heißt %s."
                           :negative "Das betreffende Werk heißt nicht %s."
                           :question "Wie heißt das betroffene Werk?")
            :en (make-form :positive "The concerned orphaned work is named %s"
                           :negative "The concerned orphaned work is not named %s"
                           :question "What is the name of the concerned orphaned work?")}
    :type "text"
    :category {:en "Work" :machine "Work"})
   
   'zumZweck
   (make-question
    :symbol 'zumZweck
    :arity 2
    :forms {:de (make-form :positive "%s wird zum Zweck %s verwendet"
                           :negative "%s wird nicht zum Zweck %s verwendet"
                           :question "Zu welchem Zweck wollen sie das Werk %s verwenden?")
            :en (make-form :positive "%s will be used for a %s purpose"
                           :negative "%s will not be used for a %s purpose"
                           :question "For which purpose would like to use %s?")}
    :hint {:en "Please enter if you want to use the orphaned work for commercial purposes."}
    :category {:en "Purpose" :machine "Purpose"}
    :answers {:en ["commercial" "non-commercial"]
              :machine ["commercial" "non_commercial"]}
    :type "radio")

   'UrheberSuche
   (make-question
    :symbol 'UrheberSuche
    :arity 1
    :forms {:de (make-form :positive "Die Suche nach dem Urheber was %s."
                           :negative "Die Suche nach dem Urheber was nicht %s."
                           :question "Erfolgte eine Standard oder eine professionelle Suche?")
            :en (make-form :positive "The search for the right holder was %s"
                           :negative "The search for the right holder was not %s"
                           :question "Is the search that has been carried out a professional one or a standard search?")}
    :hint {:en "Please enter if your documented search was a professional one."}
    :category {:en "Search" :machine "Search"}
    :answers {:en ["Standard" "Professional" "None"]
              :machine ["Standard" "Professional" "None"]}
    :type "select"
    :followups ['Bekanntmachung])

   'Bekanntmachung
   (make-question
    :symbol 'Bekanntmachung
    :arity 1
    :forms {:en (make-form :positive "A public annoucement was carried out."
                           :negative "No public annoucement was carried out."
                           :question "Did you announce the search?")

            :de (make-form :question "Erfolgte eine Bekanntmachung der Suche?")}
    :category {:de "Bekanntmachung" :machine "Bekanntmachung"}
    :answers {:en ["No" "Public"]
              :machine ["No" "Public"]}
    :type "radio")
   })
