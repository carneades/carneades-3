;;; Copyright (c) 2012 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.examples.german-family-law
  (:use carneades.engine.dublin-core
        carneades.engine.scheme
        carneades.engine.argument))

(def german-family-law
  (make-theory
   :header 
   (make-metadata :title "German Family Law"
                  :description {:en "Does a person have an obligation to a family member to provide support?"})
   
   :language
   {'family-relationship (make-individual :symbol 'family-relationship
					  :text {:en "family relationship"})

    'ancestor  
    (make-predicate
     :symbol 'ancestor
     :arity 2
     :forms {:en (make-form
                  :positive "%s is an ancestor of %s."
                  :negative "%s is not an ancestor of %s."
                  :question "Is %s an ancestor of %s?")}
     :hint {:en "An ancestor is a parent (mother or father) or an ancestor of a parent."}
     :widgets '[text text]
     ;; :answers '[[] []]
     :category 'family-relationship
     :followups ['descendent])

    'descendent  
    (make-predicate
     :symbol 'descendent
     :arity 2
     :forms {:en (make-form
                  :positive "%s is a descendent of %s."
                  :negative "%s is not a desendent of %s."
                  :question "Is %s a descendent of %s?")}
     :hint {:en "A descendent is a child (daughter or son, including adopted children) or a descendent of a child."}
     :widgets '[text text]
<<<<<<< HEAD
=======
     :followups ['ancestor]
>>>>>>> reformatted (pretty-printed) a couple of files
     ;; :answers '[[] []]
     :category 'family-relationship)

    'direct-lineage 
    (make-predicate
     :symbol 'direct-lineage
     :arity 2
     :forms {:en (make-form
                  :positive "%s in direct lineage with %s."
                  :negative "%s is not in direct lineage with %s."
                  :question "Is %s in direct lineage with %s?")})
    } ; language

   :sections
   [(make-section
     :main-issue '(direct-lineage ?P1 ?P2)
     :header (make-metadata :title "Direct Lineage"
                            :description {:en "Is someone in direct lineage with someone else."})
     :sections
     [(make-section
       :header (make-metadata :title "§1589 BGB (Direct Lineage)"
                              :description {:en "A relative is in direct lineage if he is a descendant or ancestor.  For example, parents, grandparents and great grandparents."})
       
       :schemes
       [
	(make-scheme                            
         :id 'BGB-1589a
	 :header (make-metadata :title "BGB-1589a" :description {:en "Foo"})
         :conclusion '(direct-lineage ?P1 ?P2)
         :premises [(pm '(ancestor ?P1 ?P2))])
	
	(make-scheme
	 :id 'BGB-1289b
	 :header (make-metadata :title "BGB-1589b" :description {:en "Bar"})
	 :conclusion '(direct-lineage ?P1 ?P2)
	 :premises [(pm '(descendent ?P1 ?P2))])

	;; (make-scheme
	;;  :id 'ancestor-and-descendent-are-inverse-a
	;;  :header (make-metadata :title "Inverse Relationship Between Ancestors and Descendents" :description {:en "The ancestor and descendent relations are inverse."})
	;;  :conclusion '(ancestor ?P1 ?P2)
	;;  :premises [(pm '(descendent ?P2 ?P1))])

	;; (make-scheme
	;;  :id 'ancestor-and-descendent-are-inverse-b
	;;  :header (make-metadata :title "Inverse Relationship Between Ancestors and Descendents" :description {:en "The ancestor and descendent relations are inverse."})
	;;  :conclusion '(descendent ?P1 ?P2)
	;;  :premises [(pm '(ancestor ?P2 ?P1))])

])])]))

