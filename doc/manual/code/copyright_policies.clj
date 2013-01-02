(ns carneades.examples.copyright-policies
  (:use carneades.engine.dublin-core
        carneades.engine.scheme
        carneades.engine.argument))

(def copyright-policies
     (make-theory
      :header 
      (make-metadata :title "Copyright in the Knowledge Economy"
		     :description {:en ""}) ;; TODO add a description
      
      :language
      L

      :sections
      [(make-section
	:id 'Q12
	:main-issue '(may-publish the-person the-work)
	:header (make-metadata 
		 :title "Q12. Cross-Border Aspects of Orphaned Works"
		 
		 :description 
		 {:en "Question 12 of the Green Paper on Copyright in
		 the Knowledge Economy [@GreenPaper, p. 12] asks:

                 > (12) How should the cross-border aspects
                 > of the orphan works issue be tackled to
                 > ensure EU-wide recognition of the
                 > solutions adopted in different Member
                 > States?"})
	
	:sections  
	[german-copyright-law,
	 action-alliance-policy])]))
