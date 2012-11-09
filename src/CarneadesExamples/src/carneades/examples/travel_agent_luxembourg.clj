;;; Copyright (c) 2012 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.examples.travel-agent-luxembourg
  (:use carneades.engine.dublin-core
        carneades.engine.scheme
        carneades.engine.argument))

(def tour-operator-business-permit
  (make-theory
   :header 
   (make-metadata :title "Luxembourg Tour Operator Business Permit"
                  :description {:en "Are the requirments for opening a
                  business as a tour operator in Luxembourg met?"})
   
   :language
   (make-language 
    (make-individual :symbol 'qualifications :text {:en "Qualifications"})
    (make-individual :symbol 'general :text {:en "General Information"})
    (make-predicate
     :symbol 'business-responsible
     :arity 2
     :forms {:en (make-form
                  :positive "%s is the manager of %s."
                  :negative "%s is not the manager of %s."
                  :question "Is %s the manager of %s?")}
     :category 'general
     :widgets '[text text])

    (make-predicate
     :symbol 'professional-integrity
     :arity 1
     :forms {:en (make-form
                  :positive "The professional integrity of %s is proven."
                  :negative "The professional integrity of %s is not proven."
                  :question "Is the professional integrity of %s proven?")}
     :category 'general
     :widgets '[text])

    (make-predicate
     :symbol 'liability-insurance
     :arity 1
     :forms {:en (make-form
                  :positive "The applicant %s presented a copy of a
                  quote for professional liability insurance."
                  :negative "The applicant %s did not present a copy
                  of a quote for professional liability insurance."
                  :question "Did the applicant %s present a copy of a
                  quote for professional liability insurance?")}
     :category 'general
     :widgets '[text])

    (make-predicate
     :symbol 'insurance-against-financial-insolvency
     :arity 1
     :forms {:en (make-form
                  :positive "The applicant %s presented a copy of a
                 quote for a professional liability insurance for
                 travel agencies."
                  :negative "The applicant %s did not present a copy of
                 a quote for a professional liability insurance for
                 travel agencies."
                  :question "Did the applicant %s presented a copy of a
                 quote for a professional liability insurance for
                 travel agencies?")}
     :category 'general
     :widgets '[text])

    (make-predicate
     :symbol 'fixed-physical-establishment-lux
     :arity 1
     :forms {:en (make-form
                  :positive "The applicant %s has a fixed physical
                 establishment in Luxembourg (not a so-called
                 'letterbox' company)."
                  :negative "The applicant %s does not have a fixed
                 physical establishment in Luxembourg (is a so-called
                 'letterbox' company)."
                  :question "Does the applicant %s have a fixed physical
                 establishment in Luxembourg (not a so-called
                 'letterbox' company)?")}
     :category 'general
     :widgets '[text])

    (make-predicate
     :symbol 'vocational-diploma-similar-or-higher
     :arity 1
     :forms {:en (make-form
                  :positive "The manager %s has either a vocational
                 diploma (diplôme d'aptitude professionnelle - DAP) or
                 similar (vocational skills certificate - CATP, CAP,
                 etc.) or higher (secondary school graduation diploma,
                 Bachelor's degree, etc.)."
                  :negative "The manager %s does not have either a vocational
                 diploma (diplôme d'aptitude professionnelle - DAP) or
                 similar (vocational skills certificate - CATP, CAP,
                 etc.) or higher (secondary school graduation diploma,
                 Bachelor's degree, etc.)."
                  :question "Has the manager %s either a vocational
                 diploma (diplôme d'aptitude professionnelle - DAP) or
                 similar (vocational skills certificate - CATP, CAP,
                 etc.) or higher (secondary school graduation diploma,
                 Bachelor's degree, etc.)?")}
     :category 'qualifications
     :widgets '[text])

    (make-predicate
     :symbol 'professional-experience
     :arity 1
     :forms {:en (make-form
                  :positive "The manager %s has at least three years of
                 professional experience in an EU Member State."
                  :negative  "The manager %s does not have at least three years of
                 professional experience in an EU Member State."
                  :question "Does the manager %s have at least three years of
                 professional experience in an EU Member State?")}
     :category 'qualifications
     :widgets '[text])

    (make-predicate
     :symbol 'LSC-course
     :arity 1
     :forms {:en (make-form
                  :positive "The manager %s has a certificate of
                 successful completion of the final exam terminating
                 the course for access to the profession of trader
                 organized by the Luxembourg School for Commerce (LSC)
                 or similar training given in another EU Member
                 State."
                  :negative "The manager %s does not have a certificate
                 of successful completion of the final exam
                 terminating the course for access to the profession
                 of trader organized by the Luxembourg School for
                 Commerce (LSC) or similar training given in another
                 EU Member State."
                  :question "Has manager %s a certificate of successful
                 completion of the final exam terminating the course
                 for access to the profession of trader organised by
                 the Luxembourg School for Commerce (LSC) or any
                 similar training given in another EU Member State?")}
     :category 'qualifications
     :widgets '[text])

    (make-predicate
     :symbol 'previous-business-permit
     :arity 1
     :forms {:en (make-form
                  :positive "Manager %s has a copy of a previously held business permit."
                  :negative "Manager %s does not have a copy of a previously held business permit."
                  :question "Has manager %s a copy of a previously held business permit?")}
     :category 'qualifications
     :widgets '[text])

    (make-predicate
     :symbol 'travel-agent-business-permit
     :arity 1
     :forms {:en (make-form
                  :positive "%s is elgible for a travel agent business permit in Luxembourg."
                  :negative "%s is not elgible for a travel agent business permit in Luxembourg."
                  :question "Is %s elgible for a travel agent business permit in Luxembourg?")})

    (make-predicate
     :symbol 'qualification
     :arity 1
     :forms {:en (make-form
                  :positive "The manager %s has the required qualification."
                  :negative "The manager %s does not have the required qualification."
                  :question "Does the manager %s have the required qualification?")})
    
    )  ; end of language

   :sections
   [(make-section
     :main-issue '(travel-agent-business-permit ?A)
     :id 'travel-agent-business-permit
     :header (make-metadata :title "Requirements for Tour Operator
                                    Business Permits"
                            :description {:en "<description goes here>"})
     :sections
     [(make-section
       :header (make-metadata
                :title "Luxembourg Tour Operator Business Permit
       Requirements"
                :description {:en "<description goes here>"})
       :schemes
       [(make-scheme
         :id 'travel-agent-business-permit-1
         :header (make-metadata :title "Lux 1"
                                :description {:en ""})
         :conclusion '(travel-agent-business-permit ?A)
         :premises [(pm '(business-responsible ?M ?A))
                    (pm '(qualification ?M))           
                    (pm '(professional-integrity ?M))
                    (pm '(liability-insurance ?A))
                    (pm '(insurance-against-financial-insolvency ?A))
                    (pm '(fixed-physical-establishment-lux ?A))])

        (make-scheme
         :id 'qualification-1
         :header (make-metadata :title "Lux 2a"
                                :description {:en ""})
         :conclusion '(qualification ?M)
         :premises [(pm '(vocational-diploma-similar-or-higher ?M))])

        (make-scheme
         :id 'qualification-2
         :header (make-metadata :title "Lux 2b"
                                :description {:en ""})
         :conclusion '(qualification ?M)
         :premises [(pm '(professional-experience ?M))])

        (make-scheme
         :id 'qualification-3
         :header (make-metadata :title "Lux 2c"
                                :description {:en ""})
         :conclusion '(qualification ?M)
         :premises [(pm '(LSC-course ?M))])

        (make-scheme
         :id 'qualification-4
         :header (make-metadata :title "Lux 2d"
                                :description {:en ""})
         :conclusion '(qualification ?M)
         :premises [(pm '(previous-business-permit ?M))])])])]))



