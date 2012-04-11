;;; Copyright (c) 2010 Fraunhofer Gesellschaft 
;;; Licensed under the EUPL V.1.1

(ns carneades.examples.consumer-contracts
  (:use clojure.test
        carneades.engine.shell
        carneades.engine.argument
        carneades.engine.dublin-core
        carneades.engine.scheme
        carneades.engine.aspic))
                  
(def theory1
  (make-theory
    :sections 
    [(make-section 
       :header (make-metadata :title "Art1341" :creator "Marcello")
       :schemes 
       [(make-scheme                            
          :header (make-metadata :title "Art1341co1" :creator "Marcello")
          :conclusion '(Relevant-ExArt1341co1 ?x)
          :premises [(pm '(applies ?x General))
                     (pm '(applies ?x Unilateral))
                     (pm '(applies ?x Not-Knowledgeable))])
        
        (make-scheme                            
          :header (make-metadata :title "Art1341co2" :creator "Marcello")
          :conclusion '(Relevant-ExArt1341co2 ?x)
          :premises [(pm '(applies ?x ?y))
                     (pm '(Oppressive-Status ?y)) 
                     (pm '(applies ?x General))
                     (pm '(applies ?x Unilateral))
                     (pm '(not (applies ?x SpecificallySigned)))]) ])
     
     (make-section 
       :header (make-metadata :title "Art1342" :creator "Marcello")
       :schemes 
       [(make-scheme                            
          :header (make-metadata :title "Art1342co1-1" :creator "Marcello")
          :conclusion '(Better-ExArt1342co1 ?x)
          :premises [(pm '(contained-in ?x ?c))
                     (pm '(applies ?c Precompiled))
                     (pm '(applies ?x AddedToPrecompiled))])
        
        (make-scheme                            
          :header (make-metadata :title "Art1342co1-1" :creator "Marcello")
          :conclusion '(Worse-ExArt1342co1 ?x)
          :premises [(pm '(contained-in ?x ?c))
                     (pm '(applies ?c Precompiled))
                     (pm '(not(applies ?x AddedToPrecompiled)))])
        
        (make-scheme                            
          :header (make-metadata :title "Art1342co2" :creator "Marcello")
          :conclusion '(Relevant-ExArt1342co2 ?x)
          :premises [(pm '(contained-in ?x ?c))
                     (pm '(applies ?c Precompiled))
                     (pm '(applies ?x ?y))
                     (pm '(Oppressive-Status ?y))
                     (pm '(not(applies ?x SpecificallySigned)))])  ])
     
     (make-section ; a GENERAL rule!
                   :header (make-metadata :title "Inefficacy" :creator "Marcello")
                   :schemes 
                   [(make-scheme                            
                      :header (make-metadata :title "General-Inefficacy" :creator "Marcello")
                      :conclusion '(Inefficacious ?x)
                      :premises [(pm '(considered-by ?x ?y))
                                 (pm '(applies ?y Clause-Inefficacy))] )])
     
     (make-section 
       :header (make-metadata :title "Art1341-Consequences" :creator "Marcello")
       :schemes 
       [(make-scheme                            
          :header (make-metadata :title "Art1341co1-Consequence" :creator "Marcello")
          :conclusion '(considered-by ?x Art1341co1cc)
          :premises [(pm '(Relevant-ExArt1341co1 ?x))] 
          :exceptions [(pm '(applies ?x International))
                       (pm '(applies ?x ReproducingLawDisposition))])
        
        (make-scheme 
          :header (make-metadata :title "Art1341co2-Consequence" :creator "Marcello")
          :conclusion '(considered-by ?x Art1342co1cc)
          :premises [(pm '(Relevant-ExArt1342co1 ?x))]
          :exceptions [(pm '(applies ?x International))
                       (pm '(applies ?x ReproducingLawDisposition))])])
     
     (make-section 
       :header (make-metadata :title "Art1342-Consequences" :creator "Marcello")
       :schemes 
       [(make-scheme                            
          :header (make-metadata :title "Art1342co1-Consequence" :creator "Marcello")
          :conclusion '(normatively-striclty-worse ?x ?y)
          :premises [(pm '(contained-in ?x ?c))
                     (pm '(contained-in ?y ?c))
                     (pm '(Better-ExArt1342co1 ?x))
                     (pm '(Worse-ExArt1342co1 ?y))])
        
        (make-scheme  
          :header (make-metadata :title "Art1342co2-Consequence" :creator "Marcello")
          :conclusion '(considered-by ?x Art1342co2cc)
          :premises [(pm '(Relevant-ExArt1342co2 ?x))]
          :exceptions [(pm '(applies ?x International))
                       (pm '(applies ?x ReproducingLawDisposition))])  ])
     
     (make-section 
       :header (make-metadata :title "Art1341co1-Exclusion" :creator "Marcello")
       :schemes 
       [(make-scheme                            
          :header (make-metadata :title "Art1341co1-Exclusion-2" :creator "Marcello")
          :conclusion '(not (Relevant-ExArt1341co1 ?x))
          :premises [(pm '(not(applies ?x General)))])
        (make-scheme                            
          :header (make-metadata :title "Art1341co1-Exclusion-3" :creator "Marcello")
          :conclusion '(not (Relevant-ExArt1341co1 ?x))
          :premises [(pm '(not(applies ?x Unilateral)))])
        (make-scheme                            
          :header (make-metadata :title "Art1341-Exclusion-4" :creator "Marcello")
          :conclusion '(not (Relevant-ExArt1341co1 ?x))
          :premises [(pm '(applies ?x Knowledgeable))])])
     
     (make-section 
       :header (make-metadata :title "Art1341co2-Exclusion" :creator "Marcello")
       :schemes 
       [(make-scheme                            
          :header (make-metadata :title "Art1341co2-Exclusion-1" :creator "Marcello")
          :conclusion '(not (Relevant-ExArt1341co1 ?x))
          :premises [(pm '(applies ?x SpecificallySigned))]) 
        (make-scheme                            
          :header (make-metadata :title "Art1341co2-Exclusion-2" :creator "Marcello")
          :conclusion '(not (Relevant-ExArt1341co2 ?x))
          :premises [(pm '(not (Relevant-ExArt1341co1 ?x)))]) ])] ))

(def max-goals 500)  
(def generators (list (generate-arguments-from-theory theory1)))      

(def global-facts
   '((not (applies AOUMessina-ByteSoftwareHouse-Clause2.020 SpecificallySigned))
     (applies AOUMessina-ByteSoftwareHouse-Clause2.020 ArbitrationAgreement)
     (applies AOUMessina-ByteSoftwareHouse-Clause2.020 General)
     (applies AOUMessina-ByteSoftwareHouse-Clause2.020 Unilateral)))            

(defn ag [facts query]  ;
  "(seq-of literal) literal -> argument-graph
   construct and evaluate an argument graph"
  (argue (make-engine max-goals facts generators)
         aspic-grounded
         query))
                                   
(deftest test-engine-example1
         (let [facts '((not (applies AOUMessina-ByteSoftwareHouse-Clause2.020 SpecificallySigned))
                       (applies AOUMessina-ByteSoftwareHouse-Clause2.020 ArbitrationAgreement)
                       (applies AOUMessina-ByteSoftwareHouse-Clause2.020 General)
                       (applies AOUMessina-ByteSoftwareHouse-Clause2.020 Unilateral)
                       (applies Art1341co2 Clause-Inefficacy)
                       (Oppressive-Status ArbitrationAgreement))
               query '(Inefficacious AOUMessina-ByteSoftwareHouse-Clause2.020)]
           (is (in? (ag facts query) query))))
       
(deftest test-engine-example2
         (let [facts '((not (contained-in AgenziaImmobiliareD-NgPF-ClauseX AgenziaImmobiliareD-NgPF-Contract))
                       (applies AgenziaImmobiliareD-NgPF-Contract Precompiled)
                       (applies AgenziaImmobiliareD-NgPF-ClauseX SpecificallySigned)
                       (applies AgenziaImmobiliareD-NgPF-ClauseX Unilateral))
               query '(not (Relevant-ExArt1341co2 AgenziaImmobiliareD-NgPF-ClauseX))]
           (is (in? (ag facts query) query))))
       
(deftest test-engine-example3
         (let [facts '((contained-in LLoyd-FF-Clause Lloyd-FF-Contract)
                       (applies Lloyd-FF-Contract Precompiled)
                       (applies LLoyd-FF-Clause AddedToPrecompiled)
                       (applies LLoyd-FF-Clause SpecificallySigned))
               query '(Better-ExArt1342co1 LLoyd-FF-Clause)]
           (is (in? (ag facts query) query))))
       
       
       
       
       
     