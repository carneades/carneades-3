#!r6rs

(import (rnrs)
        (carneades dlp))

(ontology time
          
          (define-primitive-concept lkif-top:Spatio_Temporal_Occurrence )
          
          (define-primitive-concept time:Moment (and mereology:Atom
                                                     time:Temporal_Occurrence))
          
          (define-primitive-concept mereology:Atom )
          
          (define-primitive-concept time:Pair_Of_Periods
            (and mereology:Pair
                 (all mereology:component time:Temporal_Occurrence)))
          
          (define-primitive-concept time:Interval
            (and mereology:Composition
                 time:Temporal_Occurrence))
          
          (define-primitive-concept mereology:Pair )
          
          (define-primitive-concept mereology:Composition )
          
          (define-primitive-concept time:Temporal_Occurrence
            (and lkif-top:Spatio_Temporal_Occurrence
                 (and (some time:immediately_after time:Temporal_Occurrence)
                      (some time:immediately_before time:Temporal_Occurrence))))
          
          (define-primitive-role time:temporal_relation :domain time:Temporal_Occurrence :range time:Temporal_Occurrence)
          
          (define-primitive-role time:before :transitive t :domain time:Temporal_Occurrence :range time:Temporal_Occurrence :parents( time:temporal_relation))
          
          (define-primitive-role time:starts :parents( time:temporal_relation))
          
          (define-primitive-role time:between :domain time:Temporal_Occurrence :range time:Temporal_Occurrence :parents( time:temporal_relation))
          
          (define-primitive-role time:finishes :parents( time:temporal_relation))
          
          (define-primitive-role time:during :parents( time:temporal_relation))
          
          (define-primitive-role time:overlap :parents( time:temporal_relation))
          
          (define-primitive-role time:after :transitive t :domain time:Temporal_Occurrence :range time:Temporal_Occurrence :parents( time:temporal_relation))
          
          (define-primitive-role mereology:component)
          
          (define-primitive-role time:preceeds :parents( time:temporal_relation))
          
          (define-primitive-role time:immediately_before :domain time:Temporal_Occurrence :range time:Temporal_Occurrence :parents( time:before))
          
          (define-primitive-role time:immediately_after :domain time:Temporal_Occurrence :range time:Temporal_Occurrence :parents( time:after))
          
          )