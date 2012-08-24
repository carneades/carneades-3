PM.ArgumentCandidate = Backbone.Model.extend(
    {initialize: function(attrs) {
         var memento = new Backbone.Memento(this);
         _.extend(this, memento);
         
         // set the ConclusionCandidate for this ArgumentCandidate
         var conclusioncandidate = new PM.ConclusionCandidate({statement: attrs.argument.get('conclusion'), 
                                                               statements: attrs.statements});
         this.set('conclusion', conclusioncandidate);
         
         var scheme_name = attrs.argument.get('scheme');
         var scheme_candidate = new PM.SchemeCandidate({schemes: attrs.schemes,
                                                        scheme_name: scheme_name});
         this.set('scheme', scheme_candidate);
         
         var metadata_candidate = new PM.MetadataCandidate(
             {metadata: attrs.argument.get('header'),
              current_lang: attrs.current_lang});
         this.set('metadata', metadata_candidate);
         
         
         // set the PremisesCandidates for this ArgumentCandidate
         var premisescandidates = new PM.PremisesCandidates;
         this.set('premises', premisescandidates);
         _.each(attrs.argument.get('premises'),
               function(premise) {
                   premisescandidates.add({premise: premise,
                                           statements: attrs.statements});
               });
     },

     validate: function(attrs) {
         return undefined;
     }
     
    }
);