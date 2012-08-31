PM.ArgumentCandidate = Backbone.Model.extend(
    {initialize: function(attrs) {
         var memento = new Backbone.Memento(this);
         _.extend(this, memento);
         
         // set the ConclusionCandidate for this ArgumentCandidate
         var conclusioncandidate = new PM.ConclusionCandidate({statement: attrs.argument.get('conclusion'), 
                                                               statements: attrs.statements});
         this.set('conclusion', conclusioncandidate);
         
         var initial_scheme_name = attrs.argument.get('scheme');
         var scheme = attrs.schemes.get(initial_scheme_name.slice(1, -1));
         var scheme_candidate = new PM.SchemeCandidate({schemes: attrs.schemes,
                                                        initial_scheme_name: initial_scheme_name,
                                                        scheme: scheme});
         this.set('scheme', scheme_candidate);
         
         var metadata_candidate = new PM.MetadataCandidate(
             {metadata: new PM.Metadata(attrs.argument.get('header')) || new PM.Metadata(),
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