PM.ArgumentCandidate = Backbone.Model.extend(
    {initialize: function(attrs) {
         // set the ConclusionCandidate for this ArgumentCandidate
         var conclusioncandidate = new PM.ConclusionCandidate({statement: attrs.argument.get('conclusion'), 
                                                               statements: attrs.statements});
         this.set('conclusion', conclusioncandidate);
         
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