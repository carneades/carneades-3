PM.ArgumentCandidate = Backbone.Model.extend(
    {defaults: {
         weight: 0.5
     },
     
     initialize: function(attrs) {
         // set the ConclusionCandidate for this ArgumentCandidate
         var conclusioncandidate = new PM.ConclusionCandidate({statement: attrs.argument.conclusion, 
                                                               statements: attrs.statements});
         this.set('conclusion', conclusioncandidate);
         
         // set the PremisesCandidates for this ArgumentCandidate
         var premisescandidates = new PM.PremisesCandidates;
         this.set('premises', premisescandidates);
         _.each(attrs.argument.premises,
               function(premise) {
                   premisescandidates.add({premise: premise,
                                           statements: attrs.statements});
               });
     },

     validate: function(attrs) {
         if(_.isNil(attrs.argument.conclusion)) {
             return "Conclusion attribute is missing";
         }

         return undefined;
     }
     
    }
);