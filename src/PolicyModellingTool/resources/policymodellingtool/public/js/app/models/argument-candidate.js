PM.ArgumentCandidate = Backbone.Model.extend(
    {defaults: {
         weight: 0.5
     },
     
     initialize: function(attrs) {
         // transforms the conclusion of the argument to a ConclusionCandidate
         var conclusioncandidate = new PM.ConclusionCandidate({statement: attrs.argument.conclusion, 
                                                               statements: attrs.statements});
         this.set('conclusion', conclusioncandidate);
         
         // TODO transform the premises to premises candidates
     },

     validate: function(attrs) {
         if(_.isNil(attrs.argument.conclusion)) {
             return "Conclusion attribute is missing";
         }

         return undefined;
     }
     
    }
);