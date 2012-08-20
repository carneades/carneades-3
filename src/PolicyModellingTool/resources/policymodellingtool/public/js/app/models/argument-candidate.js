PM.ArgumentCandidate = Backbone.Model.extend(
    {defaults: {
         weight: 0.5
     },
     
     initialize: function(attrs) {
         // transforms the conclusion of the argument to a ConclusionCandidate
         var conclusion = attrs.conclusion;
         _.extend(conclusion, {statements: attrs.statements}); 
         var conclusioncandidate = new PM.ConclusionCandidate(conclusion);
         this.set('conclusion', conclusioncandidate);
     },

     validate: function(attrs) {
         if(_.isNil(attrs.conclusion)) {
             return "Conclusion attribute is missing";
         }

         return undefined;
     }
     
    }
);