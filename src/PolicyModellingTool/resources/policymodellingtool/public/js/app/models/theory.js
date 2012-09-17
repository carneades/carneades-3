// A model for a theory containing a language, a header, schemes or sections etc.
PM.Theory = Backbone.Model.extend(
    {defaults: function(){
         return {
             id: "walton"
         };
     },
     
     urlRoot: IMPACT.wsurl + '/theory', 
     
     initialize: function(attrs) {
         
     }
     
    }
);
