PM.Argument = Backbone.Model.extend(
    {defaults: {
         weight: 0.5
     },
     
     url: function() {
         return IMPACT.wsurl + '/argument/' + IMPACT.db;
     },
     
     validate: function(attrs) {
         if(_.isNil(attrs.conclusion)) {
             return "Conclusion attribute is missing";
         }

         return undefined;
     }
     
    }
);