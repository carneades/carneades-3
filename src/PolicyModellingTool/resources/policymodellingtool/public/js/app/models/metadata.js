PM.Metadata = Backbone.Model.extend(
    {defaults: function(){
         return {
         };
     },
     
     url: function() {
         return IMPACT.wsurl + '/metadata/' + IMPACT.db;
     },

     initialize: function(attrs) {
     }
        
    }
);
