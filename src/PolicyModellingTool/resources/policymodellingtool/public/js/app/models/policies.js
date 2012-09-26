// The policies to be examined
PM.Policies = Backbone.Model.extend(
    {defaults: function(){
         return {
         
         };
     },
     
     url: function() {
         return IMPACT.wsurl + '/policies';
     },
     
     initialize: function(attrs) {
         
     }
     
    }
);
