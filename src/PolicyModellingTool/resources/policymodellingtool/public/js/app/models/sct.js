// Data for the SCT
PM.Sct = Backbone.Model.extend(
    {defaults: function(){
         return {
         username: ""
         };
     },
     
     url: function() {
         return '/sct/';
     },
     
     initialize: function(attrs) {
         
     }
     
    }
);
