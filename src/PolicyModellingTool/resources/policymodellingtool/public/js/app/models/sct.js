// Data for the SCT
PM.Sct = Backbone.Model.extend(
    {defaults: function(){
         return {
             username: "",
             issue: 'Q12'
         };
     },
     
     url: function() {
         return '/sct/';
     },
     
     initialize: function(attrs) {
         
     }
     
    }
);
