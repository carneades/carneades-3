PM.Statements = Backbone.Collection.extend(
    {model: PM.Statement,

     url: function() {
         return IMPACT.wsurl + '/statement/' + (this.db || IMPACT.db);
     },
     
     initialize: function(attrs) {
         if(attrs) {
             this.db = attrs.db || IMPACT.db;    
         } else {
             attrs = IMPACT.db;
         } 
     }
                                                         
    }
    
);