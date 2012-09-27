PM.Arguments = Backbone.Collection.extend(
    {model: PM.Argument,

     url: function() {
         return IMPACT.wsurl + '/argument/' + this.db;
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
