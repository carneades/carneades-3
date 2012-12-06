// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

PM.Arguments = Backbone.Collection.extend(
    {model: PM.Argument,

     url: function() {
         return IMPACT.wsurl + '/argument/' + (this.db || IMPACT.db);
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
