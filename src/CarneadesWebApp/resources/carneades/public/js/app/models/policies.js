// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

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
