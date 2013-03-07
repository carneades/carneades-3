// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

PM.Schemes = Backbone.Collection.extend(
    {model: PM.Scheme,

     url: function() {
         return IMPACT.wsurl + '/scheme';
     }

    }
    
);
