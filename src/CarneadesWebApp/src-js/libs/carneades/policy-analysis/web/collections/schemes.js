// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

goog.provide('carneades.policy_analysis.web.collections.schemes');


goog.require('carneades.policy_analysis.web.models.scheme');

PM.Schemes = Backbone.Collection.extend(
    {model: PM.Scheme,

     url: function() {
         return IMPACT.wsurl + '/scheme/' + IMPACT.project;
     }

    }
    
);
