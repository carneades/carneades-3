// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

goog.provide('carneades.policy_analysis.web.collections.arguments');

goog.require('carneades.policy_analysis.web.models.argument');

PM.Arguments = Backbone.Collection.extend(
    {model: PM.Argument,

     url: function() {
         return IMPACT.wsurl + '/argument/' + (this.db || IMPACT.db);
     },
     
     initialize: function(models, attrs) {
         if(attrs) {
             this.db = attrs.db || IMPACT.db;    
         } else {
             attrs = IMPACT.db;
         } 
     }

    }

);
