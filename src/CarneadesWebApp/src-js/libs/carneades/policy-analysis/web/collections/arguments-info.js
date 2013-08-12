// Copyright (c) 2013 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

goog.provide('carneades.policy_analysis.web.collections.arguments_info');

goog.require('carneades.policy_analysis.web.models.argument_info');

PM.ArgumentsInfo = Backbone.Collection.extend(
    {model: PM.ArgumentInfo,

     url: function() {
         return IMPACT.wsurl + '/argument-info/' + IMPACT.project + '/' + (this.db || IMPACT.db);
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
