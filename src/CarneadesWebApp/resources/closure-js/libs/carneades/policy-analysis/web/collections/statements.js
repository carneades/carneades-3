// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

goog.provide('carneades.policy_analysis.web.collections.statements');


goog.require('carneades.policy_analysis.web.models.statement');

PM.Statements = Backbone.Collection.extend(
    {model: PM.Statement,

     url: function() {
         return IMPACT.wsurl + '/statement/' + (this.db || IMPACT.db);
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
