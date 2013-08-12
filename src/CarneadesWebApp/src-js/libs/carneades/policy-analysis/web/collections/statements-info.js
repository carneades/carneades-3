// Copyright (c) 2013 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

goog.provide('carneades.policy_analysis.web.collections.statements_info');


goog.require('carneades.policy_analysis.web.models.statement_info');

PM.StatementsInfo = Backbone.Collection.extend(
    {model: PM.StatementInfo,

     url: function() {
         return IMPACT.wsurl + '/statement-info/' + IMPACT.project + '/' + (this.db || IMPACT.db);
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
