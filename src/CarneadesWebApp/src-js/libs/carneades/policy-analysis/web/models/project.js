// Copyright (c) 2013 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

goog.provide('carneades.policy_analysis.web.models.project');

// A model for a theory containing a language, a header, schemes or sections etc.
PM.Project = Backbone.Model.extend(
    {defaults: function(){
         return {
         };
     },
     
     url: function() {
         return IMPACT.wsurl + '/project/' + this.id;
     },
     
     initialize: function(attrs) {
     }
     
    }
);
