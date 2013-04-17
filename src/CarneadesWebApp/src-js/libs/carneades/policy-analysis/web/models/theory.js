// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

goog.provide('carneades.policy_analysis.web.models.theory');

// A model for a theory containing a language, a header, schemes or sections etc.
PM.Theory = Backbone.Model.extend(
    {defaults: function(){
         return {
         };
     },
     
     url: function() {
         return IMPACT.wsurl + '/theory/' + this.theory_path;
     },
     
     initialize: function(attrs) {
         this.theory_path = attrs.theory_path;
     }
     
    }
);
