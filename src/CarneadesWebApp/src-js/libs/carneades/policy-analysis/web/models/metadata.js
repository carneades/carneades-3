// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

goog.provide('carneades.policy_analysis.web.models.metadata');

PM.Metadata = Backbone.Model.extend(
    {defaults: function(){
         return {
             description: {}
             };
     },
     
     url: function() {
         return IMPACT.wsurl + '/metadata/' + IMPACT.project + '/' + IMPACT.db;
     },

     initialize: function(attrs) {
         var memento = new Backbone.Memento(this);
         _.extend(this, memento); 
     }
        
    }
);
