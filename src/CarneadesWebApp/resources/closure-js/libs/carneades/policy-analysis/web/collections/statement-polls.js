// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

goog.provide('carneades.policy_analysis.web.collections.statement_polls');

goog.require('carneades.policy_analysis.web.models.statement_poll');

// A collection of statement polls for the SCT
PM.StatementPolls = Backbone.Collection.extend(
    {model: PM.StatementPoll,
     
     url: function() {
         return IMPACT.wsurl + '/statement-poll/' + this.db;
     },

     initialize: function(model, options) {
         this.db = options.db || IMPACT.debate_db;
     }
     
    }
);
