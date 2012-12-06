// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

// A collection of statement polls for the SCT
PM.StatementPolls = Backbone.Collection.extend(
    {model: PM.StatementPoll,
     
     url: function() {
         return IMPACT.wsurl + '/statement-poll/' + IMPACT.debate_db;
     }
     
    }
);
