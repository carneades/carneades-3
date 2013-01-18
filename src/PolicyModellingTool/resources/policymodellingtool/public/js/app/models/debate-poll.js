// Copyright (c) 2013 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

// Stores the result of a poll
PM.DebatePoll = Backbone.Model.extend(
    {url: function() {
        return IMPACT.wsurl + '/debate-poll/' + IMPACT.debate_db;
     },
    }
);
