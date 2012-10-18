// A collection of arguument polls for the SCT
PM.ArgumentPolls = Backbone.Collection.extend(
    {model: PM.ArgumentPoll,
     
     url: function() {
         return IMPACT.wsurl + '/argument-poll/' + IMPACT.debate_db;
     }
     
    }
);
