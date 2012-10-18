// Stores the result of the SCT
PM.StatementPoll = Backbone.Model.extend(
    {url: function() {
         return IMPACT.wsurl + '/statement-poll/' + IMPACT.debate_db;
     },
     
     initialize: function(attrs) {
         this.saved = false;
         // this.id = username;
     },
     
     // normally backbone thinks the object is not new if it has an id
     // but it's not the case for us since the id is the userid and is
     // already known
     isNew: function() {
         return this.saved == false;
     },
     
     // Changes the save to issue a POST even if we already specified the id
     // in the constructor
     save: function() {
         var res = Backbone.Model.prototype.save.apply(this, arguments);
         this.saved = true; // TODO: pb if the save does not succeed
         return res;
     }
    }
);
