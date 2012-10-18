PM.Argument = Backbone.Model.extend(
    {defaults: {
         weight: 0.5,
         pro: true
     },
   
     url: function() {
         return IMPACT.wsurl + '/argument/' + IMPACT.db;
     },
   
     initialize: function() {
         var memento = new Backbone.Memento(this);
         _.extend(this, memento);
     },
     
     validate: function(attrs) {
         if(_.isNil(attrs.conclusion)) {
             return "Conclusion attribute is missing";
         }

         return undefined;
     },
     
     // Return true if the argument was agreed by the user
     // 
     // An argument is agreed if all its positives premises
     // are agreed and all its negatives premises are 
     // disagreed
     is_agreed: function(stmt_votes) {
         var positive_premises = _.filter(this.get('premises'), 
                                          function(p) { 
                                              return p.statement.positive;
                                          });
         var negative_premises = _.filter(this.get('premises'), 
                                          function(p) { 
                                              return !p.statement.positive;
                                          });
         
         var all_pos_agreed = _.all(positive_premises,
                                    function(p) {
                                        return stmt_votes[p.statement.id] == 1.0;
                                    });
         var all_neg_agreed = _.all(negative_premises,
                                    function(p) {
                                        return stmt_votes[p.statement.id] == 0.0;
                                    });
         
         return all_pos_agreed && all_pos_agreed;         
     }
     
    }
);
