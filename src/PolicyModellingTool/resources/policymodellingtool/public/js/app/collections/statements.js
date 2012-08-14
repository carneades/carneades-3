PM.StatementsCollection = Backbone.Collection.extend(
    {model: PM.Statement,
     url: function() {
         return IMPACT.wsurl + '/statement/' + IMPACT.db;
     }        
                                                         
    }
    
);