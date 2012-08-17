PM.Arguments = Backbone.Collection.extend(
    {model: PM.Argument,

     url: function() {
         return IMPACT.wsurl + '/argument/' + IMPACT.db;
     }

    }

);
