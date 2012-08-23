PM.Schemes = Backbone.Collection.extend(
    {model: PM.Scheme,

     url: function() {
         return IMPACT.wsurl + '/scheme';
     }

    }
    
);