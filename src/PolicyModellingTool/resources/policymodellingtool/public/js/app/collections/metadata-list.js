PM.MetadataList = Backbone.Collection.extend(
    {model: PM.Metadata,

     url: function() {
         return IMPACT.wsurl + '/metadata/' + IMPACT.db;
     }        

    }
    
);