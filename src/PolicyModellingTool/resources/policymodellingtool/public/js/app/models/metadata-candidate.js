// A candidate for replacing some Metadata
PM.MetadataCandidate = Backbone.Model.extend(
    {defaults: function(){
         return {
         
         };
     },
     
     
     // Expects a metadata and a current_lang field
     initialize: function(attrs) {
         var memento = new Backbone.Memento(this);
         _.extend(this, memento);
     }
     
    }
);
