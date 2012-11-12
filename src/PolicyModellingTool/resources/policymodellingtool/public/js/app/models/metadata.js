PM.Metadata = Backbone.Model.extend(
    {defaults: function(){
         return {
             description: {}
             };
     },
     
     url: function() {
         return IMPACT.wsurl + '/metadata/' + IMPACT.db;
     },

     initialize: function(attrs) {
         var memento = new Backbone.Memento(this);
         _.extend(this, memento); 
     }
        
    }
);
