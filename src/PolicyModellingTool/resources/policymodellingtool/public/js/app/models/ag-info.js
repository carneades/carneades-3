// Aggregated information about an argument graph
PM.AgInfo = Backbone.Model.extend(
    {defaults: function(){
         return {
         
         };
     },
     
     url: function() {
         return IMPACT.wsurl + '/argumentgraph-info/' + this.db;
     },
     
     initialize: function(attrs) {
         this.db = attrs.db;
     }
     
    }
);
