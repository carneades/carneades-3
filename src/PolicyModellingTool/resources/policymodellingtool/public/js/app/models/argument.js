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
     }
     
    }
);