PM.PremiseCandidate = Backbone.Model.extend(
    {defaults: function(){
         return {
             editableRole: true,
             premise: {},
             statements: null // a StatementsCollection  
         };
     },
     
     initialize: function(attrs) {
         var memento = new Backbone.Memento(this);
         _.extend(this, memento);
         
         // when one element of the collection changes
         // triggers a change on this model
         attrs.statements.bind('all', this.trigger_change, this);
     },
     
     trigger_change: function() {
         this.trigger('change');
     }
        
    }
);
