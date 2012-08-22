PM.PremiseCandidate = Backbone.Model.extend(
    {defaults: {
         editableRole: true,
         premise: {},
         statements: null // a StatementsCollection
     },
     
     initialize: function(attrs) {
         // when one element of the collection changes
         // triggers a change on this model
         attrs.statements.bind('all', this.trigger_change, this);
     },
     
     trigger_change: function() {
         this.trigger('change');
     }
        
    }
);
