PM.PremiseCandidate = Backbone.Model.extend(
    {defaults: {
         role: "",
         editableRole: true,
         statement: null, // the selected Statement
         statements: null // a StatementsCollection
     },
     
     initialize: function(attrs) {
         this.set(_.extend(this.defaults, attrs));
         
         // when one element of the collection changes
         // triggers a change on this model
         attrs.statements.bind('all', this.triggerChange, this);
     },
     
     triggerChange: function() {
         this.trigger('change');
     }
        
    }
);
