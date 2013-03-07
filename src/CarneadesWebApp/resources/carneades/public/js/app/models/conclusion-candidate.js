// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

PM.ConclusionCandidate = Backbone.Model.extend(
    {defaults: {
         statements: null // a StatementsCollection
     },
     
     initialize: function(attrs) {
         // when one element of the collection changes
         // triggers a change on this model
         attrs.statements.bind('all', this.triggerChange, this);
     },
     
     triggerChange: function() {
         this.trigger('change');
     }
        
    }
);
