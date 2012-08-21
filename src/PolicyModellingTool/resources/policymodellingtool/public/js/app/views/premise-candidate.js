PM.PremiseCandidateView = Backbone.View.extend(
    {className: "premise-candidate",
     
     events: {
         "change input": "role_changed",
         "change input[type=hidden]" : "statement_changed"
     },
     
     initialize: function() {
         this.model.on('change', this.render, this);
         _.bindAll(this, 'role_changed', 'statement_changed', 'render');
     },
     
     render: function() {
         var data = this.model.toJSON();
         
         this.$el.html(ich.premisecandidate());
         
         var role = this.$('.role-input');
         role.prop('disabled', !data.editableRole);
         role.val(data.premise.role);
         
         var statement = this.statement();
         statement.select2({data: {results: data.statements.toJSON(),
                                   text: function(statement) {
                                       return AGB.statement_text(statement);
                                   }
                                  },
                            placeholder: "Select a statement",
                            formatSelection: AGB.format_selected_statement,
                            formatResult: AGB.statement_text,
                            initSelection: function(element, callback) {
                                callback(data.statements.get(element.val()).toJSON());
                            }});

         if(data.premise) {
             statement.val(data.premise.statement.id).trigger('change');    
         } 
         
         return this;
     },
     
     statement: function() {
         return this.$('input[type=hidden]');
     },
     
     role_changed: function() {
         var premise = this.model.get('premise');
         premise.role = $('.role-input').val();
         this.model.set('premise', premise);
     },
     
     statement_changed: function() {
         var statement = this.model.get('statements').get(this.statement().val());
         var premise = this.model.get('premise');
         premise.statement = statement.attributes;
         this.model.set('premise', premise);
         // this.model.set('statement', statement.attributes);
     }
    }
);


