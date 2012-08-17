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
         
         var role = this.$el.find('.role-input');
         role.prop('disabled', !data.editableRole);
         role.val(data.role);
         
         var statement = this.statement();
         statement.select2({data: {results: statements.toJSON(),
                                   text: function(statement) {
                                       return AGB.statement_text(statement);
                                   }
                                  },
                            placeholder: "Select a statement",
                            formatSelection: AGB.format_selected_statement,
                            formatResult: AGB.statement_text,
                            initSelection: function(element) {
                                return statements.get(element.val()).toJSON();
                            }});

         if(data.statement) {
             statement.val(data.statement.attributes.id).trigger('change');    
         } 
         
         return this;
     },
     
     statement: function() {
         return this.$el.find('input[type=hidden]');
     },
     
     role_changed: function() {
         this.model.set('role', $('.role-input').val());
     },
     
     statement_changed: function() {
         var statement = this.model.get('statements').get(this.statement().val());
         this.model.set('statement', statement);
     }
    }
);


