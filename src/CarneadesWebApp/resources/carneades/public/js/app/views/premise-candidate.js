// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

// Displays a view to select a candidate for a premise or an exception
PM.PremiseCandidateView = Backbone.View.extend(
    {className: "premise-candidate",
     
     events: {
         "change .role-input": "role_changed",
         "change input[type=hidden]": "statement_changed",
         "click .delete": "on_delete_premise",
         "click .create": "create_statement",
         "change input[name=positive]": "positive_changed",
         "change input[name=implicit]": "implicit_changed"
     },
     
     initialize: function() {
         this.model.on('change', this.render, this);
         _.bindAll(this, 'role_changed', 'statement_changed', 
                   'render', 'on_delete_premise', 'create_statement');
     },
     
     render: function() {
         var data = this.model.toJSON();
         
         this.$el.html(ich.premisecandidate({pmt_role: $.i18n.prop('pmt_role'),
                                             pmt_statement: $.i18n.prop('pmt_statement')}));
         
         var role = this.$('.role-input');
         role.prop('disabled', !data.editableRole);
         if(data.premise.role) {
             role.val(data.premise.role);    
         } 
         
         this.$('input[name=positive]').attr('checked', 
                                             data.premise.positive == undefined ? 
                                             true : data.premise.positive);
         this.$('input[name=implicit]').attr('checked', data.premise.implicit);
         
         var statement = this.statement();
         statement.select2({data: {results: data.statements.toJSON(),
                                   text: function(statement) {
                                       return AGB.statement_text(statement);
                                   }
                                  },
                            placeholder: $.i18n.prop('pmt_select_a_statement'),
                            formatSelection: AGB.format_selected_statement,
                            formatResult: AGB.statement_text,
                            initSelection: function(element, callback) {
                                var statement = data.statements.get(element.val());
                                if(statement) {
                                    callback(statement.toJSON()); 
                                } 
                            }});

         if(data.premise.statement) {
             statement.val(data.premise.statement.id).trigger('change');    
         } 
         
         return this;
     },
     
     statement: function() {
         return this.$('input[type=hidden]');
     },
     
     role_changed: function() {
         var role = this.$('.role-input').val();
         var premise = _.clone(this.model.get('premise'));
         premise.role = role;
         this.model.set('premise', premise);
     },
     
     statement_changed: function() {
         var statement = this.model.get('statements').get(this.statement().val());
         
         if(!_.isNil(statement)) {
             var premise = _.clone(this.model.get('premise'));
             premise.statement = statement.attributes;
             this.model.set('premise', premise);
         } 
     },
     
     on_delete_premise: function() {
         // removes the PremiseCandidate from the PremisesCandidates collection
         this.model.get('container').remove(this.model);
         
         this.statement().val(undefined).trigger('change');
         
         // removes the view
         this.remove();
         return false;
     },
     
     create_statement: function() {
         var self = this;
         var atom = this.model.get('suggested_atom') ? this.model.get('suggested_atom').replace(/\?/g, '') : "";
         AGB.show_statement_editor({atom: atom,
                                   save_callback: function(data) {
                                       var id = data.id;
                                       var statements = self.model.get('statements');
                                       var statement = statements.get(id);
                                       
                                       var premise = _.clone(self.model.get('premise'));
                                       premise.statement = statement; 
                                       self.model.set('premise', premise);
                                   }
                                   });
     },
     
     positive_changed: function() {
         var positive = this.$('input[name=positive]').is(':checked');
         var premise = _.clone(this.model.get('premise'));
         premise.positive = positive;
         this.model.set('premise', premise);
     },
     
     implicit_changed: function() {
         var implicit = this.$('input[name=implicit]').is(':checked');
         var premise = _.clone(this.model.get('premise'));
         premise.implicit = implicit ;
         this.model.set('premise', premise);
     }
     
    }
);

