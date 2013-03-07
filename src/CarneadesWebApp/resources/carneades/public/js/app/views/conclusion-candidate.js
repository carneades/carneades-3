// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

// A candidate for the conclusion of an argument
PM.ConclusionCandidateView = Backbone.View.extend(
    {events: {
         "change input[type=hidden]" : "statement_changed",
         "click .create": "on_new_conclusion"
     },
     
     initialize: function() {
         this.model.on('change', this.render, this);
         _.bindAll(this, 'statement_changed', 'render');
     },
     
     render: function() {
         var data = this.model.toJSON();
         
         this.$el.html(ich.conclusioncandidate({conclusion_text: $.i18n.prop('pmt_conclusion'),
                                                create_new_statement_text: 
                                                $.i18n.prop('pmt_create_statement')}));
         
         var statement = this.statement();
         statement.select2({data: {results: data.statements.toJSON(),
                                   text: function(statement) {
                                       return AGB.statement_text(statement);
                                   }
                                  },
                            placeholder: $.i18n.prop('pmt_select_statement'),
                            formatSelection: AGB.format_selected_statement,
                            formatResult: AGB.statement_text,
                            initSelection: function(element, callback) {
                                callback(data.statements.get(element.val()).toJSON());
                            }});

         if(data.statement) {
             statement.val(data.statement.id).trigger('change');    
         } 
         
         return this;
     },
     
     statement: function() {
         return this.$('input[type=hidden]');
     },

     statement_changed: function() {
         var statement = this.model.get('statements').get(this.statement().val());
         this.model.set('statement', statement);
     },
     
     on_new_conclusion: function() {
         var self = this;
         var atom = this.model.get('suggested_atom') ? this.model.get('suggested_atom').replace(/\?/g, '') : "";
         AGB.show_statement_editor({atom: atom,
                                   save_callback: function(data) {
                                       var id = data.id;
                                       var statements = self.model.get('statements');
                                       var statement = statements.get(id);

                                       self.model.set('statement', statement);
                                   }
                                   });
         return false;
     }

    }
);



