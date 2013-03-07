// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

// View for editing an argument
PM.ArgumentEditorView = Backbone.View.extend(
    {className: "argument-editor",
     
     events: {
         "click .cancel-argument": "cancel",
         "click .save-argument": "save",
         "change input:radio[name=pro][value=pro]": "direction_changed",
         "change input:radio[name=pro][value=con]": "direction_changed",
         "change input:radio[name=strict][value=yes]": "strict_changed",
         "change input:radio[name=strict][value=no]": "strict_changed",
         "change .weight-input": "weight_changed",
         "change input[type=range]": "weight_slided"
     },
     
     initialize: function(attrs) {
         this.title = attrs.title;

         // saves state of the argument candidate
         this.model.store(); 
         this.model.get('argument').store();
         
         this.model.on('change', this.render, this);
         // this.model.get('argument').on('change', this.render, this);
         this.model.get('scheme').on('change', this.scheme_changed, this);
         _.bindAll(this, 'render', 'cancel');
     },

     render: function() {
         var data = this.model.toJSON();
         
         this.$el.html(ich.argumenteditor({title: this.title,
                                           pmt_header: $.i18n.prop('pmt_header'),
                                           pmt_direction: $.i18n.prop('pmt_direction'),
                                           pmt_strict: $.i18n.prop('pmt_strict'),
                                           pmt_weight: $.i18n.prop('pmt_weight'),
                                           pmt_yes: $.i18n.prop('pmt_yes'),
                                           pmt_no: $.i18n.prop('pmt_no'),
                                           pmt_high: $.i18n.prop('pmt_high'),
                                           pmt_low: $.i18n.prop('pmt_low'),
                                           pmt_cancel: $.i18n.prop('pmt_cancel'),
                                           pmt_save_argument: $.i18n.prop('pmt_save_argument') 
                                          }));

         if(data.argument.attributes.pro) {
             this.pro_el().attr('checked',true);
         } else {
             this.con_el().attr('checked', true);
         }
         
         if(data.argument.attributes.strict) {
             this.strict_yes_el().attr('checked', true);
         } else {
             this.strict_no_el().attr('checked', true);
         }

         this.$('.weight-input').val(data.argument.attributes.weight);
         this.$('input[type=range]').val(data.argument.attributes.weight);
         
         this.scheme_candidate_view = new PM.SchemeCandidateView({model: this.model.get('scheme'),
                                                                  el: this.$('.scheme-candidate')});
         this.scheme_candidate_view.render();
         var scheme = this.model.get('scheme').get('scheme');
         if(scheme) {
             var scheme_metadata = _.clone(scheme.get('header'));
             delete scheme_metadata.title;
             PM.set_metadata_has_properties(scheme_metadata);

             var data = _.clone(scheme_metadata);
             data.pmt_key = $.i18n.prop('pmt_key');
             data.pmt_coverage = $.i18n.prop('pmt_coverage');
             data.pmt_creator = $.i18n.prop('pmt_creator');
             data.pmt_date = $.i18n.prop('pmt_date');
             data.pmt_date = $.i18n.prop('pmt_date');
             data.pmt_format = $.i18n.prop('pmt_format');
             data.pmt_identifier = $.i18n.prop('pmt_identifier');
             data.pmt_language = $.i18n.prop('pmt_language');
             data.pmt_publisher = $.i18n.prop('pmt_publisher');
             data.pmt_relation = $.i18n.prop('pmt_relation');
             data.pmt_rights = $.i18n.prop('pmt_rights');
             data.pmt_source = $.i18n.prop('pmt_source');
             data.pmt_subject = $.i18n.prop('pmt_subject');
             data.pmt_title = $.i18n.prop('pmt_title');
             data.pmt_type = $.i18n.prop('pmt_type');

             var scheme_metadata_html = ich.metadata(data);
             this.$('.scheme-metadata').html(scheme_metadata_html);
             
             if(scheme_metadata.description && scheme_metadata.description[IMPACT.lang]) {
                 var description_text = PM.markdown_to_html(scheme_metadata.description[IMPACT.lang]);
                 this.$('.scheme-description').html(description_text);
             }
             
         } 
         
         var conclusioncandidateview = new PM.ConclusionCandidateView({model: this.model.get('conclusion'),
                                                                       el: this.$('.conclusion-candidate')});
         conclusioncandidateview.render();

         if(this.premises_candidates_view) {
             this.premises_candidates_view.remove();
         }
         this.premises_candidates_view = new PM.PremisesCandidatesView(
             {model: this.model,
              add_more_text: $.i18n.prop('pmt_add_premise'),
              container: 'premises',
              elements_name: 'Premises',
              el: this.$('.argument-editor-premises')});
         this.premises_candidates_view.render();

         if(this.exceptions_candidates_view) {
             this.exceptions_candidates_view.remove();
         }
         this.exceptions_candidates_view = new PM.PremisesCandidatesView(
             {model: this.model,
              add_more_text: $.i18n.prop('pmt_add_exception'),
              container: 'exceptions',
              elements_name: 'Exceptions',
              el: this.$('.argument-editor-exceptions')});
         this.exceptions_candidates_view.render();
         
         
         this.metadata_editor_view = new PM.MetadataEditorView({model: this.model.get('metadata'),
                                                                el: this.$('.argument-metadata')});
         this.metadata_editor_view.render();

         return this;
     },
     
     pro_el: function() {
         return this.$('input:radio[name=pro][value=pro]');
     },
     
     con_el: function() {
         return this.$('input:radio[name=pro][value=con]');
     },
     
     strict_yes_el: function() {
         return this.$('input:radio[name=strict][value=yes]');
     },
     
     strict_no_el: function() {
         return this.$('input:radio[name=strict][value=no]');
     },
     
     is_strict: function() {
         return this.strict_yes_el().is(':checked') && !this.strict_no_el().is(':checked');
     },
     
     is_pro: function() {
       return this.pro_el().is(':checked') && !this.con_el().is(':checked');  
     },
     
     direction_changed: function() {
         this.model.get('argument').set('pro', this.is_pro());
     },

     strict_changed: function() {
         this.model.get('argument').set('strict', this.is_strict());
     },
     
     weight: function() {
         return this.$('.weight-input').val();
     },
     
     weight_changed: function() {
         this.model.get('argument').set('weight', this.weight());
     },
     
     weight_slided: function() {
         var slider_val = this.$('input[type=range]').val();
         this.$('.weight-input').val(slider_val).trigger('change');
     },
     
     scheme_changed: function() {
         var self = this;
         var scheme = this.model.get('scheme').get('scheme');
         var premises_candidates_views = this.premises_candidates_view.premises_candidates_views;
         var nb_premises = scheme.get('premises').length;
         
         var premises_candidates = this.model.get('premises');
         
         // removes excedent premises, if needed
         var length = premises_candidates.length;
         for(var i = 0; i < (length - nb_premises); i++) {
             premises_candidates.pop();
         }

         // adds necessary premises, if needed
         length = premises_candidates.length;
         for(i = 0; i < (nb_premises - length); i++) {
             premises_candidates.push(
                 new PM.PremiseCandidate({statements: this.model.get('statements')}));
         }
         
         // set the exceptions candidate and their expected atoms
         var exceptions_candidates = this.model.get('exceptions');

         while(exceptions_candidates.length != 0) {
             exceptions_candidates.pop();
         } 

         _.each(scheme.get('exceptions'),
               function(exception) {
                   exceptions_candidates.add({premise: exception,
                                              suggested_atom: AGB.sexpr_to_str(exception.statement.atom),
                                              statements: self.model.get('statements')});
               });

         // set the role of the premises and their expected atoms
         for(i = 0; i < nb_premises; i++) {
             var premise = scheme.get('premises')[i];
             var premise_candidate = premises_candidates.at(i);
             var current_premise = _.clone(premise_candidate.get('premise'));
             current_premise.role = premise.role;
             premise_candidate.set('premise', current_premise);
             premise_candidate.set('suggested_atom', AGB.sexpr_to_str(premise.statement.atom));
         }
         
         // set the suggested_atom for the conclusion candidate
         var conclusion_candidate = this.model.get('conclusion');
         conclusion_candidate.set('suggested_atom',  AGB.sexpr_to_str(scheme.get('conclusion')));

         this.render();

     },

     premise_candidate_exists: function(premise_candidate) {
         var premise = premise_candidate.get('premise');
         if(_.isNil(premise) || _.isNil(premise.statement)) {
             return false;
         }
         var atom = premise.statement.atom;
         var is_grounded = AGB.is_grounded(AGB.sexpr_to_str(atom));
         return is_grounded;
                          
     },
     
     save: function() {
         // TODO get rid of the id
         if(!$('#editor-conclusion').valid()) {
             return false;
         }
                      
         var argument = this.model.get('argument');
         
         // some attributes are changed dynamically with events
         // but the following are pulled from the view
         var conclusion =  this.model.get('conclusion').get('statement');

         argument.set('conclusion', conclusion);

         var self = this;
         argument.set('premises', this.model.get('premises').
                      filter(self.premise_candidate_exists).
                      map(function(premise_candidate) {
                              return premise_candidate.get('premise');
                          }));
         
         argument.set('exceptions', this.model.get('exceptions').
                      filter(self.premise_candidate_exists).
                      map(function(premise_candidate) {
                              return premise_candidate.get('premise');
                          }));
         
         var metadata = argument.get('header') || {};
         // merge new metadata with the old one
         _.extend(metadata, this.model.get('metadata').get('metadata').attributes);
         argument.set('header', metadata);

         if(this.model.get('scheme').get('scheme')) {
             var scheme = this.model.get('scheme').get('scheme').id;
             argument.set('scheme', '(' + scheme  + ')'); 
         }
         
         if(argument.save(null, 
                          {error: PM.on_model_error,
                           wait: true,
                           success : function() {
                               // manually redisplay argument page since it is not yet
                               // a backbone view
                               PM.arguments.fetch({success: function() {
                                                       AGB.display_argument(IMPACT.db, argument.id);
                                                   },
                                                   error: PM.on_model_error
                                                  });
                               
                           }})) {
             this.model = undefined;
             this.premises_candidates_view.remove();
             this.remove();
         }
         
         return false;
     },
     
     cancel: function() {
         // restores state of the argument
         this.model.restore(); 
         this.model.get('argument').restore();
         
         this.premises_candidates_view.cancel();

         this.premises_candidates_view.remove();
         this.remove();
         
         this.metadata_editor_view.remove();
         
         return false;
     } 


    }
);
