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
     },
     
     initialize: function() {
         // saves state of the argument candidate
         this.model.store(); 
         this.model.get('argument').store();
         
         this.model.on('change', this.render, this);
         this.model.get('scheme').on('change', this.scheme_changed, this);
         _.bindAll(this, 'render', 'cancel');
     },

     render: function() {
         var data = this.model.toJSON();
         
         this.$el.html(ich.argumenteditor2());

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
         
         this.scheme_candidate_view = new PM.SchemeCandidateView({model: this.model.get('scheme'),
                                                                  el: this.$('.scheme-candidate')});
         this.scheme_candidate_view.render();
         
         this.argumenteditorfreeview = new PM.ArgumentEditorFreeView({model: this.model});
         this.argumenteditorfreeview.render();
         this.$('.argument-editor-conclusion-and-premises').html(this.argumenteditorfreeview.$el);
         
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
     
     scheme_changed: function() {
         var scheme = this.model.get('scheme').get('scheme');
         var premises_candidates_views = this.argumenteditorfreeview.premises_candidates_views;
         var nb_premises = scheme.get('premises').length;
         
         // removes excedent premises
         for(var i = 0; i < premises_candidates_views.length; i++) {
             if(i >= nb_premises) {
                 premises_candidates_views[i].remove();
             }
         }

         // add required premises
         
         // set the role of the premises
         for(i = 0; i < scheme.get('premises').length; i++) {
             var premise = scheme.get('premises')[i];
             var current_premise = _.clone(premises_candidates_views[i].model.get('premise'));
             current_premise.role = premise.role;
             premises_candidates_views[i].model.set('premise', current_premise);
         }

             // set the filter to true?
             // builds a substitutions and stores it

         // TODO adjust the number of premises to the number of premises
         // in the scheme
         // then assign a 'role' for each premise, keep the previously selected statement
         
         // adjust or add the number of exceptions for the selected scheme
         // set the 'role' exception
         // keep the previously selected statements
     },
     
     save: function() {
         // // forces the update of data for events
         // // swallowed by markItUp and not captured by the view
         // this.metadata_editor_view.update_data();
             
         var argument = this.model.get('argument');
         
         // some attributes are changed dynamically with events
         // but the following are pulled from the view
         var conclusion =  this.model.get('conclusion').get('statement');

         argument.set('premises', this.model.get('premises').map(
                            function(premise_candidate) {
                                return premise_candidate.get('premise');
                            }));
         argument.set('conclusion', conclusion);
         
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
                               AGB.display_argument(IMPACT.db, argument.id);
                           }})) {
             this.model = undefined;
             this.argumenteditorfreeview.remove();
             this.remove();
         }
         
         return false;
     },
     
     cancel: function() {
         // restores state of the argument
         this.model.restore(); 
         this.model.get('argument').restore();
         
         this.argumenteditorfreeview.cancel();

         this.argumenteditorfreeview.remove();
         this.remove();
         
         this.metadata_editor_view.remove();
         
         return false;
     } 


    }
);
