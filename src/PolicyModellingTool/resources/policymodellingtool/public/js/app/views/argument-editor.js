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
         "change .weight-input": "weight_changed"
     },
     
     initialize: function() {
         // saves state of the argument candidate
         this.model.store(); 
         this.model.get('argument').store();
         
         this.model.on('change', this.render, this);
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
         
         var scheme_candidate_view = new PM.SchemeCandidateView({model: this.model.get('scheme')});
         scheme_candidate_view.render();
         this.$('.scheme-candidate').html(scheme_candidate_view.$el);
         
         //if(this.argumenteditorview) {
             //this.argumenteditorview.remove();
         //}
         
         this.argumenteditorfreeview = new PM.ArgumentEditorFreeView({model: this.model});
         this.argumenteditorfreeview.render();
         this.$('#argument-editor-conclusion-and-premises').html(this.argumenteditorfreeview.$el);

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
     
     save: function() {
         var argument = this.model.get('argument');
         
         // some attributes are changed dynamically with events
         // but the following are pulled from the view
         var conclusion =  this.model.get('conclusion').get('statement');

         argument.set('premises', this.model.get('premises').map(
                            function(premise_candidate) {
                                return premise_candidate.get('premise');
                            }));
         argument.set('conclusion', conclusion);

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
         return false;
     } 


    }
);
