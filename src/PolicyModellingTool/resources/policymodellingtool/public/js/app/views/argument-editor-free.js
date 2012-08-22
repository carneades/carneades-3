// Subview for modifying an argument without a scheme
PM.ArgumentEditorFreeView = Backbone.View.extend(
    {className: "argument-editor-free",
     
     events: {
         "click .add-premise": "add_premise"
     },

     initialize: function() {
         this.model.on('change', this.render, this);
         this.model.get('premises').on('add', this.render, this);
         // this.model.get('premises').on('remove', this.render, this);
         _.bindAll(this, 'render');
     },

     render: function() {
         this.$el.html(ich.argumenteditor2free());
         
         var conclusioncandidateview = new PM.ConclusionCandidateView({model: this.model.get('conclusion')});
         conclusioncandidateview.render();
         this.$('.conclusion-candidate').html(conclusioncandidateview.$el);

         var self = this;
         this.model.get('premises').each(
             function(premise) {
                 premise.set('container', self.model.get('premises'));
                 var premisecandidateview = new PM.PremiseCandidateView({model: premise});
                 premisecandidateview.render();
                 self.$('.argument-premises').append(premisecandidateview.$el);
             });

         this.$el.append(ich.button({clazz: "add-premise",
                                     value: "Add a premise"}));
         
         return this;
     },
     
     add_premise: function() {
         var premisecandidate = new PM.PremiseCandidate(
             {statements: this.model.get('statements')});

         this.model.get('premises').add(premisecandidate); 
         return false;
     }
     
    }
);
