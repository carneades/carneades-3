// Subview for modifying an argument without a scheme
PM.ArgumentEditorFreeView = Backbone.View.extend(
    {className: "argument-editor-free",

     initialize: function() {
         this.model.on('change', this.render, this);
         _.bindAll(this, 'render');
     },

     render: function() {
         this.$el.html(ich.argumenteditor2free());
         
         var conclusioncandidateview = new PM.ConclusionCandidateView({model: this.model.get('conclusion')});
         conclusioncandidateview.render();
         this.$('.conclusion-candidate').html(conclusioncandidateview.$el);
         
         // uses premiseview to display the existing premise of the statement
         // adds a button to add new premises
         // adds buttons to remove premises
         var self = this;
         this.model.get('premises').each(
             function(premise) {
                 var premisecandidateview = new PM.PremiseCandidateView({model: premise});
                 premisecandidateview.render();
                 self.$('.argument-premises').append(premisecandidateview.$el);
             });
         
         return this;
     }
     
    }
);
