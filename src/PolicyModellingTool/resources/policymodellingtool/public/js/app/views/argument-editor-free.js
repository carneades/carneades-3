// Subview for modifying an argument without a scheme
PM.ArgumentEditorFreeView = Backbone.View.extend(
    {className: "argument-editor-free",

     initialize: function() {
         this.model.on('change', this.render, this);
         _.bindAll(this, 'render');
     },

     render: function() {
         var data = this.model.toJSON();
         
         this.$el.html(ich.argumenteditor2free());
         
         var conclusioncandidateview = new PM.ConclusionCandidateView({model: this.model.get('conclusion')});
         conclusioncandidateview.render();
         this.$('.conclusion-candidate').html(conclusioncandidateview.$el);
         
         // TODO uses select2.js to display conclusion
         // attaches a listener to the conclusion to reupdate the display
         // uses premiseview to display the existing premise of the statement
         // adds a button to add new premises
         // adds buttons to remove premises
         
         return this;
     }
     
    }
);
