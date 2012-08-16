PM.PremiseCandidateView = Backbone.View.extend(
    {className: "premise-candidate",

     initialize: function() {
       this.model.bind('change', this.render, this);
     },

     render: function() {
         var data = this.model.toJSON();
         
         this.$el.html(ich.premisecandidate());
         
         var role = this.$el.find('.role-input');
         role.prop('disable', !data.editableRole);
         role.val(data.role);

         return this;
     }
    }
);
