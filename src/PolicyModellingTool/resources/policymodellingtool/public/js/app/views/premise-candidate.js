PM.PremiseCandidateView = Backbone.View.extend(
    {className: "premise-candidate",
     
     events: {
         "change input": "role_changed"
     },
     
     initialize: function() {
         this.model.bind('change', this.render, this);
         _.bindAll(this, 'role_changed');
     },

     render: function() {
         var data = this.model.toJSON();
         
         this.$el.html(ich.premisecandidate());
         
         var role = this.$el.find('.role-input');
         role.prop('disabled', !data.editableRole);
         role.val(data.role);
         
         // TODO display list of statement

         return this;
     },
     
     role_changed: function() {
         this.model.set('role', $('.role-input').val());
     }
    }
);
