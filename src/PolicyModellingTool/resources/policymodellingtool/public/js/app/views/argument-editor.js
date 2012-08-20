// View for editing an argument
PM.ArgumentEditorView = Backbone.View.extend(
    {className: "argument-editor",
     
     events: {
         
     },
     
     initialize: function() {
         this.model.on('change', this.render, this);
         _.bindAll(this, 'render');
     },

     render: function() {
         var data = this.model.toJSON();
         
         this.$el.html(ich.argumenteditor2());

         if(data.pro) {
             this.pro_el().attr('checked',true);
         } else {
             this.con_el().attr('checked', true);
         }
         
         if(data.strict) {
             this.strict_yes_el().attr('checked', true);
         } else {
             this.strict_no_el().attr('checked', true);
         }

         this.$('.weight-input').val(data.weight);
         
         var argumenteditorfreeview = new PM.ArgumentEditorFreeView({model: this.model});
         argumenteditorfreeview.render();
         this.$('#argument-editor-conclusion-and-premises').html(argumenteditorfreeview.$el);

         return this;
     },
     
     pro_el: function() {
         return this.$('input:radio[name=pro]').filter('[value=pro]');
     },
     
     con_el: function() {
         return this.$('input:radio[name=pro]').filter('[value=con]');
     },
     
     strict_yes_el: function() {
         return this.$('input:radio[name=strict]').filter('[value=yes]');
     },
     
     strict_no_el: function() {
         return this.$('input:radio[name=strict]').filter('[value=no]');
     }
     
    }
);
