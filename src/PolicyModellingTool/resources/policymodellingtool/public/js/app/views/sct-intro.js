// Displays the SCT introduction
PM.SctIntro = Backbone.View.extend(
    {className: "sct-intro",
     
     events: {
         "click .start": "start",
         "change .username": "username_changed"
     },
     
     initialize: function() {
         this.model.on('change', this.render, this);
         _.bindAll(this, 'render', 'start');
     },
     
     render: function() {
         var data = this.model.toJSON();

         var content = ich['sct-intro'](
             {sct_intro: PM.markdown_to_html($.i18n.prop('sct_intro')),
              sct_username: $.i18n.prop('sct_username'),
              sct_username_placeholder: $.i18n.prop('sct_username_placeholder'),
              sct_start: $.i18n.prop('sct_start'),
              sct_login: $.i18n.prop('sct_login')
             });
         
         this.$el.html(content);

         this.$('.username').val(data.username);
         
         return this;
     },
     
     start: function() {
         PM.set_sct_issues_url();
         return false;
     },
     
     username_changed: function() {
         this.model.set('username', this.$('.username').val());
     }
     
    }
);
