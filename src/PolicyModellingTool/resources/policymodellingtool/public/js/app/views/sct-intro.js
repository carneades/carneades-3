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
             {sct_purpose_intro: $.i18n.prop('sct_purpose_intro'),
              sct_first_purpose: $.i18n.prop('sct_first_purpose'),
              sct_second_purpose: $.i18n.prop('sct_second_purpose'),
              sct_steps_intro: $.i18n.prop('sct_steps_intro'),
              sct_step_1: $.i18n.prop('sct_step_1'),
              sct_step_2: $.i18n.prop('sct_step_2'),
              sct_step_3: $.i18n.prop('sct_step_3'),
              sct_step_4: $.i18n.prop('sct_step_4'),
              sct_step_5: $.i18n.prop('sct_step_5'),
              sct_step_6: $.i18n.prop('sct_step_6'),
              sct_procedure: $.i18n.prop('sct_procedure'),
              sct_login_instruction: $.i18n.prop('sct_login_instruction'),
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
