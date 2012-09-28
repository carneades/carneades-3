// Displays a claim
PM.SctClaim = Backbone.View.extend(
    {className: "sct-claim",
     
     events: {
         "change input" : "input_changed"
     },
     
     initialize: function(attrs) {
         this.lang = attrs.lang;
         this.model.on('change', this.render, this);
         _.bindAll(this, 'render', 'input_changed');
     },
     
     render: function() {
         var data = this.model.toJSON();
         
         var claim_properties = 
             {'sct_claim': $.i18n.prop('sct_claim'),
              'sct_question': $.i18n.prop('sct_question'),
              'sct_question_text': $.i18n.prop('sct_question_text'),
              'sct_agree': $.i18n.prop('sct_agree'),
              'sct_disagree': $.i18n.prop('sct_disagree'),
              'sct_show_args': $.i18n.prop('sct_show_args'),
              'sct_skip_question': $.i18n.prop('sct_skip_question'),
              'sct_next': $.i18n.prop('sct_next'),
              'claim_text': data.text[this.lang]
             };
         
         var content = ich['sct-claim'](claim_properties);

         this.$el.html(content);
         
         return this;
     },
     
     input_changed: function() {
         return false;
     }
     
    }
);
