// Displays a claim and asks the user about it
PM.SctClaim = Backbone.View.extend(
    {className: "sct-claim",
     
     events: {
         "click button" : "on_next"
     },
     
     initialize: function(attrs) {
         this.lang = attrs.lang;
         // this.model.on('change', this.render, this);
         _.bindAll(this, 'render', 'on_next');
     },
     
     render: function() {

         var content = ich['sct-claim'](
         {'sct_claim': $.i18n.prop('sct_claim'),
          'claim_text': this.model.statement.text[this.lang],
          'sct_questions': $.i18n.prop('sct_questions'),
          'sct_questions_text': $.i18n.prop('sct_questions_text'),
          'sct_agree': $.i18n.prop('sct_agree'),
          'sct_disagree': $.i18n.prop('sct_disagree'),
          'sct_show_args': $.i18n.prop('sct_show_args'),
          'sct_skip_question': $.i18n.prop('sct_skip_question'),
          'sct_next': $.i18n.prop('sct_next')
         });
         
         this.$el.html(content);
         
         return this;
     },
     
     on_next: function() {
         var val = this.$('input:checked').val();
         
         if(val == 'show-arguments') {
             // TODO get pro / con arguments
             // add them to the SCT queue
             // display argument page
         }
         
         
         return false;
     }
     
    }
);
