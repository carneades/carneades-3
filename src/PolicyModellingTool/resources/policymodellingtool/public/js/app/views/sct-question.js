// Displays a claim and asks the user about it
PM.SctQuestion = Backbone.View.extend(
    {className: "sct-question",
     
     events: {
         "click button" : "on_next"
     },
     
     initialize: function(attrs) {
         this.lang = attrs.lang;
         this.model.on('change', this.render, this);
         _.bindAll(this, 'render', 'on_next');
     },
     
     render: function() {
         var question_data = this.model.get('current-question');
         var question = question_data.question;
         var type = question_data.type;
         
         var content;
         
         if(type == 'claim') {
             content = ich['sct-claim'](
                 {'sct_claim': $.i18n.prop('sct_claim'),
                  'claim_text': question.statement.text[this.lang],
                  'sct_question': $.i18n.prop('sct_question'),
                  'sct_question_text': $.i18n.prop('sct_question_text'),
                  'sct_agree': $.i18n.prop('sct_agree'),
                  'sct_disagree': $.i18n.prop('sct_disagree'),
                  'sct_show_args': $.i18n.prop('sct_show_args'),
                  'sct_skip_question': $.i18n.prop('sct_skip_question'),
                  'sct_next': $.i18n.prop('sct_next')
                 });
         } else if(type == 'argument') {
             alert('argument');
             console.log(question);
         }         
         
         this.$el.html(content);
         
         return this;
     },
     
     on_next: function() {
         var val = this.$('input:checked').val();

         // TODO updates scores
         if(val == 'agree') {
             this.model.pop_question();
         } else if(val == 'disagree') {
             this.model.pop_question();
         } else if(val == 'show-arguments') {
             this.model.push_arguments();
         } else if(val == 'skip-question') {
             this.model.pop_question();
         }
         
         if(this.model.has_question()) {
             this.model.update_current_question();
         } else {
             alert('finito!');
         }

         return false;
     }
     
    }
);
