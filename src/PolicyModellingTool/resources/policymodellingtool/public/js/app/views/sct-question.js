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
         
         if(type == 'claim') {
             var claim_view = new PM.SctClaim({model: new PM.Statement(question.statement),
                                               lang: this.lang,
                                               el: this.el});
             claim_view.render();
         } else if(type == 'argument') {
             _.each(question.get('premises'),
                   function(premise) {
                       // content.add(
                       //     ich['sct-claim']
                       //     (_.extend(claim_properties, 
                       //               {'claim_text': premise.statement.text[this.lang]})));
                   });
         }
         
         // this.$el.html(content);
         
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
