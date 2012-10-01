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
         var self = this;
         var question_data = this.model.get('current-question');
         var question = question_data.question;
         var type = question_data.type;
         
         if(type == 'claim') {
             var claim_view = new PM.SctClaim({model: new PM.Statement(question.statement),
                                               lang: this.lang,
                                               el: this.el});
             claim_view.render();
         } else if(type == 'argument') {
             self.$el.html(ich['sct-argument']
                           ({'sct_argument': $.i18n.prop('sct_argument'),
                             'sct_argument_text': this.get_description(question),
                             'sct_premises': $.i18n.prop('sct_premises')
                            }));

             // self.$el.append('<ol class="sct-premises">');

             _.each(question.get('premises'),
                    function(premise) {
                        var claim_view = 
                            new PM.SctClaim({model: new PM.Statement(premise.statement),
                                             lang: self.lang,
                                             hide_title: true});
                        
                        claim_view.render();
                        // self.$el.append('<li>');
                        self.$el.append(claim_view.$el.html());
                        // self.$el.append('</li>');
                   });

             // self.$el.append('</ol>');
         }
         
          this.$el.append(ich.button2({label: $.i18n.prop('sct_next')}));
         
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
     },
     
     get_description: function(question) {
         return AGB.description_text(question.get('header')).replace(/\[@.+\]/, "");    
     }
     
     
    }
);
