 // Displays a claim or an argument and asks the user about it
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
          var seen = question_data.seen;

          if(type == 'claim') {
              var claim_view = new PM.SctClaim({model: new PM.Statement(question),
                                                sct: this.model,
                                                lang: this.lang,
                                                seen: seen,
                                                el: this.el});
              claim_view.render();
          } else if(type == 'argument') {
              self.$el.html(ich['sct-argument']
                            ({'sct_argument': $.i18n.prop('sct_argument'),
                              'sct_argument_text': this.get_description(question),
                              'sct_premises': $.i18n.prop('sct_premises')
                             }));

              // self.$el.append('<ol class="sct-premises">');

              _.each(question.premises,
                     function(premise) {
                         var claim_view = 
                             new PM.SctClaim({model: new PM.Statement(premise.statement),
                                              lang: self.lang,
                                              embedded: true,
                                              sct: PM.sct});

                         claim_view.render();
                         self.$el.append(claim_view.$el);
                         self.$el.append('<br/>');
                    });

              // self.$el.append('</ol>');
          }

           this.$el.append(ich.button2({label: $.i18n.prop('sct_next')}));

          // this.$el.html(content);

          return this;
      },

      // Parse the answers, push arguments if 'show me the arguments' is
      // selected and updates the statement polls or argument polls votes
      // in the database
      parse_answers: function() {
          var self = this;

          var question_data = this.model.get('current-question');
          var type = question_data.type;
          var question = question_data.question;
          var seen = question_data.seen;

          var poll;
          if(type == 'claim') {
              var val = this.$('input:checked').val();
              var score = val == 'agree' ? 1.0 : 0.0;

              poll = this.model.get('statement-poll');
              this.set_score(poll, question.id, score);

              var arg_poll = this.model.get('argument-poll');

              var weights = this.$('input[type=range]:not(:hidden)');
              _.each(weights,
                    function(weight) {
                       weight = $(weight);
                       var id = weight.attr('id').substr(6);
                       var val = parseFloat(weight.val());

                       self.set_score(arg_poll, id, val);
                    });

              arg_poll.save(null, {wait: true});

          } else if(type == 'argument' && seen) {
              var val = this.$('input:checked').val();
              var score = val == 'agree' ? 1.0 : 0.0;

              poll = this.model.get('argument-poll'); 
              this.set_score(poll, question.id, score);

          } else if(type == 'argument' && !seen) {
              // we save in the statement poll, since we 
              // save the votes for the premises
              poll = this.model.get('statement-poll');

              var premises = question.premises;
              var answers = this.$('input:checked');
              var statements = this.model.get('statements');

              _.each(premises,
                    function(premise, index) {
                        var answer = $(answers[index]).val();
                        if(answer == 'agree') {
                            self.set_score(poll, premise.statement.id, 1.0);
                        } else if(answer == 'disagree') {
                            self.set_score(poll, premise.statement.id, 0.0);
                        } else if(answer == 'show-arguments') {
                            self.model.push_question(premise.statement, 'claim', true); 
                            self.model.push_arguments(statements.get(premise.statement.id).toJSON());
                        }
                    });
          }

          poll.save(null, {wait: true});
      },

      set_score: function(poll, id, score) {
          var votes = poll.get('votes');
          votes[id] = score;
          poll.set('votes', votes); 
      },

      on_next: function() {
          var val = this.$('input:checked').val();
          var question_data = this.model.get('current-question');
          var type = question_data.type;

          if(type == 'claim') {
              if(val == 'show-arguments') {
                  this.model.current_question().seen = true;
                  this.model.push_arguments(
                      this.model.current_question().question); 
              } else if(val == 'skip-question') {
                  this.model.pop_question();
              } else {
                  this.model.pop_question();
                  this.parse_answers(); 
              }
          } else if(type == 'argument') {
              this.model.pop_question();
              this.parse_answers();
          }

          if(this.model.has_question()) {
              this.model.update_current_question();
          } else {
              PM.set_sct_summary_url();
          }

          return false;
      },

      get_description: function(question) {
          return AGB.description_text(question.header).replace(/\[.+\]/, "");    
      }

     }
 );
