// Displays a claim
PM.SctClaim = Backbone.View.extend(
    {className: "sct-claim",
     
     events: {
         "change input" : "input_changed"
     },

     initialize: function(attrs) {
         this.seen = attrs.seen,
         this.embedded = attrs.embedded; // embedded in the argument page
         this.lang = attrs.lang;
         this.sct = attrs.sct;
         this.model.on('change', this.render, this);
         _.bindAll(this, 'render', 'input_changed');
     },

     render: function() {
         var data = this.model.toJSON();
         
         var pro = this.get_agreed_pro();
         this.set_argument_texts(pro);
         
         var con = this.get_agreed_con();
         this.set_argument_texts(con);
         
         var claim_properties = 
             {'sct_claim': $.i18n.prop('sct_claim'),
              'sct_question': $.i18n.prop('sct_question'),
              'sct_question_text': $.i18n.prop('sct_question_text'),
              'sct_agree': $.i18n.prop('sct_agree'),
              'sct_disagree': $.i18n.prop('sct_disagree'),
              'sct_show_args': $.i18n.prop('sct_show_args'),
              'sct_skip_question': $.i18n.prop('sct_skip_question'),
              'sct_next': $.i18n.prop('sct_next'),
              'sct_seen': $.i18n.prop('sct_seen'),
              'sct_ask_agree': $.i18n.prop('sct_ask_agree'),
              'sct_pro_arguments': $.i18n.prop('sct_pro_arguments'),
              'sct_con_arguments': $.i18n.prop('sct_con_arguments'),
              'sct_weak': $.i18n.prop('sct_weak'),
              'sct_strong': $.i18n.prop('sct_strong'),
              'claim_text': data.text[this.lang],
              'has_arg': pro.length > 0 || con.length > 0,
              'has_pro': pro.length > 0,
              'has_con': con.length > 0,
              'pro': pro,
              'con': con,
              'id': this.cid
             };

         var content = ich['sct-claim'](claim_properties);
         
         this.$el.html(content);
         if(this.embedded) {
             this.$('h2').attr('hidden', true);
             this.$('input:last').attr('checked', true);
         } else {
             this.$('input[type=radio]:first').attr('checked', true);
         }
         
         if(data.pro.length == 0 && data.con.length == 0) {
             this.$(".show-arguments").remove();
         }
         
         if(!this.seen) {
             this.$(".seen").attr('hidden', true);
         } else {
             this.$(".show-arguments").remove();
         }
       
         return this;
     },

     input_changed: function() {
         return false;
     },
     
     get_agreed_pro: function() {
         var self = this;
         
         var pros = _.map(this.model.get('pro'),
                          function(id) {
                              return PM.debate_arguments.get(id);
                          });
         
         return _.filter(pros,
                         function(arg) {
                             return arg.is_agreed(self.sct.get('statement-poll').get('votes'));
                         }
                        );
     },
     
     get_agreed_con: function() {
         var self = this;
         
         var cons = _.map(this.model.get('con'),
                          function(id) {
                              return PM.debate_arguments.get(id);
                          });
         
         return _.filter(cons,
                         function(arg) {
                             return arg.is_agreed(self.sct.get('statement-poll').get('votes'));
                         }
                        );
     },

     
     set_argument_texts: function(args) {
         _.each(args,
               function(arg) {
                   arg.argument_text = AGB.argument_text(arg);
                   arg.description_text = AGB.description_text(arg.get('header'));
               });
     }

    }
);
