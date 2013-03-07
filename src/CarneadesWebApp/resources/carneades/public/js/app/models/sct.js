// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

// Data for the SCT
PM.Sct = Backbone.Model.extend(
    {defaults: function() {
         return {
             username: this.generate_password(10),
             questions: [],
             'current-question': undefined
         };
     },
     
     generate_password: function(limit, inclNumbers) {
         var vowels = 'aeiou'.split('');
         var constonants = 'bcdfghjklmnpqrstvwxyz'.split('');
         var word = '', i, num;

         if (!limit) limit = 8;

         for (i = 0; i < (inclNumbers ? limit - 3 : limit); i++) {
             if (i % 2 == 0) { // even = vowels
                 word += vowels[Math.floor(Math.random() * 4)]; 
             } else {
                 word += constonants[Math.floor(Math.random() * 20)];
             } 
         }

         if (inclNumbers) {
             num = Math.floor(Math.random() * 99) + '';
             if (num.length == 1) num = '00' + num;
             else if (num.length == 2) num = '0' + num;
             word += num;
         }

         return word.substr(0, limit);
     },
     
     url: function() {
         return '/sct/';
     },
     
     initialize: function(attrs) {
         
     },
     
     push_question: function(question, type, seen) {
         var questions = this.get('questions');
         questions.push({question: question, type: type, seen: seen});
     },

     pop_question: function() {
         this.get('questions').pop(); 
     },
     
     update_current_question: function() {
         this.set('current-question', this.current_question());
     },
     
     current_question: function() {
         var questions = this.get('questions'); 
         return questions[questions.length - 1];
     },
     
     has_question: function() {
         return this.get('questions').length != 0;
     },
     
     // Push the argument of the statement in the question stack
     push_arguments: function(statement) {
         var args_id = [].concat(statement.pro, statement.con);
         
         var self = this;
         var args = _.map(args_id,
                              function(id) {
                                  return self.get('arguments').get(id);
                              });

         _.each(args,
               function(arg) {
                   self.push_question(arg.toJSON(), 'argument');
               });
     }
     
    }
);
