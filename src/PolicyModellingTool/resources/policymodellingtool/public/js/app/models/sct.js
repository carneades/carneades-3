// Data for the SCT
PM.Sct = Backbone.Model.extend(
    {defaults: function() {
         return {
             username: "",
             questions: [],
             'current-question': undefined
         };
     },
     
     url: function() {
         return '/sct/';
     },
     
     initialize: function(attrs) {
         
     },
     
     push_question: function(question, type) {
         var questions = this.get('questions');
         questions.push({question: question, type: type});
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
     
     // Push the argument of the current question (statement)
     push_arguments: function() {
         var current = this.current_question().question;
         var args_id = [].concat(current.pro, current.con);
         
         var self = this;
         var args = _.map(args_id,
                              function(id) {
                                  return self.get('arguments').get(id);
                              });

         _.each(args,
               function(arg) {
                   self.push_question(arg, 'argument');
               });
     }
     
    }
);
