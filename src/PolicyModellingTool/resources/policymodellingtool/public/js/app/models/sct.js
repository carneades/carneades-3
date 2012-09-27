// Data for the SCT
PM.Sct = Backbone.Model.extend(
    {defaults: function() {
         return {
             username: "",
             questions: []
         };
     },
     
     url: function() {
         return '/sct/';
     },
     
     initialize: function(attrs) {
         
     },
     
     push_question: function(question, type) {
         this.get('questions').push({question: question, type: type});
     },
     
     current_question: function() {
         return this.get('questions')[0];
     }
     
    }
);
