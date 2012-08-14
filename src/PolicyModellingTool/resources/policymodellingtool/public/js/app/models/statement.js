PM.Statement = Backbone.Model.extend(
    {url: function() {
         return IMPACT.wsurl + '/statement/' + IMPACT.db;
     },

     initialize: function() {
         
     },
     
     validate: function(attributes) {
         if(_.isNil(attributes.text)) {
             return "Text attribute is missing";
         }
         
         if(_.isNil(attributes.text.en)) {
             return "English text attribute is missing";
         }
         
         if(_.isNil(attributes.standard)) {
             return "Standard attribute is missing";
         }
         
         return undefined;
     }
     
    }
);

