// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

PM.Statement = Backbone.Model.extend(
    {defaults: {
         text: {en: ""},
         standard: "pe"
     },

     url: function() {
         return IMPACT.wsurl + '/statement/' + IMPACT.db;
     },
     
     validate: function(attrs) {
         if(_.isNil(attrs.text)) {
             return "Text attribute is missing";
         }
         
         if(_.isNil(attrs.standard)) {
             return "Standard attribute is missing";
         }
         
         return undefined;
     }
     
    }
);

