// Copyright (c) 2013 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

goog.provide('carneades.policy_analysis.web.collections.projects');


goog.require('carneades.policy_analysis.web.models.project');

PM.Projects = Backbone.Collection.extend(
    {model: PM.Project,

     url: function() {
         return IMPACT.wsurl + '/project';
     }                                                         
    }
    
);
