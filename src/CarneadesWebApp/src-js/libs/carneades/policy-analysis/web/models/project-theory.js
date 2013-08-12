// Copyright (c) 2013 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

goog.provide('carneades.policy_analysis.web.models.project_theory');

PM.ProjectTheory = Backbone.Model.extend(
    {
     url: function() {
         return IMPACT.wsurl + '/project/' + this.id + '/theories';
     }
    }
);
