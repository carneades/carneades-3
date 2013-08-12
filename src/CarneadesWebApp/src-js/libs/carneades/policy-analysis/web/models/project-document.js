// Copyright (c) 2013 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

goog.provide('carneades.policy_analysis.web.models.project_document');

PM.ProjectDocument = Backbone.Model.extend(
    {
        url: function() {
            return IMPACT.wsurl + '/documents/' + this.id;
        }
    }
);
