// Copyright (c) 2013 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

goog.provide('carneades.policy_analysis.web.models.argument_info');

PM.ArgumentInfo = Backbone.Model.extend(
    {url: function() {
         return IMPACT.wsurl + '/argument-info/' + IMPACT.project + '/' + IMPACT.db;
     }
     
    }
);

