// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

goog.provide('carneades.policy_analysis.web.metadata');

PM.description_text = function(header)  {
    if(header) {
        return header.description ? PM.markdown_to_html(header.description[IMPACT.lang]) : "";
    } else {
        return "";
    }
};

PM.title_text = function (header) {
    if(header && header.title) {
        return header.title;
    }

    return "";
};
