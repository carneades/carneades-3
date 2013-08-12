// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

goog.provide('carneades.policy_analysis.web.agb.agb_utils'); 

// replaces the object properties names with minus (-) to properties names with underscore
AGB.normalize = function(o) {
    for(name in o) {
        if(typeof o[name] !== 'function') {
            var normalized = name.replace(/-/g, '_');
            if(normalized !== name) {
                o[normalized] = o[name];
                delete o[name];                            
            }
        }
    }
};

AGB.escape_html = function(text)
{
    return $('<div/>').text(text).html();
};

AGB.is_url = function(url)
{
    return /^(http|https|ftp|file)/.test(url);
};

PM.on_edit = function () {
    return $.address.queryString().indexOf('edit=true') != -1;
}

PM.on_statement_entity = function () {
    return $.address.queryString().indexOf('entity=statement') != -1;
}

PM.on_argument_entity = function () {
    return $.address.queryString().indexOf('entity=argument') != -1;
}

PM.on_statement_edit = function () {
    return PM.on_edit() && PM.on_statement_entity();
}

PM.on_argument_edit = function () {
    return PM.on_edit() && PM.on_argument_entity();
}

PM.on_statement_delete = function () {
    return $.address.queryString().indexOf('delete=true') != -1;
}

PM.on_argument_delete =  function () {
    return $.address.queryString().indexOf('delete=true') != -1;
}
