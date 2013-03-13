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
    return /^(http|https|ftp)/.test(url);
};
