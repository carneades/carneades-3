// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

goog.provide('carneades.policy_analysis.web.markdown');

PM.markdown_to_html = function(md_text) {
    if(md_text == null) {
        return "";
    }
    var converter = Markdown.getSanitizingConverter(); 
    converter.hooks.chain("preConversion", PM.citation_to_url); 
    
    var html = converter.makeHtml(md_text);
    return html;  
};

AGB.markdown_to_html = PM.markdown_to_html;

/// Preconvertion hook for the Markdown converter, converting
/// citation of the type [@ref] or [@ref, p.13] to HTML links.
/// If the metadata of the element referenced by the @ref key
/// has a source it is linked to /carneadesws/documents/:project/:source
/// If the metadata of the element has an identifier beginning by http
/// it is linked to the identifier URL.
///
/// This means users can copy documents in their project documents folder
/// add a reference having a source to it and references them in their
/// arguments graphs
PM.citation_to_url = function(text) {
    return text.replace(/\[@([a-zA-Z0-9]+)[^\]]*\]/g, function (match, citation_key) {
        var metadata = PM.debate_metadata.filter(function(m) { 
            return m.get('key') == citation_key; 
        });
        
        if(metadata.length == 1) {
            if(metadata[0].get('source')) {
                var url = IMPACT.wsurl + '/documents/' 
                    + IMPACT.project + '/' 
                    + metadata[0].get('source');
                return '<a href="' + url + '" >' + match + '</a>';
            }
            
            if(metadata[0].get('identifier') &&
               metadata[0].get('identifier').indexOf('http') == 0) {
                var url = metadata[0].get('identifier');
                return '<a href="' + url + '" >' + match + '</a>';
            }
        }
        
        return match;
    });
};

