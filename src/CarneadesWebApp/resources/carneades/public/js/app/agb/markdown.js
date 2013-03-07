// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

AGB.markdown_to_html = function(md_text)
{
    if(md_text == null) {
        return "";
    }
    var converter = Markdown.getSanitizingConverter(); 
    var html = converter.makeHtml(md_text);
    // as a temporary fix, we remove <p> tags for sentence of one line
    // because of a problem
    // with the CSS styles
    // if(md_text.split('\n').length <= 2 &&
    //    html.slice(0, 3) == "<p>") {
    //     html = html.slice(3, -4);
    // }
    return html;
};
