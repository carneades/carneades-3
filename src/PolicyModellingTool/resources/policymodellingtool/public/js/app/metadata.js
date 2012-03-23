PM.description_text = function(header)  {
    if(header) {
        return header.description ? markdown_to_html(header.description[IMPACT.lang]) : "";        
    } else {
        return "";
    }
};

