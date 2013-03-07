// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

PM.description_text = function(header)  {
    if(header) {
        return header.description ? markdown_to_html(header.description[IMPACT.lang]) : "";        
    } else {
        return "";
    }
};

