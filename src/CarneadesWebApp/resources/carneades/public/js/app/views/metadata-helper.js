// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

PM.set_metadata_has_properties = function(header) {
    if(_.isNil(header)) {
        return;
    }
    
    // hasdescription = header.description &&
    //     header.description[IMPACT.lang] ? true : false;
    header.header_haskey = header.key ? true : false;
    header.header_hascontributor = header.contributor ? true : false;
    header.header_hascoverage = header.coverage ? true : false;
    header.header_hascreator = header.creator ? true : false;
    header.header_hasdate = header.date ? true : false;
    header.header_hasformat = header.format ? true : false;
    header.header_hasidentifier = header.identifier ? true : false;
    header.header_haslanguage = header.language ? true : false;
    header.header_haspublisher = header.publisher ? true : false;
    header.header_hasrelation = header.relation ? true : false;
    header.header_hasrights = header.rights ? true : false;
    header.header_hassource = header.source ? true : false;
    header.header_hassubject = header.subject ? true : false;
    header.header_hastitle = header.title ? true : false;
    header.header_hastype = header.type ? true : false;  
};

PM.has_description = function(header) {
    return header.description && header.description[IMPACT.lang];
};
