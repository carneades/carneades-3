AGB.get_string = function(data, is_last, escape)
{
    if(data == null || data == undefined) {
        return "";
    }
    return (escape ? AGB.escape_html(data) : data) + "." + (is_last ? "" : " ");
};

AGB.format_metadata = function(metadata)
{
    if(_.isNil(metadata)) {
        return "";    
    }
    var creator = metadata.creator ? metadata.creator[0] : null;
    var date = metadata.date ? metadata.date[0] : null;
    var title = metadata.title ? metadata.title[0] : null;
    var identifier = metadata.identifier ? metadata.identifier[0] : null;
    var is_identifier_url = AGB.is_url(identifier);
    
     // makes a link if the identifier is an url
    title = is_identifier_url ? '<a href="{0}">{1}</a>'.format(identifier, title) : title;

    var formatted = AGB.get_string(creator, false, true) +
        AGB.get_string(date, false, true) +
        AGB.get_string(title, is_identifier_url, !is_identifier_url) +
        (is_identifier_url ? "" : AGB.get_string(identifier, true, true));
    
    return AGB.markdown_to_html(formatted);
};

// TODO there is a mismatch between metadata with elements represented
// as vectors and metadata with elements represented with strings!!
// this should be fixed in the service.clj, in the statement/argument editor
// and in the argumentgraph page.
AGB.format_linear_metadata = function(metadata)
{
    if(_.isNil(metadata)) {
        return "";    
    }
    var creator = metadata.creator;
    var date = metadata.date;
    var title = metadata.title;
    var identifier = metadata.identifier;
    var is_identifier_url = AGB.is_url(identifier);
    
     // makes a link if the identifier is an url
    title = is_identifier_url ? '<a href="{0}">{1}</a>'.format(identifier, title) : title;

    var formatted = AGB.get_string(creator, false, true) +
        AGB.get_string(date, false, true) +
        AGB.get_string(title, is_identifier_url, !is_identifier_url) +
        (is_identifier_url ? "" : AGB.get_string(identifier, true, true));
    
    return AGB.markdown_to_html(formatted);
};

AGB.description_text = function(header)
{
    if(header) {
        return header.description ? AGB.markdown_to_html(header.description[IMPACT.lang]) : "";        
    } else {
        return "";
    }
};
