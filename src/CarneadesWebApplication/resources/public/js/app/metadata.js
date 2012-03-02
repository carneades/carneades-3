function get_string(data, is_last, escape)
{
    if(data == null || data == undefined) {
        return "";
    }
    return (escape ? escape_html(data) : data) + "." + (is_last ? "" : " ");
}

function format_metadata(metadata)
{
    if(metadata == null || metadata == undefined) {
        return "";    
    }
    var creator = metadata.creator;
    var date = metadata.date;
    var title = metadata.title;
    var identifier = metadata.identifier;
    var is_identifier_url = is_url(identifier);
    
     // makes a link if the identifier is an url
    title = is_identifier_url ? '<a href="{0}">{1}</a>'.format(identifier, title) : title;

    var formatted = get_string(creator, false, true) +
        get_string(date, false, true) +
        get_string(title, is_identifier_url, !is_identifier_url) +
        (is_identifier_url ? "" : get_string(identifier, true, true));
    
    return markdown_to_html(formatted);
}

function description_text(header)
{
    if(header) {
        return header.description ? markdown_to_html(header.description[CARNEADES.lang]) : "";        
    } else {
        return "";
    }
}
