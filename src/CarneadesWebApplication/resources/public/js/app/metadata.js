function get_string(data, is_last, no_escape)
{
    if(data == null) {
        return "";
    }
    return (no_escape ? data : escape_html(data)) + "." + (is_last ? "" : " ");
}

function format_metadata(metadata)
{
    var creator = metadata.creator;
    var date = metadata.date;
    var title = metadata.title;
    var identifier = metadata.identifier;
    var is_identifier_url = is_url(identifier);
    
    identifier = is_identifier_url ? '<a href="{0}">{0}</a>'.format(identifier) : identifier;

    return get_string(creator, false, false) +
        get_string(date, false, false) +
        get_string(title, false, false) +
        get_string(identifier, true, is_identifier_url);
}

