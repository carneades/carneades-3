
function set_description_text(info)
{
    if(info.header) {
        info.description_text = info.header.description ? 
            markdown_to_html(info.header.description[CARNEADES.lang]) : "";        
    } else {
        info.description_text = "";
    }
}
