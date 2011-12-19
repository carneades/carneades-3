
function set_description_text(info)
{
    if(info.header) {
        info.description_text = info.header.description ? info.header.description['en'] : "";        
    } else {
        info.description_text = "";
    }
}
