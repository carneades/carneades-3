function markdown_to_html(md_text)
{
    if(md_text == null) {
        return "";
    }
    var converter = Markdown.getSanitizingConverter(); 
    return converter.makeHtml(md_text);
}
