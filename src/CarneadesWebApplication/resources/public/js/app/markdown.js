function markdown_to_html(md_text)
{
    var converter = Markdown.getSanitizingConverter(); 
    return converter.makeHtml(md_text);
}
