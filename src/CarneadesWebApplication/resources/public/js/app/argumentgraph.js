function set_argumentgraph_url(db)
{
    $.address.value(argumentgraph_url(db));    
}

function argumentgraph_url(db)
{
    return '/argumentgraph/' + db;
}

function display_argumentgraph(db)
{
    ajax_get('/argumentgraph-info/' + db,
             function(data) {
                 data.normalize();
                 data.metadata_text = format_metadata(data.metadata[0]);
                 set_mainissues_text(data.main_issues);
                 data.references = data.metadata.filter(function (ref) { return ref.key; });
                 set_references_text(data.references);
                 data.title = markdown_to_html(data.metadata[0].title);
                 var argumentgraph_html = ich.argumentgraph(data);
                 $('body').html(argumentgraph_html.filter('#argumentgraph'));                    
             });
}

function set_mainissues_text(mainissues)
{
    $.each(mainissues, 
           function(index, issue) {
               issue.statement_text = statement_text(issue);
           });
    return mainissues;
}

function set_references_text(metadata)
{
    $.each(metadata, 
           function(index, md) {
               md.metadata_text = format_metadata(md);
           });
}
