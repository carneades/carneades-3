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
                 var metadata = data.metadata;
                 var mainissues = data.main_issues;
                 var metadata_string = format_metadata(metadata[0]);
                 set_mainissues_text(mainissues);
                 var references = metadata.filter(function (ref) { return ref.key; });
                 set_references_text(references);
                 var title = markdown_to_html(metadata[0].title);
                 var argumentgraph_html = ich.argumentgraph({db : db,
                                                             metadata_text : metadata_string,
                                                             title : title,
                                                             mainissues : mainissues,
                                                             references : references});
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
