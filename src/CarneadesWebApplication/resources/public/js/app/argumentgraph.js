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
    ajax_get('/metadata/' + db + '/1',
            function(metadata) {
                ajax_get('/main-issues/' + db,
                         function(mainissues) {
                             var metadata_string = format_metadata(metadata);
                             set_mainissues_text(mainissues);
                             var argumentgraph_html = ich.argumentgraph({db : db,
                                                                         metadata_text : metadata_string,
                                                                         title : metadata.title,
                                                                         mainissues : mainissues});
                             $('body').html(argumentgraph_html.filter('#argumentgraph'));                    
                         });
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
