function set_argumentgraph_url(db)
{
    $.address.value(argumentgraph_url(db));    
}

function argumentgraph_url(db)
{
    return '/argumentgraph/' + db;
}

// function fill_argumentgraph() 
// {
//     do_ajax_get("/statement/" + CARNEADES.database, 
//                 function(statements) { 
//                     var issues = statements.filter(function(stmt) { return item.main; });
//                     for(var issue in issues) {
//                         $('#main-issues').append(issue.atom);                        
//                     }
//                 });
// }

function display_argumentgraph(db)
{
    ajax_get('/metadata/' + db + '/1',
            function(metadata) {
                var metadata_string = format_metadata(metadata);
                var argumentgraph_html = ich.argumentgraph({metadata_text : metadata_string});
                $('body').html(argumentgraph_html.filter('#argumentgraph'));
            });
}