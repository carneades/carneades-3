$(function() {
      $('#main-issues').ready(fill_argumentgraph);
});


function fill_argumentgraph() 
{
    do_ajax_get("/statement/" + CARNEADES.database, 
                function(statements) { 
                    var issues = statements.filter(function(stmt) { return item.main; });
                    for(var issue in issues) {
                        $('#main-issues').append(issue.atom);                        
                    }
                });
}

