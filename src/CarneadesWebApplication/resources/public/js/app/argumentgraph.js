function fill_argumentgraph() 
{
    do_ajax_get("/statement/" + CARNEADES.database + "/1", function(data) { alert("ok"); });
}

