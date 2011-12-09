
function on_connect()
{
    CARNEADES.database = "pierson7"; // $('input[name=database]').val();
    CARNEADES.username = $('input[name=username]').val();
    CARNEADES.password = $('input[name=password]').val();

    set_argumentgraph_url(CARNEADES.database);
    
    return false; // do not make a POST request
}
