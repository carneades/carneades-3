
function on_connect()
{
    CARNEADES.database = "db7"; // $('input[name=database]').val();
    CARNEADES.username = $('input[name=username]').val();
    CARNEADES.password = $('input[name=password]').val();

    fill_argumentgraph();

    return false; // do not make a POST request
}
