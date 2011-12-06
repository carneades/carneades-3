
function on_connect()
{
    CARNEADES.database = "pierson-post3"; // $('input[name=database]').val();
    CARNEADES.username = $('input[name=username]').val();
    CARNEADES.password = $('input[name=password]').val();

    set_argument_url(CARNEADES.database, 1);
    
    return false; // do not make a POST request
}
