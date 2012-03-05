
function on_connect()
{
    CARNEADES.database = $('input[name=database]').val();
    // CARNEADES.username = $('input[name=username]').val();
    // CARNEADES.password = $('input[name=password]').val();

    set_argumentgraph_url(CARNEADES.database);
    
    return false; // do not make a POST request
}

function display_login()
{
    var login_html = ich.login();
    $('#browser').html(login_html.filter('#login'));
    $('#connect').click(on_connect);
}

