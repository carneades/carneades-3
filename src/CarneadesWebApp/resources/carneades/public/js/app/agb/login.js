// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1


AGB.on_connect = function()
{
    CARNEADES.database = $('input[name=database]').val();
    // CARNEADES.username = $('input[name=username]').val();
    // CARNEADES.password = $('input[name=password]').val();

    AGB.set_argumentgraph_url(CARNEADES.database);
    
    return false; // do not make a POST request
};

AGB.display_login = function()
{
    var login_html = ich.login();
    $('#browser').html(login_html.filter('#login'));
    $('#connect').click(AGB.on_connect);
};

