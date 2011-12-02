
function on_connect()
{
    CARNEADES.database = "db8"; // $('input[name=database]').val();
    CARNEADES.username = $('input[name=username]').val();
    CARNEADES.password = $('input[name=password]').val();

    display_argument(1);
    
    return false; // do not make a POST request
}

function display_argument(argid)
{
    ajax_get('/argument/' + CARNEADES.database + '/' + argid,
            function(argument_data) {
                argument_data.direction = argument_data.pro ? "pro" : "con";
                $('body').replaceWith(ich.argument(argument_data));
            });
}

