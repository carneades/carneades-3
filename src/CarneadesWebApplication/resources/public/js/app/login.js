
function on_connect()
{
    CARNEADES.database = "db8"; // $('input[name=database]').val();
    CARNEADES.username = $('input[name=username]').val();
    CARNEADES.password = $('input[name=password]').val();

    
    var argument_data = {header : "Titi, Toto"};
    var arg = ich.argument(argument_data);

    $('#arg').append(arg);

    return false; // do not make a POST request
}
