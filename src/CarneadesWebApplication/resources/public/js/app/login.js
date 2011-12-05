
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
                var argument_html = ich.argument(argument_data);
                $('body').html(argument_html.filter('#argument'));

                add_statements_handlers(get_statements(argument_data));
            });
}


// Returns the statements of the premises and of the conclusion
function get_statements(argument_data)
{
    var statements = [];
    $.each(argument_data.premises, 
           function(index, premise) {
               statements.push(premise.statement);
           });
    statements.push(argument_data.conclusion);
    return statements;
}


function add_statements_handlers(statements)
{
    $.each(statements, function(index, statement) {
               $('#statement' + statement.id).click(
                   function(obj) {
                       display_statement(statement.id);
                   });
           }
          );
}

function display_statement(id)
{
    ajax_get('/statement/' + CARNEADES.database + '/' + id,
            function(statement_data) {
                var statement_html = ich.statement(statement_data);
                $('body').html(statement_html.filter('#statement'));
            });
}