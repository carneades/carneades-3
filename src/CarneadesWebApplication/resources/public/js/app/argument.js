
function argument_url(db, argid)
{
    return '/argument/' + db + '/' + argid;
}

function set_argument_url(db, argid)
{
    $.address.value(argument_url(db, argid));
}

function display_argument(db, argid)
{
    ajax_get(argument_url(db, argid),
            function(argument_data) {
                argument_data.direction = argument_data.pro ? "pro" : "con";
                var argument_html = ich.argument(argument_data);
                $('body').html(argument_html.filter('#argument'));

                add_statements_handlers(db, get_statements(argument_data));
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


function add_statements_handlers(db, statements)
{
    $.each(statements, function(index, statement) {
               $('#statement' + statement.id).click(
                   function(obj) {
                       set_statement_url(db, statement.id);
                   });
           }
          );
}
