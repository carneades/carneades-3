
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
                argument_data.db = db;
                argument_data.header = {title : "One Title"};
                argument_data.conclusion.pro_text = argument_data.conclusion.pro ? "pro" : "con";
                argument_data.conclusion.statement_text = statement_text(argument_data.conclusion);
                set_premises_text(argument_data);
                var argument_html = ich.argument(argument_data);
                $('body').html(argument_html.filter('#argument'));
            });
}

function set_premises_text(argument_data)
{
    $.each(argument_data.premises, 
           function(index, premise) {
               premise.statement.statement_text = statement_text(premise.statement);
           });
}

