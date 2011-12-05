
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
                var argument_html = ich.argument(argument_data);
                $('body').html(argument_html.filter('#argument'));
            });
}
