
function statement_url(db, stmtid)
{
    return 'statement/' + db + '/' + stmtid;
}

function set_statement_url(db, stmtid)
{
    $.address.value(statement_url(db, stmtid));
}

function display_statement(db, stmtid)
{
    ajax_get(statement_url(db, stmtid),
            function(statement_data) {
                statement_data.db = db;
                statement_data.statement_text = statement_text(statement_data);
                var statement_html = ich.statement(statement_data);
                $('body').html(statement_html.filter('#statement'));
            });
}

function statement_text(statement)
{
    if(statement.text) {
        return statement.text[CARNEADES.lang] || statement.atom;
    }
    return statement.atom;
}
