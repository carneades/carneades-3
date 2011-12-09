
function statement_url(db, stmtid)
{
    return 'statement/' + db + '/' + stmtid;
}

function set_statement_url(db, stmtid)
{
    $.address.value(statement_url(db, stmtid));
}

function fill_statement_template(statement_data)
{
    statement_data.statement_text = statement_text(statement_data);
    var statement_html = ich.statement(statement_data);
    $('body').html(statement_html.filter('#statement'));
}

function display_statement(db, stmtid)
{
    ajax_get(statement_url(db, stmtid),
            function(statement_data) {
                statement_data.db = db;
                
                if(statement_data.header) {
                    ajax_get('/metadata/' + db + '/' + statement_data.header,
                             function(metadata) {    
                                 statement_data.metadata_text = format_metadata(metadata);
                                 fill_statement_template(statement_data);
                             });
                } else {
                    fill_statement_template(statement_data);                    
                }
            });
}

function statement_text(statement)
{
    if(statement.text) {
        // TODO: if atom is UUID, then returns the string "statement" ?
        return statement.text[CARNEADES.lang] || statement.atom;
    }
    return statement.atom;
}
