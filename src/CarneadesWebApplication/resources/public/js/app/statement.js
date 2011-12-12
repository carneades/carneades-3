
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
    ajax_get('/statement-info/' + db + '/' + stmtid,
             function(info) {
                 info.db = db;
                 set_statement_title_text(info);
                 set_description_text(info);
                 set_procon_texts(info);                            
                 fill_statement_template(info);
             });
}

function set_description_text(info)
{
    if(info.header) {
        info.description_text = info.header.description ? info.header.description['en'] : "";        
    } else {
        info.description_text = "";
    }
}

function set_statement_title_text(info)
{
    var default_text = "Statement";
    if(info.header) {
        info.statement_title_text = info.header.title ? info.header.title['en'] : default_text;
    } else {
        info.statement_title_text = default_text;
    }
}

function set_arg_texts(info, direction)
{
    $.each(info[direction], 
           function(index, metadata) {
               var text = format_metadata(metadata);
               text = text.length == 0 ? "Argument" : text;
               info[direction][index].argument_text = text;
               info[direction][index].id = info.pro[index]; // used by the template to create the ahref
           });
}

function set_procon_texts(info)
{
    set_arg_texts(info, 'pro_metadata');
    set_arg_texts(info, 'con_metadata');

}

function statement_text(statement)
{
    if(statement.text) {
        // TODO: if atom is UUID, then returns the string "statement" ?
        return statement.text[CARNEADES.lang] || statement.atom;
    }
    return statement.atom;
}
