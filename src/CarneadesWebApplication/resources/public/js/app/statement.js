
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
                 info.normalize();
                 info.db = db;
                 set_statement_title_text(info);
                 set_description_text(info);
                 set_procon_texts(info);                            
                 set_premise_of_texts(info);
                 fill_statement_template(info);
             });
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
           function(index, data) {
               var text = argument_text(data);
               info[direction][index].argument_text = text;
               info[direction][index].id = info.pro[index]; // used by the template to create the ahref
           });
}

function set_premise_of_texts(info)
{
    $.each(info.premise_of_data,
           function(index, data) {
               var text = argument_text(data);
               data.argument_text = text;
               data.id = info.premise_of[index]; // used by the template to create the href
           }
          );
}

function set_procon_texts(info)
{
    set_arg_texts(info, 'pro_data');
    set_arg_texts(info, 'con_data');
}

function statement_text(statement)
{
    if(statement.text) {
        // TODO: if atom is UUID, then returns the string "statement" ?
        return statement.text[CARNEADES.lang] || statement.atom;
    }
    return statement.atom;
}
