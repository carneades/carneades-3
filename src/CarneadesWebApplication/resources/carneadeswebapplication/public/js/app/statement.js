
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
    statement_data.statement_text = statement_data.text[CARNEADES.lang]; // statement_text(statement_data);
    var statement_html = ich.statement(statement_data);
    $('#browser').html(statement_html.filter('#statement'));
}

function display_statement(db, stmtid)
{
    ajax_get('statement-info/' + db + '/' + stmtid,
             function(info) {
                 info.normalize();
                 info.db = db;
                 set_statement_title_text(info);
                 info.description_text = description_text(info.header);
                 set_procon_texts(info);    
                 set_procon_premises_text(info);
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
               var text = argument_text(data, index + 1);
               info[direction][index].argument_text = text;
               info[direction][index].id = info.pro[index]; // used by the template to create the ahref
           });
}

function set_procon_premises_text(statement_data)
{
    $.each(statement_data.pro_data,
           function(index, pro) {
               set_premises_text(pro);
           });
    $.each(statement_data.con_data,
           function(index, con) {
               set_premises_text(con);
           });
}

function set_premise_of_texts(info)
{
    $.each(info.premise_of_data,
           function(index, data) {
               var text = argument_text(data, index + 1);
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

function slice_statement(statement_text)
{
    var maxlen = 180;
    if(statement_text.length > maxlen) {
        return statement_text.slice(0, maxlen - 3) + "...";
    } else {
        return statement_text;
    }
}

function statement_text(statement)
{
    if(statement.text && statement.text[CARNEADES.lang]) {
        var text = statement.text[CARNEADES.lang];
        if(statement_in(statement)) {
            text = "✔ " + text;
        } else if(statement_out(statement)) {
            text = "✘ " + text;
        }
        return markdown_to_html(text);
    }
    // if(statement.header && statement.header.description && statement.header.description[CARNEADES.lang]) {
    //     return markdown_to_html(slice_statement(statement.header.description[CARNEADES.lang]));
    // }

    // TODO: if atom is UUID, then returns the string "statement" ?
    return statement.atom;
}

function statement_link(db, id, text)
{
    return '<a href="/statement/{0}/{1}" rel="address:/statement/{0}/{1}" class="statement" id="statement{1}">{2}</a>'.format(db, id, text);
}

function statement_in(statement)
{
    return (1.0 - statement.value) < 0.001;
}

function statement_out(statement)
{
    return statement.value < 0.001;
}