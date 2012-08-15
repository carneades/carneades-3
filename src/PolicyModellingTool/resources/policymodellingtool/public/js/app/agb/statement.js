"use strict";

AGB.statement_url = function(db, stmtid)
{
    return '/arguments/statement/' + db + '/' + stmtid;
};

AGB.set_statement_url = function(db, stmtid)
{
    $.address.value(AGB.statement_url(db, stmtid));
};

AGB.statement_html = function(db, info, lang)
{
    AGB.normalize(info);
    info.db = db;
    AGB.set_statement_title_text(info);
    info.description_text = AGB.description_text(info.header);
    AGB.set_procon_texts(info);    
    AGB.set_procon_premises_text(info);
    AGB.set_premise_of_texts(info);
    info.statement_text = AGB.markdown_to_html(info.text[lang]);

    var statement_html = ich.statement(info);
    return statement_html.filter('#statement');
};

AGB.display_statement = function(db, stmtid)
{
    PM.ajax_get(IMPACT.wsurl + '/statement-info/' + db + '/' + stmtid,
                function(info) {
                    $('#browser').html(AGB.statement_html(db, info, IMPACT.lang));
                    $('#export').click(function (event){
                                           window.open('/impactws/export/{0}'.format(db), 'CAF XML');
                                           return false; 
                                       });
                    AGB.enable_statement_edition(db, info);
                },
                PM.on_error);;
}

AGB.set_statement_title_text = function(info)
{
    var default_text = "Statement";
    if(info.header) {
        info.statement_title_text = info.header.title ? info.header.title['en'] : default_text;
    } else {
        info.statement_title_text = default_text;
    };
};

AGB.set_arg_texts = function(info, direction)
{
    $.each(info[direction], 
           function(index, data) {
               var text = AGB.argument_text(data, index + 1);
               info[direction][index].argument_text = text;
               info[direction][index].id = info.pro[index]; // used by the template to create the ahref
           });
};

AGB.set_procon_premises_text = function(statement_data)
{
    $.each(statement_data.pro_data,
           function(index, pro) {
               AGB.set_premises_text(pro);
           });
    $.each(statement_data.con_data,
           function(index, con) {
               AGB.set_premises_text(con);
           });
};

AGB.set_premise_of_texts = function(info)
{
    $.each(info.premise_of_data,
           function(index, data) {
               var text = AGB.argument_text(data, index + 1);
               data.argument_text = text;
               data.id = info.premise_of[index]; // used by the template to create the href
           }
          );
};

AGB.set_procon_texts = function(info)
{
    AGB.set_arg_texts(info, 'pro_data');
    AGB.set_arg_texts(info, 'con_data');
};

AGB.slice_statement = function(statement_text)
{
    var maxlen = 180;
    if(statement_text.length > maxlen) {
        return statement_text.slice(0, maxlen - 3) + "...";
    } else {
        return statement_text;
    }
};

AGB.statement_prefix = function(statement) {
    if(AGB.statement_in(statement)) {
        return "✔ ";
    } else if(AGB.statement_out(statement)) {
        return "✘ ";
    } else {
        return "";
    }
};

AGB.statement_standard = function(statement) {
    if(statement.standard == "pe") {
        return "Preponderance of Evidence";
    } 
    
    if(statement.standard == "dv") {
        return "Dialectical Validity";
    }
    
    if(statement.standard == "cce") {
        return "Clear and Convincing Evidence";
    }

    if(statement.standard == "brd") {
        return "Beyond Reasonable Doubt";
    }
    
    return "";
};

AGB.sexpr_to_str = function(sexpr) {
    var str = "(";
    
    _.each(sexpr, function(s) {
              str += s + " ";
           });
    
    str = str.slice(0, -1);
    str += ")";
    
    return str;
};

AGB.statement_raw_text = function(statement) {
    if(statement.text && statement.text[IMPACT.lang]) {
        var text = statement.text[IMPACT.lang];
        return text;
    }

    // TODO: if atom is UUID, then returns the string "statement" ?
    return statement.atom;  
};

AGB.statement_text = function(statement)
{
    var text = AGB.statement_raw_text(statement);
    return AGB.markdown_to_html(AGB.statement_prefix(statement) + text);
};

AGB.statement_link = function(db, id, text)
{
    return '<a href="/arguments/statement/{0}/{1}" rel="address:/arguments/statement/{0}/{1}" class="statement" id="statement{1}">{2}</a>'.format(db, id, text);
};

AGB.statement_in = function(statement)
{
    return (statement.value != null) && ((1.0 - statement.value) < 0.001);
};

AGB.statement_out = function(statement)
{
    return (statement.value != null) && (statement.value < 0.001);
};

AGB.enable_statement_edition = function(db, info) {
    $('#menus').append(ich.statementeditormenu());
    $('#delete-statement').click(_.bind(AGB.delete_statement, AGB, db, info.id));
    $('#edit-statement').click(_.bind(AGB.edit_statement, AGB, db, info));

    
    return false;
};

AGB.delete_statement = function(db, stmtid) {
    if(confirm('Delete this statement and its linked arguments?')) {
        PM.ajax_delete(IMPACT.wsurl + '/statement/' + db + '/' + stmtid,
                       function(e) {
                           console.log('statement deleted');
                           console.log(e);
                           
                           AGB.set_argumentgraph_url(db);
                       },
                       IMPACT.user, 
                       IMPACT.password,
                       PM.on_error);    
    }

    return false; 
};

AGB.edit_statement = function(db, info) {
    AGB.show_statement_editor({update: true,
                              statement: info,
                              save_callback: _.bind(AGB.display_statement, AGB, db, info.id) 
                              });
    return false;
};
