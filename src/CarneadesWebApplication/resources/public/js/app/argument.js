
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
    ajax_get('argument-info/' + db + '/' + argid,
            function(argument_data) {
                argument_data.normalize();
                argument_data.direction = argument_data.pro ? "pro" : "con";
                argument_data.db = db;
                set_description_text(argument_data);
                set_argument_title_text(argument_data);
                argument_data.conclusion.statement_prefix = argument_data.conclusion.pro ? "pro" : "con";
                argument_data.conclusion.statement_text = statement_text(argument_data.conclusion);
                set_premises_text(argument_data);
                set_undercutters_text(argument_data);
                set_rebuttals_text(argument_data);
                set_dependents_text(argument_data);
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

function set_argument_title_text(info)
{
    var default_text = "Argument";
    if(info.header) {
        info.argument_title_text = info.header.title ? info.header.title['en'] : default_text;
    } else {
        info.argument_title_text = default_text;
    }
}

function set_undercutters_text(info)
{
    $.each(info.undercutters_data, 
           function(index, data) {
               data.argument_text = argument_text(data);
               data.id = info.undercutters[index];
           });  
}

function set_rebuttals_text(info)
{
    $.each(info.rebuttals_data,
          function(index, data) {
              data.argument_text = argument_text(data);
              data.id = info.rebuttals[index];
          });
}

function set_dependents_text(info)
{
    $.each(info.dependents_data,
          function(index, data) {
              data.argument_text = argument_text(data);
              data.id = info.dependents[index];
          });
}

function argument_text(data)
{
    var text;
    if(data.header == null) {
        text = argument_context(data);
    } else {
        text = format_metadata(data.header);
    }

    return text;
}

// returns a text representing the context of the argument, ie.
// its conclusion and its premises
function argument_context(data)
{
    var statement = statement_text(data.conclusion);
    var context = '<div class="argument">{0}<ul>'.format(statement);

    $.each(data.premises,
          function(index, premise) {
              var premise_text = statement_text(premise.statement);
              context += '<li>{0}</li>'.format(premise_text);
          });
    context += '</ul></div>';
    return context;
}