
AGB.argument_url = function(db, argid)
{
    return '/arguments/argument/' + db + '/' + argid;
};

AGB.set_argument_url = function(db, argid)
{
    $.address.value(AGB.argument_url(db, argid));
};

AGB.argument_html = function(db, argument_data)
{
    AGB.normalize(argument_data);
    argument_data.direction = argument_data.pro ? "pro" : "con";
    argument_data.db = db;
    argument_data.description_text = AGB.description_text(argument_data.header);
    AGB.set_argument_title_text(argument_data);
    argument_data.direction_text = argument_data.pro ? "pro" : "con";
    argument_data.conclusion.statement_text = AGB.statement_text(argument_data.conclusion);
    AGB.set_premises_text(argument_data);
    AGB.set_undercutters_text(argument_data);
    AGB.set_rebuttals_text(argument_data);
    AGB.set_dependents_text(argument_data);
    var argument_html = ich.argument(argument_data);
    return argument_html.filter('#argument');
};

AGB.display_argument = function(db, argid)
{
    PM.ajax_get(IMPACT.wsurl + '/argument-info/' + db + '/' + argid,
                function(argument_data) {
                    $('#browser').html(AGB.argument_html(db, argument_data));
                    $('#export').click(function (event){
                                           window.open('/impactws/export/{0}'.format(db), 'CAF XML');
                                           return false; 
                                       });
                    AGB.enable_argument_edition(db, argid);
                },
                PM.on_error);
};

AGB.set_premises_text = function(argument_data)
{
    $.each(argument_data.premises, 
           function(index, premise) {
               premise.statement.statement_text = AGB.statement_text(premise.statement, index + 1);
               premise.positive_text = premise.positive ? "" : "neg.";
           });
};

AGB.set_argument_title_text = function(info)
{
    var default_text = "Argument";
    if(info.header) {
        info.argument_title_text = info.header.title ? info.header.title['en'] : default_text;
    } else {
        info.argument_title_text = default_text;
    }
};

AGB.set_undercutters_text = function(info)
{
    $.each(info.undercutters_data, 
           function(index, data) {
               data.argument_text = AGB.argument_text(data, index + 1);
               data.id = info.undercutters[index];
               AGB.set_premises_text(data);
           });  
};

AGB.set_rebuttals_text = function(info)
{
    $.each(info.rebuttals_data,
          function(index, data) {
              data.argument_text = AGB.argument_text(data, index + info.undercutters_data.length);
              data.id = info.rebuttals[index];
              AGB.set_premises_text(data);
          });
};

AGB.set_dependents_text = function(info)
{
    $.each(info.dependents_data,
          function(index, data) {
              data.statement_text = AGB.statement_text(data, index + 1);
              data.id = info.dependents[index];
          });
};

// Returns a text representing the argument, ie., its title
// or then its scheme, or a default text if none of them is defined
AGB.argument_text = function(data, index)
{
    var text;
    if(data.header && data.header.title) {
        text = data.header.title;
    } else if (data.scheme && data.scheme.length > 0) {
        text = data.scheme;   
    } else if(index == undefined) {
        text = 'Argument';
    } else {
        text = 'Argument #' + index;            
    }
    
    return text;
};

AGB.argument_link = function(db, id, text)
{
    return '<a href="/arguments/argument/{0}/{1}" rel="address:/arguments/argument/{0}/{1}" class="argument" id="argument{1}">{2}</a>'.format(db, id, text);
};


AGB.enable_argument_edition = function(db, argid) {
    $('#menus').append(ich.argumenteditormenu());
    $('#delete-argument').click(_.bind(AGB.delete_argument, AGB, db, argid));
    
    return false;
};

AGB.delete_argument = function(db, argid) {
    if(confirm('Delete this argument?')) {
        PM.ajax_delete(IMPACT.wsurl + '/argument/' + db + '/' + argid,
                       function(e) {
                           console.log('argument deleted');
                           console.log(e);
                           
                           AGB.set_argumentgraph_url(db);
                       },
                       IMPACT.user, 
                       IMPACT.password,
                       PM.on_error);    
    }

    return false; 
};