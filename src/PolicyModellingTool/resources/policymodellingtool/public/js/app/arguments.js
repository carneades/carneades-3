// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

PM.arguments_url = function(db) {
    return 'arguments/outline/' + db;    
};

PM.set_arguments_url = function(db) {
    $.address.value(PM.arguments_url(db));  
};

// this is the main entry point to display
// either the outline, the map, an argument or a statement
PM.display_arguments = function(db, type, id) {
    var arguments_html = ich.arguments(PM.merge_menu_props({}));
    
    if(_.isNil(db)) {
        db = IMPACT.db;
    }
    if(_.isNil(db) || db == undefined || db == 'undefined') {
        $('#pm').html(arguments_html.filter("#arguments"));
        $('#pm').append('<div>Please enter some facts to see the arguments.</div>');
        PM.activate('#arguments-item');
        PM.attach_lang_listener();
        
        return;
    }
    
    IMPACT.db = db;
    
    $('#pm').html(arguments_html.filter("#arguments"));
    PM.activate('#arguments-item');
    PM.attach_lang_listener();

    $.when(PM.arguments.fetch(),
           PM.statements.fetch()).then(
        function() {
            if (type == "statement")  {
                AGB.display_statement(db, id);
            } else if(type == "argument") {
                AGB.display_argument(db, id);
            } else if(type == "map") {
                AGB.display_map(db);
            } else if (type == "vote") {
                catb.views.pmt.vote.display();
            } else {
                AGB.display_argumentgraph(db);        
            }                                    
        });

};

PM.current_mainissueatompredicate = function() {
    var current_issue = PM.current_issue();
    if(current_issue == undefined) {
        return undefined;
    }
    var match = current_issue.atom.match(/\(([^ ]+) /);
    var mainissueatompredicate = "";
    if(match) {
        mainissueatompredicate = match[1];
    } else {
        return undefined;
    }

    return mainissueatompredicate;
};

PM.current_case_pollid = function() {
    var pollid = PM.get_cookies()['pollid-' + IMPACT.db];
    if(!pollid) {
        return undefined;
    }

    return parseInt(pollid, 10);
};
