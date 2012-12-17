// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

PM.arguments_url = function(db) {
    return 'arguments/outline/' + db;    
};

PM.set_arguments_url = function(db) {
    $.address.value(PM.arguments_url(db));  
};

PM.display_arguments = function(db, type, id) {
    IMPACT.facts_state = 'done';
    
    var arguments_html = ich.arguments({pmt_menu_intro: $.i18n.prop('pmt_menu_intro'),
                                        pmt_menu_issues: $.i18n.prop('pmt_menu_issues'),
                                        pmt_menu_facts: $.i18n.prop('pmt_menu_facts'),
                                        pmt_menu_arguments: $.i18n.prop('pmt_menu_arguments'),
                                        pmt_menu_schemes: $.i18n.prop('pmt_menu_schemes'),
                                        pmt_menu_policies: $.i18n.prop('pmt_menu_policies') 
                                       });
    
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
    
    var deferreds = [];

    deferreds.push(PM.arguments.fetch());
    deferreds.push(PM.statements.fetch());
    
    $.when($, deferreds).done(
        function() {
            if (type == "statement")  {
                      AGB.display_statement(db, id);
                  } else if(type == "argument") {
                      AGB.display_argument(db, id);
                  } else if(type == "map") {
                      AGB.display_map(db);
                  } else {
                      AGB.display_argumentgraph(db);        
                  }                                    
        });

};
