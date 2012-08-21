PM.arguments_url = function(db) {
    return 'arguments/argumentgraph/' + db;    
};

PM.set_arguments_url = function(db) {
    $.address.value(PM.arguments_url(db));  
};

PM.display_arguments = function(db, type, id) {
    if(_.isNil(db)) {
        db = IMPACT.db;
    }
    if(_.isNil(IMPACT.db)) {
        console.log('No db. Please enter some facts first'); 
    }
    
    IMPACT.db = db;
    
    PM.arguments = new PM.Arguments;
    PM.arguments.fetch();
                    
    PM.statements = new PM.Statements;
    PM.statements.fetch();

    var arguments_html = ich.arguments();
    $('#pm').html(arguments_html.filter("#arguments"));
    PM.activate('#arguments-item');
    
    //    db = "copyright"; // to debug
        
    if (type == "statement")  {
        AGB.display_statement(db, id);
    } else if(type == "argument") {
        AGB.display_argument(db, id);
    } else if(type == "map") {
        AGB.display_map(db);
    } else {
        AGB.display_argumentgraph(db);        
    }

};
