// (jdbc/create-table 
//           :argument
//           [:id "varchar primary key not null"] ; a URN in the UUID namespace
//           [:conclusion "varchar not null"]     ; URN of the conclusion
//           [:strict "boolean default false"]
//           [:weight "double default 0.50"]
//           [:value "double"]                    ; null means not evaluated
//           [:scheme "varchar"]                  ; URI of the scheme
//           [:pro "boolean default true"]        ; con argument if false
//           [:header "int"]
//           ["foreign key(conclusion) references statement(id)"]
//           ["foreign key(header) references metadata(id)"])

AGB.get_argument_data = function() {
    return {conclusion: $('#editor-conclusion').val(),
            scheme: $('#editor-argument-scheme').val(),
            pro: $('#pro').val(),
            header: {}
           };
};

AGB.save_argument = function() {
    var argument = AGB.get_argument_data();
    console.log('saving argument: ');
    console.log(argument);
    PM.ajax_post(IMPACT.wsurl + '/argument/' + IMPACT.db, argument,
                 AGB.argument_created, IMPACT.user, IMPACT.password);    
    return false;
};

AGB.argument_created = function() {
    console.log('argument created');
};

AGB.create_argument_editor = function() {
    var html = ich.argumenteditor();
    return html;
};

AGB.remove_argument_editor = function() {
    $('#argumenteditor-content').remove();
    return false;
};