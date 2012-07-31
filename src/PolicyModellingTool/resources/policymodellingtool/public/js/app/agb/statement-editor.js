// http://localhost:8080/policymodellingtool/#/arguments/argumentgraph/policymodellingtool-6b0d1234-48c8-41c6-ae8a-a2e2df3cca6f

// (jdbc/create-table 
//           :statement 
//           [:id "varchar primary key not null"] ; a URN in the UUID namespace DONE
//           [:weight "double default null"] ; DONE
//           [:value "double default null"]
//           [:standard "tinyint default 0"]   ; 0=pe, 1=cce, 2=brd, 3=dv DONE
//           [:atom "varchar"]                 ; Clojure s-expression DONE
//           [:text "int"] ALMOST
//           [:main "boolean default false"]   ; true if a main issue DONE
//           [:header "int"] DONE
// 

AGB.get_statement_data = function() {
    return {text: {en: $('#statementtext').val() == "" ? null : $('#statementtext').val()},
            standard: $('#standard').val(),
            main: $('#main').val(),
            weight: $('#statement-editor-weight').val(),
            header: AGB.get_metadata_data(),
            atom: $('#editor-statement-atom').val()};  
};

AGB.save_statement = function(config) {
    var stmt = AGB.get_statement_data();
    console.log('saving statement: ');
    console.log(stmt);
    PM.ajax_post(IMPACT.wsurl + '/statement/' + IMPACT.db, stmt,
                 _.isNil(config.save_callback) ? AGB.statement_created : config.save_callback,
                 IMPACT.user, IMPACT.password, PM.on_error);    
    return false;
};

AGB.statement_created = function() {
    console.log('statement created');
};

AGB.create_statement_editor = function() {
    var html = ich.statementeditor();
    return html;
};

AGB.remove_statement_editor = function() {
    $('#statementeditor').empty();
    return false;
};