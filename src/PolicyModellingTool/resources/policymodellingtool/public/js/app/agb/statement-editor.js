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

// Returns the data of entered in the statement editor as an object
AGB.get_statement_data = function() {
    return {text: {en: $('#statement-editor-text').val() == "" ? 
                   null : $('#statement-editor-text').val()},
            standard: $('#statement-editor-standard').val(),
            main: $('input:radio[name=main]:checked').val(),
            weight: $('#statement-editor-weight').val() == "" ?
            null : $('#statement-editor-weight').val(),
            header: AGB.get_metadata_data(),
            atom: $('#statement-editor-atom').val()};  
};

// Saves the statement being edited into the database
AGB.save_statement = function(config) {
    var stmt = AGB.get_statement_data();
    console.log('saving statement: ');
    console.log(stmt);
    PM.ajax_post(IMPACT.wsurl + '/statement/' + IMPACT.db, stmt,
                 _.isNil(config.save_callback) ? AGB.statement_created : config.save_callback,
                 IMPACT.user, IMPACT.password, PM.on_error);    
    return false;
};

// Callback invoked upon statement creation
AGB.statement_created = function() {
    console.log('statement created');
};

// Returns the HTML content of the statement editor
AGB.create_statement_editor = function() {
    var html = ich.statementeditor();
    return html;
};

// Removes the statement editor from the page
AGB.remove_statement_editor = function() {
    $('#statementeditor').empty();
    return false;
};