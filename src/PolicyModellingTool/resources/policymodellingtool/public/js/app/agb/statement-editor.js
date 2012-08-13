// Shows a new statement editor
// @config accepts the following values:
// atom, the default value of the atom field
// save_callback, a callback called once the statement is created
// update: true if the edit is to update the statement
AGB.show_statement_editor = function(config) {
    if(_.isNil(config)) {
        config = {}; 
    }

    $('#statementeditor').html(AGB.create_statement_editor());
    $('#statement-header').html(AGB.create_metadata_editor());
    $('#cancel-statement').click(AGB.remove_statement_editor);
    
    AGB.set_statement_main(false);
    
    if(!_.isNil(config.atom)) {
        $('#statement-editor-atom').val(config.atom);
    } 

    if(!_.isNil(config.statement)) {
        AGB.fillin_statement_editor(config.statement);
    }
    
    $('#save-statement').click(
        function() {
            if(_.isNil(config.update)) {
                AGB.save_statement(config);    
            } else {
                AGB.update_statement(config);
            }
            
            AGB.remove_statement_editor();
            return false;
        }
    );
    
    // note: mySettings is defined in set.js
    $('#metadata-description').markItUp(mySettings);
    
    return false;
};

// Sets the main attribute in the input field
AGB.set_statement_main = function(ismain) {
    var main = $('input:radio[name=main]');
    _.each(main,
           function(m) {
               if($(m).val() == "yes" && ismain) {
                   $(m).attr("checked", "checked");
               } else if($(m).val() == "no" && !ismain) {
                   $(m).attr("checked", "checked");
               }
           });
};

// Fills in the inputs of the statement editor
AGB.fillin_statement_editor = function(stmt) {
    $('#statement-editor-text').val(stmt.text.en);
    $('#statement-editor-standard').val(stmt.standard);
    AGB.set_statement_main(stmt.main);
    $('#statement-editor-weight').val(stmt.weight);
    $('#statement-editor-atom').val(stmt.atom);
    AGB.fillin_metadata_editor(stmt.header);
};

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
// @config 
// config.save_callback: callback called with the statement's id
// 
AGB.save_statement = function(config) {
    var stmt = AGB.get_statement_data();
    console.log('saving statement: ');
    console.log(stmt);
    PM.ajax_post(IMPACT.wsurl + '/statement/' + IMPACT.db, stmt,
                 _.isNil(config.save_callback) ? AGB.statement_created : config.save_callback,
                 IMPACT.user, IMPACT.password, PM.on_error);
    return false;
};

// Updates the statement being edited into the database
AGB.update_statement = function(config) {
    console.log('update statement');
    var stmt = AGB.get_statement_data();
    PM.ajax_put(IMPACT.wsurl + '/statement/' + IMPACT.db + '/' + config.statement.id, stmt,
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
