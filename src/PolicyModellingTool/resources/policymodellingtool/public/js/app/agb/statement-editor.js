// Creates a new statement editor
// @config accepts the following values:
// atom, the default value of the atom field
// save_callback, a callback called once the statement is created
AGB.argumentgraph_newstatement = function(config) {
    if(_.isNil(config)) {
        config = {}; 
    }

    $('#statementeditor').html(AGB.create_statement_editor());
    $('#statement-header').html(AGB.create_metadata_editor());
    $('#cancel-statement').click(AGB.remove_statement_editor);
    $('input:radio[name=main]:nth(1)').attr('checked',true);
    
    if(!_.isNil(config.atom)) {
        $('#statement-editor-atom').val(config.atom);
    } 
    
    $('#save-statement').click(
        function() {
            AGB.save_statement(config);
            AGB.remove_statement_editor();
            return false;
        }
    );
    
    return false;
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