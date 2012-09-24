// Shows a new statement editor
// @config accepts the following values:
// atom, the default value of the atom field
// save_callback, a callback called once the statement is created
// update: true if the edit is to update the statement
AGB.show_statement_editor = function(config) {
    if(_.isNil(config)) {
        config = {}; 
    }

    if(_.isNil(config.statement)) {
        $('#statementeditor').html(AGB.create_statement_editor({title: 'New Statement'}));    
    } else {
        $('#statementeditor').html(AGB.create_statement_editor({title: 'Edit Statement'}));    
    }
    
    $('#cancel-statement').click(AGB.remove_statement_editor);
    
    $('#statement-editor-text').markItUp(mySettings);
    
    AGB.set_statement_main(false);
    
    if(!_.isNil(config.atom)) {
        $('#statement-editor-atom').val(config.atom);
    } 

    if(!_.isNil(config.statement)) {
        AGB.fillin_statement_editor(config.statement);
    } else {
        var metadata_editor_view = new PM.MetadataEditorView(
            {model: new PM.MetadataCandidate({metadata: new PM.Metadata(),
                                              current_lang: IMPACT.lang}),
             el: $('#statement-header')});
        $('#statement-header').data('view', metadata_editor_view);
    }
    
    $('#statement-header').data('view').render();
    
    $('#save-statement').click(
        function() {
            if(_.isNil(config.update)) {
                AGB.save_statement(config);    
            } else {
                AGB.update_statement(config);
            }
            
            AGB.remove_statement_editor();
            return false;
        });
    
    $('input[type=range]').
        change(function() {
                   var slider_val = $(this).val();
                   $('#statement-editor-weight').val(slider_val);
               });

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
    if(stmt.text && stmt.text.en) {
        $('#statement-editor-text').val(stmt.text.en);    
    } 
    $('#statement-editor-standard').val(stmt.standard);
    AGB.set_statement_main(stmt.main);
    $('#statement-editor-weight').val(stmt.weight);
    $('#statement-editor-atom').val(stmt.atom);
    // AGB.fillin_metadata_editor(stmt.header);
    var metadata_editor_view = new PM.MetadataEditorView(
        {model: new PM.MetadataCandidate({metadata: new PM.Metadata(stmt.header),
                                          current_lang: IMPACT.lang}),
         el: $('#statement-header')});
    $('#statement-header').data('view', metadata_editor_view);
    
};

// Returns the data of entered in the statement editor as an object
AGB.get_statement_data = function() {
    var metadata = $('#statement-header').data('view').
        model.get('metadata').attributes;
    
    return {text: {en: $('#statement-editor-text').val() == "" ? 
                   null : $('#statement-editor-text').val()},
            standard: $('#statement-editor-standard').val(),
            main: $('input:radio[name=main]:checked').val(),
            weight: $('#statement-editor-weight').val() == "" ?
            null : $('#statement-editor-weight').val(),
            header: metadata,
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
    var new_statement = new PM.Statement(stmt);
    new_statement.save(null, {success: function(data) {
                                  return _.isNil(config.save_callback) ? AGB.statement_created() : config.save_callback(data);
                              },
                              error: PM.on_error});
    PM.statements.add(new_statement);
    // PM.ajax_post(IMPACT.wsurl + '/statement/' + IMPACT.db, stmt,
    //              _.isNil(config.save_callback) ? AGB.statement_created : config.save_callback,
    //              IMPACT.user, IMPACT.password, PM.on_error);
    return false;
};

// Updates the statement being edited into the database
AGB.update_statement = function(config) {
    console.log('update statement');
    var stmt = AGB.get_statement_data();
    stmt = _.extend(stmt, {id: config.statement.id});
    PM.ajax_put(IMPACT.wsurl + '/statement/' + IMPACT.db, stmt,
                 _.isNil(config.save_callback) ? AGB.statement_created : config.save_callback,
                 IMPACT.user, IMPACT.password, PM.on_error);
    return false;
};

// Callback invoked upon statement creation
AGB.statement_created = function() {
    console.log('statement created');
};

// Returns the HTML content of the statement editor
AGB.create_statement_editor = function(data) {
    var html = ich.statementeditor(data);
    return html;
};

// Removes the statement editor from the page
AGB.remove_statement_editor = function() {
    $('#statementeditor').empty();
    return false;
};
