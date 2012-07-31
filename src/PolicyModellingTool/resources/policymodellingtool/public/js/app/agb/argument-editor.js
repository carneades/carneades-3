// Returns the JQuery premises object of the argument editor
AGB.get_premises = function() {
    return $('#argument-premises input.statement-select.required');
};

// Returns the JQuery premises object of the argument editor
AGB.get_assumptions = function() {
    return $('#argument-assumptions input.statement-select.required');
};

// Returns the JQuery premises object of the argument editor
AGB.get_exceptions = function() {
    return $('#argument-exceptions input.statement-select.required');
};

// Returns the premises, assumptions and exceptions of the argument
// being edited
AGB.get_all_premises = function() {
    var premises = AGB.get_premises();
    var assumptions = AGB.get_assumptions();
    var exceptions = AGB.get_exceptions();

    var all_premises = $.merge(premises, assumptions);
    all_premises = $.merge(all_premises, exceptions);
    return all_premises;
};

// Returns the current substitutions for the argument's
// conclusions, premises, assumptions and exceptions
AGB.get_argument_substitutions = function() {
    var conclusion = $('#editor-conclusion').val();
    var subs = {};

    if(conclusion) {
        subs = $('#editor-conclusion').data(conclusion).substitutions;
    }

    _.each(AGB.get_all_premises(),
           function(premise) {
               premise = $(premise);
               if(premise.val()) {
                   $.extend(subs, premise.data(premise.val()).substitutions);
               }

           });

    return subs;
};

// Saves the argument being edited into the database
AGB.save_argument = function() {
    var scheme_id = $('#editor-argument-scheme').val();
    console.log('saving argument: ');

    var subs = AGB.get_argument_substitutions();

    console.log('Substitutions for apply-scheme:');
    console.log(subs);

    PM.ajax_post(IMPACT.wsurl + '/apply-scheme/' + IMPACT.db + '/' + scheme_id,
                 subs, AGB.argument_created, IMPACT.user, IMPACT.password);
    return false;
};

// Callback invoked upon argument creation
AGB.argument_created = function(data) {
    console.log('arguments created');
    console.log(data);
};

// Returns the HTML content of the argument editor
AGB.create_argument_editor = function() {
    var html = ich.argumenteditor();
    return html;
};

// Removes the argument editor from the page
AGB.remove_argument_editor = function() {
    $('#argumenteditor').empty();
    return false;
};