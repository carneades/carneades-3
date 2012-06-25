AGB.save_argument = function() {
    var scheme_id = $('#editor-argument-scheme').val();
    var conclusion = $('#editor-conclusion').val();
    
    console.log('saving argument: ');
    var subs = $('#editor-conclusion').data(conclusion).substitutions;

    var premises = $('#argument-premises input[class=statement-select]');
    var assumptions = $('#argument-assumptions input[class=statement-select]');
    var exceptions = $('#argument-exceptions input[class=statement-select]');

    var all_premises = $.merge(premises, assumptions);
    all_premises = $.merge(all_premises, exceptions);
    
    _.each(all_premises,
           function(premise) {
               premise = $(premise);
              $.extend(subs, premise.data(premise.val()).substitutions);
          });
    
    console.log('Substitutions for apply-scheme:');
    console.log(subs);
    
    PM.ajax_post(IMPACT.wsurl + '/apply-scheme/' + IMPACT.db + '/' + scheme_id, subs,
                 AGB.argument_created, IMPACT.user, IMPACT.password);    
    
    return false;
};

AGB.argument_created = function(data) {
    console.log('arguments created');
    console.log(data);
};

AGB.create_argument_editor = function() {
    var html = ich.argumenteditor();
    return html;
};

AGB.remove_argument_editor = function() {
    $('#argumenteditor').empty();
    return false;
};