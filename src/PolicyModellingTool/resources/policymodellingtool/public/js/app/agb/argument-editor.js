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
               if(premise.val() && !_.isNil(premise.data(premise.val()))) {
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


AGB.format_filtered_statement = function(statement) {
    return "<div>{0}</div>".format(AGB.statement_text(statement));
};

AGB.format_selected_statement = function(statement) {
    if(_.isNil(statement)) {
        // if called on an empty initial selection
        return "";
    }
    return "{0}".format(AGB.statement_raw_text(statement));
};

AGB.format_filtered_scheme = function(scheme) {
    return "<div>{0}</div>".format(scheme.id);
};

AGB.format_selected_scheme = function(scheme) {
    return "{0}".format(scheme.id);
};

AGB.format_filtered_matching_result = function(result) {
    return AGB.format_filtered_statement(result.statement);
};

AGB.format_selected_matching_result = function(result) {
    if(result.id == "") {
        // if called on an empty initial selection
        return "";
    }
    return AGB.format_selected_statement(result.statement);
};

// Creates a new argument editor
AGB.argumentgraph_newargument = function() {
    $('#argumenteditor').html(AGB.create_argument_editor());
    var search_term = "";

    var scheme_search_term = "";
    $('#editor-argument-scheme').select2({formatResult: AGB.format_filtered_scheme,
                                          formatSelection: AGB.format_selected_scheme, 
                                          placeholder: "Select a scheme",
                                          dataType: 'json',
                                          type: 'GET',
                                          error: function(jqXHR, textStatus) {
                                              console.log('[ERROR] AJAX ' + textStatus);
                                          },
                                          ajax: {
                                              url: IMPACT.wsurl + '/scheme',
                                              data: function(term, page) {
                                                  scheme_search_term = term;
                                                  return {};
                                              },
                                              results: function(schemes) {
                                                  return {
                                                      results: _.filter(schemes, function(scheme) {
                                                                            return scheme.id.indexOf(scheme_search_term) != -1;
                                                                        })
                                                  };
                                              }
                                          }
                                         });

    $('input:radio[name=pro]:nth(0)').attr('checked',true);
    $('input:radio[name=strict]:nth(1)').attr('checked',true);
    $('input:radio[name=apply-scheme]:nth(0)').attr('checked',true);

    $('#cancel-argument').click(AGB.remove_argument_editor);
    $('#save-argument').click(AGB.save_argument_display_graph);
    $('#editor-argument-scheme').change(AGB.scheme_changed);
    $('#editor-conclusion').change(AGB.conclusion_changed);
    $('input:radio[name=apply-scheme]').change(AGB.apply_scheme_changed);

    $('#argument-editor-conclusion-and-premises').hide();
    
    return false;
};

// Called when the apply scheme radio buttons are set to 'yes' or 'no'
AGB.apply_scheme_changed = function() {
    var apply_scheme = $('input:radio[name=apply-scheme]:checked').val();
    if(apply_scheme == "yes") {
        AGB.prepare_edition_without_scheme();
    } else {
        $('#scheme-selection').hide();
    }
};

// Enables the editor to allow an edition without being constrained by
// an argument scheme
AGB.prepare_edition_without_scheme = function() {
    $('#scheme-selection').show();
    $('#argument-premises').empty();
    $('#argument-exceptions').empty();
    $('#argument-assumptions').empty();
};


// Set the list of candidates for the conclusion
AGB.set_conclusion_candidates = function(atom) {
    PM.ajax_post(IMPACT.wsurl + '/matching-statements/' + IMPACT.db,
                 atom,
                 function(conclusion_statements_results) {
                     var conclusion = $('#editor-conclusion');
                     _.each(conclusion_statements_results, function(result) {
                                if(!AGB.is_grounded(atom)) {
                                    conclusion.data(result.statement.id, result);
                                }
                                result.id = result.statement.id;
                            });

                     conclusion.select2("destroy");
                     conclusion.select2(
                         {data: conclusion_statements_results,
                          placeholder: "Select a conclusion",
                          formatResult: AGB.format_filtered_matching_result,
                          formatSelection:
                          AGB.format_selected_matching_result,
                         initSelection: function(element) {
                             return element.data(element.val());
                         }});
                 },
                 IMPACT.user,
                 IMPACT.password,
                 PM.on_error);

    // new statement link
    $('#new-statement-for-conclusion').click(
        _.bind(AGB.argumentgraph_newstatement,
               AGB,
               {atom: atom.replace(/\?/g, ''),
                save_callback: AGB.update_conclusion_premises_candidates})
    );
};


// Set the list of candidates for an argument premise, assumption or
// exception 
// @id is the id of section, either 'argument-premises',
// 'argument-assumptions' or 'argument-exceptions' 
// @atom is the atom of
// the premise on which the current substitutions of the argument
// being edited have been applied
AGB.set_premise_candidates = function(id, premise, atom) {
    console.log('set_premise_candidates atom:');
    console.log(atom);
    var p = $(_.filter($(id + ' .statement-select'),
                       function(p) {
                           return $(p).data('role') == premise.role;
                       })[0]);

    PM.ajax_post(IMPACT.wsurl + '/matching-statements/' + IMPACT.db,
                 atom,
                 function(premise_results) {
                     console.log('premise result:');
                     console.log(premise_results);
                     _.each(premise_results, function(result) {
                                if(!AGB.is_grounded(atom)) {
                                    p.data(result.statement.id, result);
                                }
                                result.id = result.statement.id;
                            });
                     p.select2("destroy");
                     p.select2({data: premise_results,
                                placeholder: "Select a statement",
                                formatResult: AGB.format_filtered_matching_result,
                                formatSelection: AGB.format_selected_matching_result,
                                initSelection: function(element) {
                                    return element.data(element.val());
                                }
                               });

                 },
                 IMPACT.user,
                 IMPACT.password,
                 PM.on_error);


};


// Add select premise inputs to the argument editor
// @id is either '#argument-premises', '#assumptions-premises'
// or '#exceptions-premises'
AGB.add_premises = function(id, premises) {
    _.each(premises,
           function(premise) {
               var premise_form = ich.premiseeditor();
               $(id).append(premise_form);
               var role = $(id + ' input[class=role-input]:last');
               role.val(premise.role);
               role.prop('disabled', true);

               var p = $(id + ' .statement-select').last();
               p.data('role', premise.role);
               p.change(AGB.update_conclusion_premises_candidates);

               // adds a 'create statement' link
               $(id + ' a:last').click(
                   _.bind(AGB.argumentgraph_newstatement,
                          AGB,
                          {atom: AGB.sexpr_to_str(premise.statement.atom).replace(/\?/g, ''),
                           save_callback:
                           AGB.update_conclusion_premises_candidates}));

               AGB.set_premise_candidates(id,
                                          premise,
                                          AGB.sexpr_to_str(premise.statement.atom));
           });
};


// Called when the argument conclusion has changed in the editor
AGB.conclusion_changed = function() {
    AGB.update_conclusion_premises_candidates();
};

// Updates the candidates for a premise
AGB.update_premises_candidates = function(id, subs, premise) {
    PM.ajax_post(IMPACT.wsurl + '/apply-substitutions',
                 {statement: AGB.sexpr_to_str(premise.statement.atom),
                  substitutions: subs},
                 function(atom) {
                     AGB.set_premise_candidates(id,
                                                premise,
                                                AGB.sexpr_to_str(atom));
                 },
                 IMPACT.user,
                 IMPACT.password,
                 PM.on_error);
};

// Updates conclusion candidates
AGB.update_conclusion_candidates = function(conclusion, subs) {
  PM.ajax_post(IMPACT.wsurl + '/apply-substitutions',
                 {statement: AGB.sexpr_to_str(conclusion),
                  substitutions: subs},
                 function(atom) {
                     AGB.set_conclusion_candidates(AGB.sexpr_to_str(atom));
                 },
                 IMPACT.user,
                 IMPACT.password,
                 PM.on_error)  ;
};

// Updates candidates for the premises and conclusion to reflect the
// current substitutions
AGB.update_conclusion_premises_candidates = function() {
    var subs = AGB.get_argument_substitutions();
    var scheme = AGB.current_scheme();

    AGB.update_conclusion_candidates(scheme.conclusion, subs);
    _.each(scheme.premises, _.bind(AGB.update_premises_candidates, AGB, '#argument-premises', subs));
    _.each(scheme.assumptions, _.bind(AGB.update_premises_candidates, AGB, '#argument-assumptions', subs));
    _.each(scheme.exceptions, _.bind(AGB.update_premises_candidates, AGB, '#argument-exceptions', subs));

};

// Returns the current selected scheme of the argument editor
AGB.current_scheme = function() {
    var val = $('#editor-argument-scheme').val();
    return $('#editor-argument-scheme').data(val);
};

// Called when the argument scheme is changed
AGB.scheme_changed = function() {
    $('#argument-editor-conclusion-and-premises').show();
    
    var id = $('#editor-argument-scheme').val();
    console.log('scheme changed: ' + id);
    PM.ajax_get(IMPACT.wsurl + '/scheme/' + id,
                function(scheme) {
                    $('#editor-argument-scheme').data(id, scheme);
                    $('#editor-conclusion').select2('val', '');
                    AGB.set_conclusion_candidates(AGB.sexpr_to_str(scheme.conclusion));
                    $('#argument-premises').empty();
                    $('#argument-exceptions').empty();
                    $('#argument-assumptions').empty();
                    AGB.add_premises('#argument-premises', scheme.premises);
                    AGB.add_premises('#argument-assumptions', scheme.assumptions);
                    AGB.add_premises('#argument-exceptions', scheme.exceptions);
                },
                PM.on_error);
};



AGB.save_argument_display_graph = function() {
    if(!$('#editor-argument-scheme').valid()) {
        return false;
    }

    if(!$('#editor-conclusion').valid()) {
        return false;
    }

    if(_.filter(AGB.get_all_premises(),
                function(premise) {
                    return $(premise).valid() == false;
                }).length > 0) {
        return false;
    }

    AGB.save_argument();
    AGB.remove_argument_editor();
    AGB.display_argumentgraph(IMPACT.db);
    return false;
};
