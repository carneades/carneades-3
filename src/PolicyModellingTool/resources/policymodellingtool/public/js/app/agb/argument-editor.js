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
AGB.save_argument_with_scheme = function() {
    var scheme_id = $('#argument-editor-scheme').val();
    console.log('saving argument: ');

    var subs = AGB.get_argument_substitutions();

    console.log('Substitutions for apply-scheme:');
    console.log(subs);

    PM.ajax_post(IMPACT.wsurl + '/apply-scheme/' + IMPACT.db + '/' + scheme_id,
                 subs, AGB.argument_created, IMPACT.user, IMPACT.password);
    
    AGB.remove_argument_editor();
    AGB.display_argumentgraph(IMPACT.db);

    return false;
};

AGB.get_premises_ids = function(premises) {
    return _.reject(_.map(premises,
                          function(premise) {
                              return $(premise).val();
                          }),
                    function(id) {
                        return id == "";
                    });
};

AGB.get_statement_from_id = function(statements, id) {
    return _.find(statements, function(statement) {
                      return statement.id == id;
                  });
};

AGB.statement_to_premise = function(statement, role) {
    return {
        statement: statement,
        role: role
    };
};

AGB.get_premises_content_helper = function(statements, selector, getterfn) {
    var roles = _.map($(selector + ' .role-input'),
                      function(r) { 
                          return $(r).val(); 
                      }
                     );

    var premises = getterfn();
    
    var premises_content = [];
    
    _.each(premises,
           function(premise, index) {
               premise = $(premise);
               if(premise.val() != "") {
                   var statement = AGB.get_statement_from_id(statements, premise.val());
                   premises_content.push(AGB.statement_to_premise(statement, roles[index]));
               }
           });
    
    return premises_content;    
};

AGB.get_premises_content = function(statements) {
    return AGB.get_premises_content_helper(statements, '#argument-premises', AGB.get_premises);
};

AGB.get_exceptions_content = function(statements) {
    return AGB.get_premises_content_helper(statements, '#argument-exceptions', AGB.get_exceptions);
};

AGB.save_argument_without_scheme = function() {
    if(!$('#editor-conclusion').valid()) {
        return false;
    }

    var conclusion_id = $('#editor-conclusion').val();
  
    var exceptions_id = AGB.get_premises_ids(AGB.get_exceptions());
    
    PM.ajax_get(IMPACT.wsurl + '/statement/' + IMPACT.db,
                function(statements) {
                    var conclusion = AGB.get_statement_from_id(statements, conclusion_id);
                    var premises = AGB.get_premises_content(statements);
                    var exceptions = AGB.get_exceptions_content(statements);

                    var argument = {
                        conclusion: conclusion,
                        premises: premises,
                        exceptions: exceptions
                    };
                    
                    PM.ajax_post(IMPACT.wsurl + '/argument/' + IMPACT.db,
                                 argument, AGB.argument_created, IMPACT.user, IMPACT.password);
                    
                    AGB.remove_argument_editor();
                    AGB.display_argumentgraph(IMPACT.db);
                });
    
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
    return "<div>{0}</div>".format(scheme.header.title);
};

AGB.format_selected_scheme = function(scheme) {
    return "{0}".format(scheme.header.title);
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

    PM.ajax_get(IMPACT.wsurl + '/scheme',
               function(schemes) {
                   $('#argument-editor-scheme').select2({formatResult: AGB.format_filtered_scheme,
                                                         formatSelection: AGB.format_selected_scheme, 
                                                         placeholder: "Select a scheme",
                                                         data: {
                                                             results: schemes,
                                                             text: function(scheme) {
                                                                 return scheme.header.title;
                                                             }
                                                         }
                                                        });
               
               });
    
    $('input:radio[name=pro]:nth(0)').attr('checked',true);
    $('input:radio[name=strict]:nth(1)').attr('checked',true);
    $('input:radio[name=apply-scheme]:nth(0)').attr('checked',true);

    $('#cancel-argument').click(AGB.remove_argument_editor);

    AGB.prepare_argument_edition({pre_edition: AGB.pre_edition_with_scheme,
                                  on_save: AGB.save_argument_with_scheme});
    
    return false;
};

// Called when the apply scheme radio buttons are set to 'yes' or 'no'
AGB.apply_scheme_changed = function() {
    var apply_scheme = $('input:radio[name=apply-scheme]:checked').val();
    if(apply_scheme == "yes") {
        AGB.prepare_argument_edition({pre_edition: AGB.pre_edition_with_scheme,
                                      on_save: AGB.save_argument_with_scheme});
    } else {
        AGB.prepare_argument_edition({pre_edition: AGB.pre_edition_without_scheme,
                                      on_save: AGB.save_argument_without_scheme});
    }
};

// Prepares edition for editing with a scheme
AGB.pre_edition_with_scheme = function() {
    $('#scheme-selection').show();
    if($('#argument-editor-scheme').val() == "") {
        $('#argument-editor-conclusion-and-premises').hide();
    } else {
        $('#argument-editor-conclusion-and-premises').show();
    }
    $('#editor-conclusion').change(AGB.conclusion_changed);
    $('#argument-editor-scheme').change(AGB.scheme_changed);
    $('input:radio[name=apply-scheme]').change(AGB.apply_scheme_changed);
};

// Add some inputs for select premises when editing without a scheme
// @id is either '#argument-premises', '#argument-assumptions',
// '#argument-exceptions'
// @nb number of inputs to create
AGB.add_premises_inputs = function(id, nb) {
    for(var i = 0; i < nb; i++) {
        $(id).append(ich.premiseeditorwithoutscheme());    
    }
    
    $(id + '-extra').append(ich.addmore());
    $(id + '-extra').append(" or ");
    $(id + '-extra').append(ich.newstatementlink());
    
    $(id + '-extra input[type=submit]').click(
        _.bind(AGB.add_more_premises_inputs, AGB, id));
    
    // adds a 'create statement' link listener
    $(id + '-extra a:last').click(
        _.bind(AGB.argumentgraph_newstatement,
               AGB,
               {atom: "",
                save_callback:
                AGB.update_conclusion_premises_candidates_without_scheme}));

};

// Add more premises input fields
AGB.add_more_premises_inputs = function(id) {
    $(id).append(ich.premiseeditorwithoutscheme());
    $(id).append(ich.premiseeditorwithoutscheme());

    AGB.update_conclusion_premises_candidates_without_scheme();
    
    return false;
};

// Prepares the argument editor for editing either in a mode where a
// scheme is applied, or in a mode where no scheme is applied
AGB.prepare_argument_edition = function(mode) {
    mode.pre_edition();
    $('#save-argument').unbind('click');
    $('#editor-conclusion').unbind('change');
    $('#save-argument').click(mode.on_save);
};

// Prepares edition for editing without a scheme
AGB.pre_edition_without_scheme = function() {
    $('#scheme-selection').hide();
    $('#argument-editor-conclusion-and-premises').show();
    $('#argument-premises').empty();
    $('#argument-exceptions').empty();
    $('#argument-assumptions').empty();
    $('#new-statement-for-conclusion').unbind('click');
    
    AGB.add_premises_inputs('#argument-premises', 3);
    // AGB.add_premises_inputs('#argument-assumptions', 1);
    AGB.add_premises_inputs('#argument-exceptions', 1);

    AGB.update_conclusion_premises_candidates_without_scheme();

    // new statement link
    $('#new-statement-for-conclusion').click(
        _.bind(AGB.argumentgraph_newstatement,
               AGB,
               {atom: "",
                save_callback: 
                AGB.update_conclusion_premises_candidates_without_scheme}));
};

// Updates the list of candidates in the input fields of the conclusion
// and of the premises
AGB.update_conclusion_premises_candidates_without_scheme = function() {
    PM.ajax_get(IMPACT.wsurl + '/statement/' + IMPACT.db,
                function(statements) {
                    _.each($('#argument-editor-conclusion-and-premises input[type=hidden]'),
                           function(input) {
                               $(input).select2('destroy');
                               $(input).select2({data: {results: statements,
                                                        text: function(statement) {
                                                            return AGB.statement_text(statement);
                                                        }
                                                       },
                                                 placeholder: "Select a statement",
                                                 formatSelection: AGB.format_selected_statement,
                                                 formatResult: AGB.statement_text,
                                                 initSelection: function(element) {
                                                     return AGB.get_statement_from_id(statements, element.val());
                                                 }});
                           });
                },
                PM.on_error);
    
    return false;
};

// Set the list of candidates for the conclusion
AGB.set_conclusion_candidates = function(atom, callback) {
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
                     conclusion.select2({data: {results: conclusion_statements_results,
                                                text: function(statement_result) {
                                                    return AGB.statement_raw_text(statement_result.statement);
                                                }
                                               },
                                         placeholder: "Select a conclusion",
                                         formatResult: AGB.format_filtered_matching_result,
                                         formatSelection:
                                         AGB.format_selected_matching_result,
                                         initSelection: function(element) {
                                             return element.data(element.val());
                                         }});

                     if(!_.isNil(callback)) {
                         callback();   
                     }
                 },
                 IMPACT.user,
                 IMPACT.password,
                 PM.on_error);

    // new statement link
    $('#new-statement-for-conclusion').unbind('click');
    $('#new-statement-for-conclusion').click(
        _.bind(AGB.argumentgraph_newstatement,
               AGB,
               {atom: atom.replace(/\?/g, ''),
                save_callback: function(stmt_id) {
                    AGB.update_conclusion_premises_candidates(
                        function() {
                            console.log('statement id: ' + stmt_id);
                            $('#editor-conclusion').val(stmt_id).trigger('change');
                    }); 
                }}));
};


// Set the list of candidates for an argument premise, assumption or
// exception 
// @id is the id of section, either 'argument-premises',
// 'argument-assumptions' or 'argument-exceptions' 
// @atom is the atom of
// the premise on which the current substitutions of the argument
// being edited have been applied
AGB.set_premise_candidates = function(id, premise, atom, callback) {
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
                     p.select2({data: {results: premise_results,
                                       text: function(statement_result) {
                                           return AGB.statement_raw_text(statement_result.statement);
                                       }
                                    },
                                placeholder: "Select a statement",
                                formatResult: AGB.format_filtered_matching_result,
                                formatSelection: AGB.format_selected_matching_result,
                                initSelection: function(element) {
                                    return element.data(element.val());
                                }
                               });
                     
                     if(!_.isNil(callback)) {
                         callback();
                     }
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
                           function(stmt_id) {
                               AGB.update_conclusion_premises_candidates(
                                   function() {
                                       p.val(stmt_id).trigger('change');
                                   });
                           }
                           }));

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
AGB.update_premises_candidates = function(id, subs, callback, premise) {
    PM.ajax_post(IMPACT.wsurl + '/apply-substitutions',
                 {statement: AGB.sexpr_to_str(premise.statement.atom),
                  substitutions: subs},
                 function(atom) {
                     AGB.set_premise_candidates(id,
                                                premise,
                                                AGB.sexpr_to_str(atom),
                                                callback);
                 },
                 IMPACT.user,
                 IMPACT.password,
                 PM.on_error);
};

// Updates conclusion candidates
AGB.update_conclusion_candidates = function(conclusion, subs, callback) {
  PM.ajax_post(IMPACT.wsurl + '/apply-substitutions',
                 {statement: AGB.sexpr_to_str(conclusion),
                  substitutions: subs},
                 function(atom) {
                     AGB.set_conclusion_candidates(AGB.sexpr_to_str(atom), callback);
                 },
                 IMPACT.user,
                 IMPACT.password,
                 PM.on_error);
};

// Updates candidates for the premises and conclusion to reflect the
// current substitutions
AGB.update_conclusion_premises_candidates = function(callback) {
    var subs = AGB.get_argument_substitutions();
    var scheme = AGB.current_scheme();

    var nb_calls_executed = 0;
    var nb_calls = 1 /* conclusion */ + scheme.premises.length +
        scheme.assumptions.length + scheme.exceptions.length;

    var on_ajax_calls_finished = function() {
        nb_calls_executed++;
        if(nb_calls_executed == nb_calls) {
            if(!_.isNil(callback)) {
                callback();
            }
        }
    }; 
    
    AGB.update_conclusion_candidates(scheme.conclusion, subs, on_ajax_calls_finished);
    _.each(scheme.premises, _.bind(AGB.update_premises_candidates, AGB, '#argument-premises', subs, on_ajax_calls_finished));
    _.each(scheme.assumptions, _.bind(AGB.update_premises_candidates, AGB, '#argument-assumptions', subs, on_ajax_calls_finished));  
    _.each(scheme.exceptions, _.bind(AGB.update_premises_candidates, AGB, '#argument-exceptions', subs, on_ajax_calls_finished));

};

// Returns the current selected scheme of the argument editor
AGB.current_scheme = function() {
    var val = $('#argument-editor-scheme').val();
    return $('#argument-editor-scheme').data(val);
};

// Called when the argument scheme is changed
AGB.scheme_changed = function() {
    $('#argument-editor-conclusion-and-premises').show();
    
    var id = $('#argument-editor-scheme').val();
    console.log('scheme changed: ' + id);
    PM.ajax_get(IMPACT.wsurl + '/scheme/' + id,
                function(scheme) {
                    $('#argument-editor-scheme').data(id, scheme);
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
    if(!$('#argument-editor-scheme').valid()) {
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

};
