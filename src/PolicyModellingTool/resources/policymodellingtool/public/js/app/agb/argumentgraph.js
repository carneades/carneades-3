// Sets the argument graph url. This causes the argument graph to
// displayed.
AGB.set_argumentgraph_url = function(db)
{
    $.address.value(AGB.argumentgraph_url(db));    
};

// Returns the relative URL of the argument graph page
AGB.argumentgraph_url = function(db)
{
    return '/arguments/argumentgraph/' + db;
};

// Returns the HTML content of the argument graph page
AGB.argumentgraph_html = function(db, data) 
{
    AGB.normalize(data);
    data.db = db;
    data.metadata_text = AGB.format_metadata(data.metadata[0]);
    data.description_text = AGB.description_text(data.metadata[0]);
    AGB.set_mainissues_text(data.main_issues);
    data.references = data.metadata.filter(function (ref) { return ref.key; });
    AGB.set_references_text(data.references);
    data.title = AGB.markdown_to_html(data.metadata[0].title);
    data.outline_text = AGB.outline_text(data.outline, db);
    var argumentgraph_html = ich.argumentgraph(data);
    return argumentgraph_html.filter('#argumentgraph');
};

// Displays the argument graph page
AGB.display_argumentgraph = function(db)
{
    if($.address.value() == "/arguments") {
        // forces URL with specified argument graph without pushing
        // a new value in the history
        $.address.history(false);
        AGB.set_argumentgraph_url(db);
        $.address.history(true);
        return;
    }

    PM.ajax_get(IMPACT.wsurl + '/argumentgraph-info/' + db,
                function(data) {
                    $('#browser').html(AGB.argumentgraph_html(db, data));
                    $('#export').click(
                        function (event){
                            window.open('/impactws/export/{0}'.format(db),
                                        'CAF XML');
                            return false; 
                        });
                    AGB.enable_ag_edition();
                },
                PM.on_error);
};

// Formats the text for the main issues
AGB.set_mainissues_text = function(mainissues)
{
    $.each(mainissues, 
           function(index, issue) {
               issue.statement_nb = index + 1;
               issue.statement_text = AGB.statement_text(issue);
           });
    return mainissues;
};

// Formats the text for the references
AGB.set_references_text = function(metadata)
{
    $.each(metadata, 
           function(index, md) {
               md.metadata_text = AGB.format_metadata(md);
           });
};

// Returns the text for the outline
AGB.outline_text = function(tree, db, index)
{
    var text = "";
    var node = tree[0];
    var children = tree[1];
    
    if(node === "root") {
        text = "<ul>";
    } else if (node.hasOwnProperty('premises')) {
        var direction_text = node.pro ? "pro" : "con";
        text = '{0} <span class="content">{1}</span> <ul>'.format(direction_text, AGB.argument_link(db, node.id, AGB.argument_text(node, index)));
    } else {
        var stmt_text = AGB.statement_text(node, index);
        text = "{0} <ul>".format(AGB.statement_link(db, node.id, stmt_text));
    }

    $.each(children,
           function(index, child) {
               text += "<li>{0}</li>".format(AGB.outline_text(child, db, index + 1));
           });
    text += "</ul>";

    return text;
};

// Activates the edition of the argument graph
AGB.enable_ag_edition = function() {
    // $('#ageditormenu').remove();
    $('#menus').append(ich.ageditormenuon());
    $('#newstatement').click(_.bind(AGB.argumentgraph_newstatement, AGB,
                                    {save_callback: function() {
                                         AGB.display_argumentgraph(IMPACT.db);
                                         return false;
                                     }}));
    $('#newargument').click(AGB.argumentgraph_newargument);
    
    return false;
};

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

    $('#cancel-argument').click(AGB.remove_argument_editor);
    $('#save-argument').click(AGB.save_argument_display_graph);
    $('#editor-argument-scheme').change(AGB.scheme_changed);
    $('#editor-conclusion').change(AGB.conclusion_changed);


    $('#argument-editor-conclusion-and-premises').hide();
    
    return false;
};

// Returns true if the atom is grounded
// (hack)
AGB.is_grounded = function(atom) {
    return atom.indexOf("?") == -1;
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