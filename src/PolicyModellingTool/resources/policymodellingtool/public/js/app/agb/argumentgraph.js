AGB.set_argumentgraph_url = function(db)
{
    $.address.value(AGB.argumentgraph_url(db));    
};

AGB.argumentgraph_url = function(db)
{
    return '/argumentgraph/' + db;
};

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

AGB.display_argumentgraph = function(db)
{
    PM.ajax_get(IMPACT.wsurl + '/argumentgraph-info/' + db,
                function(data) {
                    $('#browser').html(AGB.argumentgraph_html(db, data));
                    $('#export').click(
                        function (event){
                            window.open('/impactws/export/{0}'.format(db),
                                        'CAF XML');
                            return false; 
                        });
                    $('#edit').click(AGB.edit_argumentgraph);
                },
                PM.on_error);
};

AGB.set_mainissues_text = function(mainissues)
{
    $.each(mainissues, 
           function(index, issue) {
               issue.statement_nb = index + 1;
               issue.statement_text = AGB.statement_text(issue);
           });
    return mainissues;
};

AGB.set_references_text = function(metadata)
{
    $.each(metadata, 
           function(index, md) {
               md.metadata_text = AGB.format_metadata(md);
           });
};

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

AGB.edit_argumentgraph = function() {
    $('#ageditormenu').remove();
    $('#menus').append(ich.ageditormenuon());
    $('#newstatement').click(_.bind(AGB.argumentgraph_newstatement, AGB,
                                   {save_callback: function() {
                                        AGB.display_argumentgraph(IMPACT.db);
                                        return false;
                                    }}));
    $('#newargument').click(AGB.argumentgraph_newargument);
    
    return false;
};

// config accepts the following values
// atom: the default value of the atom field
// save_callback: a callback called once the statement is created

AGB.argumentgraph_newstatement = function(config) {
    if(_.isNil(config)) {
        config = {}; 
    }
    $('#statementeditor').html(AGB.create_statement_editor());
    $('#statement-header').html(AGB.create_metadata_editor());
    $('#cancel-statement').click(AGB.remove_statement_editor);
    $('input:radio[name=main]:nth(0)').attr('checked',true);
    
    if(!_.isNil(config.atom)) {
        $('#editor-statement-atom').val(config.atom);        
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
    return AGB.format_selected_statement(result.statement);
};

AGB.argumentgraph_newargument = function() {
    $('#argumenteditor').html(AGB.create_argument_editor());
    var search_term = "";

    var scheme_search_term = "";
    $('#editor-argument-scheme').select2({formatResult: AGB.format_filtered_scheme,
                                          formatSelection: AGB.format_selected_scheme, 
                                          dataType: 'json',
                                          type: 'GET',
                                          error: function(jqXHR, textStatus) {
                                              console.log('[ERROR] AJAX ' + textStatus);
                                          },
                                          ajax: {
                                              url: IMPACT.wsurl + '/scheme',
                                              data: function(term, page) {
                                                  console.log('term entered by the user: ' + term);
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

    $('#cancel-argument').click(AGB.remove_argument_editor);
    $('#save-argument').click(AGB.save_argument_display_graph);
    $('#editor-argument-scheme').change(AGB.scheme_changed);
    
    return false;
};

AGB.fill_conclusion_select = function(scheme) {
    PM.ajax_post(IMPACT.wsurl + '/matching-statements/' + IMPACT.db,
                             AGB.sexpr_to_str(scheme.conclusion),
                             function(conclusion_statements_results) {
                                 _.each(conclusion_statements_results, function(result) {
                                            $('#editor-conclusion').data(result.statement.id, 
                                                                         result);
                                            result.id = result.statement.id;
                                        });
                                 
                                 $('#editor-conclusion').select2("destroy");
                                 $('#editor-conclusion').select2(
                                     {data: conclusion_statements_results,
                                      // placeholder: conclusion_statements_results[0],
                                      formatResult: AGB.format_filtered_matching_result,
                                      formatSelection: AGB.format_selected_matching_result});
                             },
                IMPACT.user,
                IMPACT.password,
                PM.on_error);
    
    $('#new-statement-for-conclusion').click(_.bind(AGB.argumentgraph_newstatement, AGB, {atom: AGB.sexpr_to_str(scheme.conclusion),
                                                                                          save_callback: _.bind(AGB.fill_conclusion_select, AGB, scheme)}));
};

AGB.fill_premise = function(id, premise) {
    var p = $(_.filter($(id + ' .statement-select'),
                     function(p) {
                         return $(p).data('role') == premise.role;
                     })[0]);
                                
    PM.ajax_post(IMPACT.wsurl + '/matching-statements/' + IMPACT.db,
                 AGB.sexpr_to_str(premise.statement.atom),
                 function(premise_results) {
                     _.each(premise_results, function(result) {
                                p.data(result.statement.id, result);
                                result.id = result.statement.id;
                            });
                     
                     p.select2("destroy");   
                     p.select2({data: premise_results,
                                //                                placeholder: premise_results[0],
                                formatResult: AGB.format_filtered_matching_result,
                                formatSelection: AGB.format_selected_matching_result});
                     },
                IMPACT.user,
                IMPACT.password,
                PM.on_error);
    
};

AGB.add_premises = function(id, premises) {
    _.each(premises,
          function(premise) {
              var premise_form = ich.premiseeditor();
              $(id).append(premise_form);
              $(id + ' input[class=role-input]:last').val(premise.role);
              $(id + ' input[class=role-input]:last').prop('disabled', true);
              
              $(id + ' .statement-select').last().data('role', premise.role);
              $(id + ' a:last').click(_.bind(AGB.argumentgraph_newstatement, AGB, {atom: AGB.sexpr_to_str(premise.statement.atom),
                                                                                                save_callback: _.bind(AGB.fill_premise, AGB, id, premise)})); 
              AGB.fill_premise(id, premise);
          }); 
};

AGB.scheme_changed = function() {
    var id = $('#editor-argument-scheme').val();
    console.log('scheme changed: ' + id);
    PM.ajax_get(IMPACT.wsurl + '/scheme/' + id,
                function(scheme) {
                    AGB.fill_conclusion_select(scheme);
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