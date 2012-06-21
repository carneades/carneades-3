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
    data.normalize();
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
                 $('#export').click(function (event){
                                        window.open('/impactws/export/{0}'.format(db), 'CAF XML');
                                        return false; 
                                    });
                 $('#edit').click(AGB.edit_argumentgraph);
             });
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
    $('#newstatement').click(AGB.argumentgraph_newstatement);
    $('#newargument').click(AGB.argumentgraph_newargument);
    
    return false;
};

AGB.argumentgraph_newstatement = function() {
    $('#statementeditor').html(AGB.create_statement_editor());
    $('#statement-header').html(AGB.create_metadata_editor());
    $('#save-statement').click(AGB.save_statement_display_graph);
    $('#cancel-statement').click(AGB.remove_statement_editor);
    $('input:radio[name=main]:nth(0)').attr('checked',true);
    
    return false;
};

AGB.save_statement_display_graph = function() {
    AGB.save_statement();
    AGB.display_argumentgraph(IMPACT.db);
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

    var statement_config = {formatResult: AGB.format_filtered_statement,
                            formatSelection: AGB.format_selected_statement, 
                            dataType: 'json',
                            type: 'GET',
                            error: function(jqXHR, textStatus) {
                                console.log('[ERROR] AJAX ' + textStatus);
                            },
                            ajax: {
                                url: IMPACT.wsurl + '/statement/' + IMPACT.db,
                                data: function(term, page) {
                                    console.log('term entered by the user: ' + term);
                                    search_term = term;
                                    return {};
                                },
                                results: function(statements) {
                                    return {
                                        results: _.filter(statements, function(statement) {
                                                              return AGB.statement_text(statement).indexOf(search_term) != -1;
                                                          })
                                    };
                                }
                            }
                           };
    $('#editor-conclusion').select2(statement_config);

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
    $('#new-statement-for-conclusion').click(AGB.argumentgraph_newstatement);
    $('#new-statement-for-premise').click(AGB.argumentgraph_newstatement);
    $('#new-statement-for-exception').click(AGB.argumentgraph_newstatement);

    $('#argument-premises input[type=hidden]').select2(statement_config);
    $('#argument-exceptions input[type=hidden]').select2(statement_config);
    $('#add-more-premises').click(_.bind(AGB.add_more_premises, PM, statement_config));
  //  $('#add-more-exceptions').click(_.bind(AGB.add_more_exceptions, PM, statement_config));
    $('#cancel-argument').click(AGB.remove_argument_editor);
    $('#save-argument').click(AGB.save_argument_display_graph);
    $('#editor-argument-scheme').change(AGB.scheme_changed);
    
    return false;
};

AGB.scheme_changed = function() {
    var id = $('#editor-argument-scheme').val();
        PM.ajax_get(IMPACT.wsurl + '/scheme/' + id,
             function(scheme) {
                 PM.ajax_post(IMPACT.wsurl + '/matching-statements/' + IMPACT.db,
                             AGB.sexpr_to_str(scheme.conclusion),
                             function(conclusion_statements_results) {
                                 _.each(conclusion_statements_results, function(result) {
                                            $('#editor-conclusion').data(result.statement.id, 
                                                                         result);
                                            result.id = result.statement.id;
                                        });
                                 
                                 $('#editor-conclusion').select2(
                                     {data: conclusion_statements_results,
                                      formatResult: AGB.format_filtered_matching_result,
                                      formatSelection: AGB.format_selected_matching_result});
                             });
             }
                   );
};

AGB.add_more_premises = function(statement_config) {
    for(var i = 0; i < 3; i++) {
//        $('#argument-premises').append('<input type="hidden" class="statement-select" /> <br/>');        
    } 
    $('#argument-premises input').select2(statement_config);
};

AGB.add_more_exceptions = function(statement_config) {
    for(var i = 0; i < 2; i++) {
//        $('#argument-exceptions').append('<input type="hidden" class="statement-select" /> <br/>');
    }
    $('#argument-exceptions input').select2(statement_config);
};


AGB.save_argument_display_graph = function() {
    AGB.save_argument();
    AGB.display_argumentgraph(IMPACT.db);
    return false;
};