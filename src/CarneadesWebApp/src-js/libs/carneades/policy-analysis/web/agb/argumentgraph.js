// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

goog.provide('carneades.policy_analysis.web.agb.argumentgraph');

// Sets the argument graph url. This causes the argument graph to
// displayed.
AGB.set_argumentgraph_url = function(db)
{
    $.address.value(AGB.argumentgraph_url(db));
};

// Returns the relative URL of the argument graph page
AGB.argumentgraph_url = function(db)
{
    return '/arguments/outline/' + IMPACT.project + '/' + db;
};

// Returns the HTML content of the argument graph page
AGB.argumentgraph_html = function(db, data)
{
    AGB.normalize(data);
    data.db = db;
    data.metadata_text = AGB.format_metadata(data.metadata[0]);
    data.hasdescription = PM.has_description(data.metadata[0]);
    data.description_text = AGB.description_text(data.metadata[0]);
    AGB.set_mainissues_info(data.main_issues);
    data.references = data.metadata.filter(function (ref) { return ref.key; });
    data.hasreferences = data.references.length > 0;
    AGB.set_references_text(data.references);
    data.title = data.metadata[0].title ? data.metadata[0].title[0] : $.i18n.prop('pmt_menu_arguments');
    data.outline_text = AGB.outline_text(data.outline, db);
    data.pmt_main_issues = $.i18n.prop('pmt_main_issues');
    data.pmt_outline = $.i18n.prop('pmt_outline');
    data.pmt_references = $.i18n.prop('pmt_references');

    data = PM.merge_menu_props(data);
    data = PM.merge_ag_menu_props(data);

    data.lang = IMPACT.lang;

    var argumentgraph_html = ich.argumentgraph(data);
    return argumentgraph_html.filter('#argumentgraph');
};

PM.agb_outline_menu = function (db) {
    return [{text: 'pmt_ag_menu_map'
             ,link: '#/arguments/map/' + PM.project.id + '/' + db},
            {text: 'pmt_ag_menu_vote'
             ,link: "#/arguments/vote/" + PM.project.id},
            {text: 'pmt_ag_menu_copy'
             ,link: "#/arguments/copy-case/" + PM.project.id},
            {text: 'pmt_ag_menu_export'
             ,link: "#/arguments/export/" + PM.project.id + '/' + db},
            {text: 'pmt_ag_menu_evaluate'
             ,link: "#/arguments/evaluate/" + PM.project.id + '/' + db},
            {text: 'pmt_new_statement'
             ,link: "#/arguments/outline/" + PM.project.id + '/' + db + '?edit=true&entity=statement'},
            {text: 'pmt_new_argument'
             ,link: "#/arguments/outline/" + PM.project.id + '/' + db + '?edit=true&entity=argument'}
           ];
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

    var data = PM.get_ag_info(db);

    PM.show_menu({text: PM.project.get('title'),
                  link: "#/project/" + PM.project.id},
                 PM.agb_outline_menu(db));

    $('#browser').html(AGB.argumentgraph_html(db, data));
    $('#export').click(
        function (event){
            window.open('/carneadesws/export/{0}/{1}'.format(IMPACT.project, db),
                        'CAF XML');
            return false;
        });

    if(PM.on_statement_edit()) {
        AGB.show_statement_editor({save_callback: function() {
            $.address.queryString('');
            return false;
        }});
    }

    if(PM.on_argument_edit()) {
        AGB.new_argument();
    }
};

AGB.statement_value_to_cssclass = function(statement) {
    if(AGB.statement_in(statement)) {
        return "in";
    }

    if(AGB.statement_out(statement)) {
        return "out";
    }

    return "undecided";

};

// Formats the text for the main issues
AGB.set_mainissues_info = function(mainissues)
{
    $.each(mainissues,
           function(index, issue) {
               issue.statement_nb = index + 1;
               issue.statement_text = AGB.statement_text(issue);
               issue.label_value = AGB.statement_value_to_cssclass(issue);
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
        text = '{0} <span class="argument-content">{1}</span> <ul>'.format(direction_text, AGB.argument_link(db, node.id, AGB.argument_text(node, index)));
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

// Returns true if the atom represented as a string is grounded
// (hack)
AGB.is_grounded = function(atom) {
    return atom.indexOf("?") == -1;
};

AGB.new_argument = function(conclusion) {
    var argument = undefined;
    if(conclusion) {
        argument = new PM.Argument({conclusion: conclusion});
    } else {
        argument = new PM.Argument();
    }

    var argument_candidate = new PM.ArgumentCandidate({argument: argument,
                                                       statements: PM.get_stmts(),
                                                       schemes: PM.schemes,
                                                       current_lang: IMPACT.lang});

    var argument_editor_view = new PM.ArgumentEditorView({model: argument_candidate,
                                                          title: 'New Argument'});

    argument_editor_view.render();
    $('#argumenteditor').html(argument_editor_view.$el);

    return false;
};

AGB.evaluate = function(callback) {
    PM.notify('Evaluating...');
    PM.ajax_post(IMPACT.wsurl + '/evaluate-argument-graph/' + IMPACT.project + '/' + IMPACT.db, {},
                function(data) {
                    if(_.isFunction(callback)) {
                        callback();
                        PM.notify('Evaluation finished');
                    }
                },
                IMPACT.user, IMPACT.password, PM.on_error);
};
