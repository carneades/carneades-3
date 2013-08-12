// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

goog.provide('carneades.policy_analysis.web.facts');

// Returns the relative url of the facts page
PM.facts_url = function() {
    return '/policies/facts/' + IMPACT.project;
};

// Sets the current URL to the facts URL.
// This will cause the facts page to be displayed.
PM.set_facts_url = function() {
  $.address.value(PM.facts_url());
};

// Displays the questions
PM.display_facts = function(project) {
    PM.load_project(project);

    PM.show_menu({text: PM.project.get('title'),
                  link: "#/project/" + PM.project.id},
                 PM.policies_menu(undefined));

    if(IMPACT.db == undefined) {
        var facts_html = ich.facts(PM.merge_menu_props({}));
        $('#pm').html(facts_html.filter("#facts"));
        PM.activate('#facts-item');
        PM.attach_lang_listener();
        carneades.policy_analysis.web.views.pmt.questions.init_show_questions(
            {
                wsurl: IMPACT.simulation_url
            });
    } else {
        var facts_html = ich.ask_modify_facts(PM.merge_menu_props(
            {'pmt_facts_already_submitted': $.i18n.prop('pmt_facts_already_submitted'),
             'pmt_modify_facts': $.i18n.prop('pmt_modify_facts')}));

        $('#pm').html(facts_html.filter("#facts"));
        PM.activate('#facts-item');
        PM.attach_lang_listener();
    }
};
