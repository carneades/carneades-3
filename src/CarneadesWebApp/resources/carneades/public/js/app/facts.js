// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

// Returns the relative url of the facts page
PM.facts_url = function() {
    return 'facts';
};

// Sets the current URL to the facts URL.
// This will cause the facts page to be displayed.
PM.set_facts_url = function() {
  $.address.value(PM.facts_url());
};

// Displays the questions
PM.display_facts = function() {
    if(IMPACT.db == undefined) {
        var facts_html = ich.facts(PM.merge_menu_props({}));
        $('#pm').html(facts_html.filter("#facts"));
        PM.activate('#facts-item');
        PM.attach_lang_listener();
        catb.views.pmt.questions.init_show_questions();
    } else {
        var facts_html = ich.ask_modify_facts(PM.merge_menu_props(
            {'pmt_facts_already_submitted': $.i18n.prop('pmt_facts_already_submitted'),
             'pmt_modify_facts': $.i18n.prop('pmt_modify_facts')}));
        
        $('#pm').html(facts_html.filter("#facts"));
        PM.activate('#facts-item');
        PM.attach_lang_listener();
    }
};
