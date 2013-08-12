// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

goog.provide('carneades.policy_analysis.web.introduction');

PM.introduction_url = function() {
    return 'policies/introduction/' + IMPACT.project;    
};

PM.set_introduction_url = function() {
    $.address.value(PM.introduction_url());  
};

PM.policies_menu = function(caseid) {
    return [{text: 'pmt_menu_intro',
             link: '#/policies/introduction/' + PM.project.id},
            {text: 'pmt_menu_issues'
             ,link: "#/policies/issues/" + PM.project.id},
            {text: 'pmt_menu_facts'
             ,link: "#/policies/facts/" + PM.project.id},
            {text: 'pmt_menu_policies'
             ,link: "#/policies/policies/" + PM.project.id},
            {text: 'pmt_menu_analysis'
             ,link: "#/arguments/outline/" + PM.project.id + '/' + caseid},
            {text: 'pmt_menu_report'
             ,link: "#/policies/report/" + PM.project.id + '/' + caseid}
           ];
};

PM.display_introduction = function(project) {
    PM.load_project(project);
    
    PM.show_menu({text: PM.project.get('title'),
                  link: "#/project/" + PM.project.id},
                 PM.policies_menu(undefined));
    
    var introduction_html = ich.introduction(
        PM.merge_menu_props({pmt_pmt: $.i18n.prop('pmt_pmt'),
                             pmt_start: $.i18n.prop('pmt_start'),
                             pmt_intro_pmt: $.i18n.prop('pmt_intro_pmt'),
                             pmt_intro: PM.markdown_to_html($.i18n.prop('pmt_intro'))
                            }));
    $('#pm').html(introduction_html.filter("#introduction"));
    $('#start').click(PM.on_start_button);
    PM.activate('#introduction-item');
    // PM.attach_lang_listener();
};

PM.on_start_button = function() {
    // reset session
    PM.ajax_post(IMPACT.simulation_url, {reset: {lang: IMPACT.lang}}, function() {},
                IMPACT.user,
                IMPACT.password,
                PM.on_error);

    PM.set_issues_url();
    return false;
};

PM.call_template = function(template_name, variables) {
    return ich[template_name](variables);
};
