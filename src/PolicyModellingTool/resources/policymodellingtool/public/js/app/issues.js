// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

PM.issues_url = function() {
    return 'issues';
};

PM.set_issues_url = function() {
    $.address.value(PM.issues_url());
};

PM.display_issues = function() {
    PM.ajax_post(IMPACT.simulation_url, {"current-policy": null},
                 function(currentpolicy) {
                     IMPACT.facts_state = 'waiting';
                     IMPACT.current_policy = currentpolicy;
                     PM.ajax_get(IMPACT.wsurl + "/policies",
                                 function(policies) {
                                     var data = _.clone(policies[currentpolicy]);
                                     data.pmt_issues_desc = $.i18n.prop('pmt_issues_desc');
                                     data.pmt_menu_intro = $.i18n.prop('pmt_menu_intro');
                                     data.pmt_menu_issues = $.i18n.prop('pmt_menu_issues');
                                     data.pmt_menu_facts = $.i18n.prop('pmt_menu_facts');
                                     data.pmt_menu_arguments = $.i18n.prop('pmt_menu_arguments');
                                     data.pmt_menu_schemes = $.i18n.prop('pmt_menu_schemes');
                                     data.pmt_menu_policies = $.i18n.prop('pmt_menu_policies');
                                     data.pmt_submit = $.i18n.prop('pmt_submit');

                                     var issues_html = ich.issues(data);
                                     $('#pm').html(issues_html.filter("#issues"));
                                     $('input').first().attr('checked', true);
                                     $('#submit').click(PM.on_submit_issues);
                                     PM.activate('#issues-item');
                                 },
                                 IMPACT.user,
                                 IMPACT.password,
                                 PM.on_error);
                 },
                 IMPACT.user,
                 IMPACT.password,
                 PM.on_error);
};

PM.on_submit_issues = function() {
    if($('#issuesform').valid()) {
        IMPACT.question = $('input[name="issue"]:checked').val();
        PM.set_facts_url();
    }
    return false;
};
