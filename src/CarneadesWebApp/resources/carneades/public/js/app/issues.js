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
                     IMPACT.current_policy = currentpolicy;
                     PM.ajax_get(IMPACT.wsurl + "/policies",
                                 function(policies) {
                                     var data = _.clone(policies[currentpolicy]);
                                     data.pmt_issues_desc = $.i18n.prop('pmt_issues_desc');
                                     data = PM.merge_menu_props(data);
                                     data.pmt_submit = $.i18n.prop('pmt_submit');
                                     
                                     var issues_html = ich.issues(data);
                                     $('#pm').html(issues_html.filter("#issues"));
                                     $('input').first().attr('checked', true);
                                     $('#submit').click(PM.on_submit_issues);
                                     PM.activate('#issues-item');
                                     PM.attach_lang_listener();
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
        IMPACT.db = undefined;
        PM.set_facts_url();
    }
    return false;
};
