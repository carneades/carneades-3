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
                                     var issues_html = ich.issues(policies[currentpolicy]);
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