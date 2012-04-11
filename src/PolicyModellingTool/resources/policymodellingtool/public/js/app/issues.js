PM.issues_url = function() {
    return 'issues';
};

PM.set_issues_url = function() {
    $.address.value(PM.issues_url());
};

PM.display_issues = function() {
    PM.ajax_get(IMPACT.impactws_url + '/policies',
                function(policies) {
                    var issues_html = ich.issues(policies);
                    $('#pm').html(issues_html.filter("#issues"));
                    $('#submit').click(PM.on_submit_issues);
                    PM.activate('#issues-item');                        
                });
};

PM.on_submit_issues = function() {
    IMPACT.question = $('input[name="issue"]:checked').val();
    PM.set_facts_url();
    return false;  
};