PM.issues_url = function() {
    return 'issues';
};

PM.set_issues_url = function() {
    $.address.value(PM.issues_url());
};

PM.display_issues = function() {
    var issues_html = ich.issues();
    $('#pm').html(issues_html.filter("#issues"));
    $('#submit').click(PM.on_submit_issues);
};

PM.on_submit_issues = function() {
    PM.set_facts_url();
    return false;  
};