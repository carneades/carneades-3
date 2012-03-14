PM.policies_url = function() {
    return 'arguments';    
};

PM.set_policies_url = function() {
    $.address.value(PM.policies_url());  
};

PM.display_policies = function() {
    var policies_html = ich.policies();
    $('#pm').html(policies_html.filter("#policies"));
    PM.activate('#policies-item');
    
    // TODO get the policies from JSON
    $('#Q12-Aktionsbundnis').click(_.bind(PM.on_select_policy, PM, 'Q12-Aktionsbundnis'));

};

PM.on_select_policy = function(id) {
    PM.ajax_post(IMPACT.evaluation_url, {'evaluate': {policy: id}}, PM.on_evaluated_policy);
}

PM.on_evaluated_policy = function(data) {
    IMPACT.db = data.db;
    PM.display_arguments();
}
