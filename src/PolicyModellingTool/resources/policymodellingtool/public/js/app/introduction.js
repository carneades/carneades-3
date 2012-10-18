PM.introduction_url = function() {
    return 'introduction';    
};

PM.set_introduction_url = function() {
    $.address.value(PM.introduction_url());  
};

PM.display_introduction = function() {
    PM.ajax_post(IMPACT.simulation_url, {"current-policy": null},
                 function(currentpolicy) {
                     IMPACT.current_policy = currentpolicy; 
                 },
                IMPACT.user,
                IMPACT.password,
                PM.on_error);

    var introduction_html = ich.introduction();
    $('#pm').html(introduction_html.filter("#introduction"));
    $('#start').click(PM.on_start_button);
    PM.activate('#introduction-item');
};

PM.on_start_button = function() {

    // reset session
    PM.ajax_post(IMPACT.simulation_url, {reset: null}, function() {},
                IMPACT.user,
                IMPACT.password,
                PM.on_error);

    PM.set_issues_url();
    return false;
};

PM.call_template = function(template_name, variables) {
    return ich[template_name](variables);
};