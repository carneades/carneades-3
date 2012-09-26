PM.display_sct_intro = function() {
    var sct_intro = new PM.SctIntro({model: PM.sct});
    sct_intro.render();
    $('#pm').html(sct_intro.$el);  
};

PM.sct_issues_url = function() {
    return '/sct/issues';
};

PM.set_sct_issues_url = function() {
    $.address.value(PM.sct_issues_url());
};

PM.display_sct_issues = function() {
    var sct_issues = new PM.SctIssues({model: PM.sct, 
                                       policies: PM.policies,
                                       current_policy: IMPACT.current_policy});
    
    if(PM.policies.get(PM.current_policy) == undefined) {
        PM.policies.fetch({success: function() {
                               sct_issues.render();
                           }
                           });
    } else {
        sct_issues.render();
    } 
    
    $('#pm').html(sct_issues.$el);  
};
