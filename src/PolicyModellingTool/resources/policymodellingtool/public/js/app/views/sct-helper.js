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
    var sct_issues = undefined;
    if(PM.debate_info.get('main-issues') == undefined) {
        PM.debate_info.fetch({success: function() {
                                  sct_issues = new PM.SctIssues(
                                      {model: PM.sct,
                                       issues: PM.debate_info.get('main-issues')});
                                  sct_issues.render();
                                  $('#pm').html(sct_issues.$el);
                              }
                             });
    } else {
        sct_issues = new PM.SctIssues(
            {model: PM.sct,
             issues: PM.debate_info.get('main-issues')});
        sct_issues.render();
        $('#pm').html(sct_issues.$el);  
    } 
};
