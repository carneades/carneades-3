PM.display_introduction = function() {
    var introduction_html = ich.introduction();
    $('#pm').html(introduction_html);
    $('#start').click(PM.on_start_button);
};

PM.on_start_button = function() {
    PM.set_issues_url();
    return false;  
};