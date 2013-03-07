// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

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

    var introduction_html = ich.introduction(
        PM.merge_menu_props({pmt_pmt: $.i18n.prop('pmt_pmt'),
                             pmt_start: $.i18n.prop('pmt_start'),
                             pmt_intro_pmt: $.i18n.prop('pmt_intro_pmt'),
                             pmt_intro: PM.markdown_to_html($.i18n.prop('pmt_intro'))
                            }));
    $('#pm').html(introduction_html.filter("#introduction"));
    $('#start').click(PM.on_start_button);
    PM.activate('#introduction-item');
    PM.attach_lang_listener();
};

PM.on_start_button = function() {
    // reset session
    PM.ajax_post(IMPACT.simulation_url, {reset: {lang: IMPACT.lang}}, function() {},
                IMPACT.user,
                IMPACT.password,
                PM.on_error);

    PM.set_issues_url();
    return false;
};

PM.call_template = function(template_name, variables) {
    return ich[template_name](variables);
};
