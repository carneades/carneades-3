PM.policies_url = function() {
    return 'arguments';    
};

PM.set_policies_url = function() {
    $.address.value(PM.policies_url());  
};

PM.display_policies = function(sectionid) {
    PM.ajax_get(IMPACT.impactws_url + '/policies', 
                function(policies) {
                    policies.outline_text = PM.policies_outline_text(policies.sections);
                    var policies_html = ich.policies(policies);
                    $('#pm').html(policies_html.filter("#policies"));
                    PM.activate('#policies-item');
                    
                    // TODO get the policies from JSON
                    $('#Q12-Aktionsbundnis').click(_.bind(PM.on_select_policy, PM, 'Q12-Aktionsbundnis'));
                    if(sectionid != undefined) {
                        $.scrollTo($('#' + sectionid));                        
                    }

                });
    

};

PM.on_select_policy = function(id) {
    PM.ajax_post(IMPACT.evaluation_url, {'evaluate': {policy: id}}, PM.on_evaluated_policy);
};

PM.on_evaluated_policy = function(data) {
    IMPACT.db = data.db;
    PM.display_arguments();
};

PM.policies_outline_text = function(sections) {
    var text = "<ul>";
    
    _.each(sections, function(section) {
               text += '<li><a href="#/policies/{0}">{1}</a></li>'.format(section.id, section.header.title);
               text += PM.policies_outline_text(section.sections);
           });

    text += "</ul>";
    
    return text;
};
