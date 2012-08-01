PM.policies_url = function() {
    return 'arguments';    
};

PM.set_policies_url = function() {
    $.address.value(PM.policies_url());  
};

PM.display_policies = function(sectionid) {
    PM.ajax_get(IMPACT.wsurl + '/policies', 
                function(policies) {
                    var ids = [];
                    var current_policy = policies[IMPACT.current_policy];
                    current_policy.outline_text = PM.policies_outline_text(current_policy.sections);
                    current_policy.description_text = current_policy.header.description[IMPACT.lang];
                    current_policy.policies_text = PM.policies_text(current_policy.language, current_policy.sections, 2,
                                                             function(policyid) {
                                                                 ids.push(policyid);
                                                             });
                    
                    var current_policy_html = ich.policies(current_policy);
                    $('#pm').html(current_policy_html.filter("#policies"));
                    PM.activate('#policies-item');
                    
                    _.each(ids, function(policyid) {
                               $('#' + policyid).click(_.bind(PM.on_select_policy, PM, policyid));
                           });
                    
                    if(sectionid != undefined) {
                        $.scrollTo($('#' + sectionid));
                    }

                },
               PM.on_error);
    

};

PM.on_select_policy = function(id) {
    console.log('db before evaluate: ' + IMPACT.db);
    PM.ajax_get(IMPACT.wsurl + '/evaluate-policy/{0}/{1}/{2}/{3}'.
                format(IMPACT.db, IMPACT.current_policy, IMPACT.question, id),
                PM.on_evaluated_policy,
                PM.on_error);
    return false;
};

PM.on_evaluated_policy = function(data) {
    IMPACT.db = data.db;
    console.log('db after evaluate: ' + IMPACT.db);
    PM.set_arguments_url(IMPACT.db);
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

PM.schemes_text = function(language, schemes) {
    var text = "<ul>";
    
    _.each(schemes, function(scheme) {
               text += '<li>{0}'.format(PM.markdown_to_html(scheme.header.description[IMPACT.lang]));
               text += '<p>{0}, {1}'.format(scheme.id, PM.format_sexpr(scheme.conclusion, language));
               text += '<ul>';
               _.each(scheme.premises, function(premise) {
                          if(premise.statement.atom[0] != "valid") {
                              text += "<li>{0}</li>".format(PM.format_sexpr(premise.statement.atom, language));
                          }
                      });
               text += '</ul>';
               text += '</p>';
               text += '</li>';
           });
    
    text += "</ul>";
    
    return text;
};

PM.policies_text = function(language, sections, level, on_policy) {
    var text = "";
    
    _.each(sections, function(section) {
               text += '<div id="{0}">'.format(section.id);
               text += '<form action=""><h{0}>'.format(level);
               if(section.schemes.length > 0) {
                   on_policy(section.id);
                   text += '<input type="submit" value="Select" id="input{0}" />'.format();
               }
               text += ' {1}</h{0}></form>'.format(level, section.header.title);
               text += '<p>{0}</p>'.format(PM.markdown_to_html(section.header.description[IMPACT.lang]));
               text += PM.markdown_to_html(PM.schemes_text(language, section.schemes));
               text += PM.policies_text(language, section.sections, level + 1, on_policy);
               text += '</div>';
           });
    
    return text;
};

// Returns the varname of a var
PM.varname = function(v) {
    if(v[0] == '?') {
        return v.slice(1);        
    }
    return v;
};

// Formats an sexpr represented as an array of strings
PM.format_sexpr = function(sexpr, language) {
    var terms = _.map(sexpr.slice(1), PM.varname);
    if(_.isNil(language[sexpr[0]])) {
        return PM.markdown_to_html(sexpr.join(" "));
    } else {
        var fstring = language[sexpr[0]].forms[IMPACT.lang].positive;
        return String.prototype.cformat.apply(fstring, terms);        
    } 
};
