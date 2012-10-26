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
                    current_policy.outline_text = PM.theory_outline_text(current_policy.sections,
                                                                         'policies');
                    current_policy.description_text = current_policy.header.description[IMPACT.lang];
                    current_policy.policies_text = PM.policies_text(current_policy.language, current_policy.sections, 2,
                                                             function(policyid) {
                                                                 ids.push(policyid);
                                                             });
                    
                    var current_policy_html = ich.policies(current_policy);
                    $('#pm').html(current_policy_html.filter("#policies"));
                    PM.activate('#policies-item');
                    
                    _.each(ids, function(policyid) {
                               $('#input' + policyid).click(_.bind(PM.on_select_policy, PM, policyid));
                           });
                    
                    if(sectionid != undefined) {
                        $.scrollTo($('#' + sectionid));
                    }

                    // hack
                    $('a:contains(argument map)').click(
                        function() {
                            PM.set_arguments_url('copyright');       
                            return false;    
                        }
                    );

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

PM.theory_outline_text = function(sections, urlfragment) {
    var text = "<ul>";
    
    _.each(sections, function(section) {
               text += '<li><a href="#/{0}/{1}">{2}</a></li>'.format(urlfragment, section.id, section.header.title);
               text += PM.theory_outline_text(section.sections, urlfragment);
           });

    text += "</ul>";
    
    return text;
};

PM.schemes_text = function(language, schemes) {
    var text = "<div>";
    
    _.each(schemes, function(scheme) {
               if(scheme.header.description && scheme.header.description[IMPACT.lang]) {
                   text += '{0}'.format(PM.markdown_to_html(scheme.header.description[IMPACT.lang]));
               }
               
               text += PM.scheme_content_text(language, scheme);
           });
    
    text += "</div>";
    
    return text;
};

PM.scheme_content_text = function(language, scheme) {
    var text = "";

    // TODO: this formatting code should be in a template
    text += '<div class="scheme-content" ><b>id:</b> {0}'.format(scheme.id);

    text += '<br><b>strict:</b> {0}'.format(scheme.strict);
    
    text += '<br><b>direction:</b> {0}'.format(scheme.pro ? "pro" : "con");

    text += '<br><b>conclusion:</b> {0}'.format(PM.format_sexpr(scheme.conclusion, language));

    text += '<br><b>premises:</b><div class="rule-body"> <ul>';
     

    _.each(scheme.premises, function(premise) {
               if(premise.statement.atom[0] != "valid") {
                   text += "<li>{0}</li>".format(PM.format_sexpr(premise.statement.atom, language));
               }
           });
    text += '</ul></div>';

    if(scheme.assumptions.length > 0) {
        text += '<b>assumptions</b>:</b><div class="rule-body"> <ul>';

        _.each(scheme.assumptions, function(premise) {
                   text += "<li>{0}</li>".format(PM.format_sexpr(premise.statement.atom, language));
               }); 

        text += '</ul></div>';
    }

    if(scheme.exceptions.length > 0) {
        text += '<b>exceptions:</b><div class="rule-body"> <ul>';

        _.each(scheme.exceptions, function(premise) {
                   text += "<li>{0}</li>".format(PM.format_sexpr(premise.statement.atom, language));
               }); 
        
        text += '</ul></div>';
    }

    

    
    text += '</div>'; // end of scheme-content stuff
    
    return text;
};

PM.policies_text = function(language, sections, level, on_policy) {
    var text = "";
    
    _.each(sections, function(section) {
               text += '<div id="{0}">'.format(section.id);
               text += '<form action=""><h{0}>'.format(level + 1);
               if(section.schemes.length > 0) {
                   on_policy(section.id);
                   text += '<input type="submit" value="Select" id="input{0}" />'.format(section.id);
               }
               text += ' {1}</h{0}></form>'.format(level + 1, section.header.title);
               text += '<p>{0}</p>'.format(PM.markdown_to_html(section.header.description[IMPACT.lang]));
               text += PM.schemes_text(language, section.schemes);
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
// or a string. A single string represents an atom (NOT a sexpr).
PM.format_sexpr = function(sexpr, language) {
    if(typeof sexpr == 'string') {
        // hack
        return UTILS.escape_html(sexpr.substr(1));
    }
    
    var terms = _.map(sexpr.slice(1), PM.varname);
    if(_.isNil(language[sexpr[0]])) {
        return UTILS.escape_html(sexpr.join(" "));
    } else {
        var fstring = language[sexpr[0]].forms[IMPACT.lang].positive;
        var args = [fstring].concat(terms);
        return sprintf.apply(null, args);        
    } 
};
