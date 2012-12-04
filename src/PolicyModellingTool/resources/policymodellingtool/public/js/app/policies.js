PM.policies_url = function() {
    return 'arguments';    
};

PM.set_policies_url = function() {
    $.address.value(PM.policies_url());  
};

PM.current_issue = function() {
    return PM.statements.filter(function(s) { return s.get('main'); })[0].toJSON();
};

PM.get_issue_text = function() {
    var issue_text = $.i18n.prop('pmt_issue_not_selected');

    if(PM.statements.length > 0) {
        var issue = PM.current_issue();
        return AGB.statement_raw_text(issue);
    }
    
    return issue_text;
};

PM.on_policy_filtering = function(event) {
    var filter = $(event.target).attr('id');
    console.log('filtering policy: ' + filter);

    if(filter == 'all') {
        PM.display_policies(undefined, undefined);
    } else {
        PM.ajax_get(IMPACT.wsurl + '/find-policies/{0}/{1}/{2}/{3}/{4}'
                    .format(IMPACT.db, 
                            IMPACT.current_policy, 
                            IMPACT.question, 
                            PM.current_issue().id,
                            filter),
                   function(policies) {
                       console.log('find-policies returns:');
                       console.log(policies);
                   });
    }
};

// sectionid, optional, is the section to jump to
// subset, optional, is a subset of policies to show
PM.display_policies = function(sectionid, subset) {
    IMPACT.facts_state = 'done';
    
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
                    
                    var template_variables = _.clone(current_policy);
                    _.extend(template_variables, 
                             {current_issue: $.i18n.prop('pmt_current_issue'),
                              issue: PM.get_issue_text(),
                              can_display: $.i18n.prop('pmt_can_display'),
                              all_policies: $.i18n.prop('pmt_all_policies'),
                              policies_making_in: $.i18n.prop('pmt_policies_making_in'),
                              policies_making_out: $.i18n.prop('pmt_policies_making_out'),
                              policies_making_undecided: $.i18n.prop('pmt_policies_making_undecided')
                             });
                    var current_policy_html = ich.policies(template_variables);
                    $('#pm').html(current_policy_html.filter("#policies"));
                    PM.activate('#policies-item');
                    
                    _.each(ids, function(policyid) {
                               $('#input' + policyid).click(_.bind(PM.on_select_policy, PM, policyid));
                           });
                    
                    if(sectionid != undefined) {
                        $.scrollTo($('#' + sectionid));
                    }
                    
                    $('.policy-filtering').click(PM.on_policy_filtering);

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

    

    
    text += '</div><br/>'; // end of scheme-content stuff
    
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
