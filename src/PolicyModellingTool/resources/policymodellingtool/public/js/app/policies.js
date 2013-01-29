// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

PM.policies_url = function() {
    return 'policies';    
};

PM.set_policies_url = function() {
    $.address.value(PM.policies_url());  
};

PM.current_issue = function() {
    if(PM.statements.length > 0) {
        return PM.statements.filter(function(s) { return s.get('main'); })[0].toJSON();
    }

    return undefined;
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
                   function(data) {
                       console.log('find-policies returns:');
                       console.log(data.policies);
                       PM.display_policies(undefined, data.policies); 
                   });
    }
};

PM.find_available_lang = function(current_policy) {
    if(!_.isNil(current_policy.header.description[IMPACT.lang])) {
        return IMPACT.lang;
    } else {
        return "en";
    }
};

// sectionid, optional, is the section to jump to
// subset, optional, is a subset of policies to show
PM.display_policies = function(sectionid, subset) {
    PM.ajax_get(IMPACT.wsurl + '/policies', 
                function(policies) {
                    var ids = [];
                    var current_policy = policies[IMPACT.current_policy];
                    var lang = PM.find_available_lang(current_policy);
                    
                    current_policy.outline_text = PM.theory_outline_text(current_policy.sections,
                                                                         'policies',
                                                                        subset);
                    current_policy.description_text = current_policy.header.description[lang];
                    var language_clj = catb.views.pmt.theory.convert_language(current_policy.language);
                    current_policy.policies_text = PM.policies_text(language_clj,
                                                                    current_policy.sections,
                                                                    2,
                                                                    subset,
                                                                    function(policyid) {
                                                                        ids.push(policyid);
                                                                    },
                                                                    lang);
                    
                    var template_variables = _.clone(current_policy);
                    _.extend(template_variables, 
                             PM.merge_menu_props({current_issue: $.i18n.prop('pmt_current_issue'),
                                                  issue: PM.get_issue_text(),
                                                  can_display: $.i18n.prop('pmt_can_display'),
                                                  all_policies: $.i18n.prop('pmt_all_policies'),
                                                  policies_making_in: $.i18n.prop('pmt_policies_making_in'),
                                                  policies_making_out: $.i18n.prop('pmt_policies_making_out'),
                                                  policies_making_undecided: $.i18n.prop('pmt_policies_making_undecided'),
                                                  pmt_intro_pmt: $.i18n.prop('pmt_intro_pmt'),
                                                  pmt_table_of_contents: $.i18n.prop('pmt_table_of_contents'),
                                                  pmt_see_effects: $.i18n.prop('pmt_see_effects'),
                                                  pmt_policies_filtering_indication: $.i18n.prop('pmt_policies_filtering_indication'),
                                                 }));

                    var current_policy_html = ich.policies(template_variables);
                    $('#pm').html(current_policy_html.filter("#policies"));
                    PM.activate('#policies-item');
                    PM.attach_lang_listener();
                    
                    if(PM.current_issue() == undefined) {
                        $('.policies-filtering-indication').show();
                        $('.policies-filtering').hide();
                    } else {
                        $('.policies-filtering-indication').hide();
                        $('.policies-filtering').show();
                    }
                    
                    _.each(ids, function(policyid) {
                               $('#input' + policyid).click(_.bind(PM.on_select_policy, PM, policyid));
                           });
                    
                    if(sectionid != undefined) {
                        PM.scroll_to($('#' + sectionid));
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
    PM.busy_cursor_on();
    PM.ajax_get(IMPACT.wsurl + '/evaluate-policy/{0}/{1}/{2}/{3}'.
                format(IMPACT.db, IMPACT.current_policy, IMPACT.question, id),
                function(data) {
                    PM.busy_cursor_off();
                    PM.on_evaluated_policy(data);
                },
                PM.on_error);
    return false;
};

PM.on_evaluated_policy = function(data) {
    IMPACT.db = data.db;
    console.log('db after evaluate: ' + IMPACT.db);
    PM.set_arguments_url(IMPACT.db);
};

PM.should_be_displayed = function(section, subset) {
    return section.id == IMPACT.question || subset == undefined || _.contains(subset, section.id);
};

PM.theory_outline_text = function(sections, urlfragment, subset) {
    var text = "<ul>";
    
    _.each(sections, function(section) {
               if(PM.should_be_displayed(section, subset)) {
                   text += '<li><a href="#/{0}/{1}">{2}</a></li>'.format(urlfragment, section.id, section.header.title);
                   text += PM.theory_outline_text(section.sections, urlfragment, subset);
               }
           });

    text += "</ul>";
    
    return text;
};

PM.schemes_text = function(language, schemes, lang) {
    var text = "<div>";
    
    _.each(schemes, function(scheme) {
               if(scheme.header.description && scheme.header.description[lang]) {
                   text += '{0}'.format(PM.markdown_to_html(scheme.header.description[lang]));
               }
               
               text += PM.scheme_content_text(language, scheme, lang);
           });
    
    text += "</div>";
    
    return text;
};

PM.capitalize = function(string) {
    return string.charAt(0).toUpperCase() + string.slice(1);
};

PM.scheme_content_text = function(language, scheme, lang) {
    var text = "";

    // TODO: this formatting code should be in a template
    text += '<div class="scheme-content" ><b>id:</b> {0}'.format(scheme.id);

    text += '<br><b>strict:</b> {0}'.format(scheme.strict);
    
    text += '<br><b>direction:</b> {0}'.format(scheme.pro ? "pro" : "con");

    text += '<br><b>conclusion:</b> {0}'.format(PM.format_sexpr(scheme.conclusion, language, lang));

    text += '<br><b>premises:</b><div class="rule-body"> <ul>';
     

    _.each(scheme.premises, function(premise) {
               if(premise.statement.atom[0] != "valid") {
                   text += "<li>{0}</li>".format(PM.format_sexpr(premise.statement.atom, language, lang));
               }
           });
    text += '</ul></div>';

    if(scheme.assumptions.length > 0) {
        text += '<b>assumptions:</b><div class="rule-body"> <ul>';

        _.each(scheme.assumptions, function(premise) {
                   text += "<li>{0}</li>".format(PM.format_sexpr(premise.statement.atom, language, lang));
               }); 

        text += '</ul></div>';
    }

    if(scheme.exceptions.length > 0) {
        text += '<b>exceptions:</b><div class="rule-body"> <ul>';

        _.each(scheme.exceptions, function(premise) {
                   text += "<li>{0}</li>".format(PM.format_sexpr(premise.statement.atom, language, lang));
               }); 
        
        text += '</ul></div>';
    }

    

    
    text += '</div><br/>'; // end of scheme-content stuff
    
    return text;
};

// if subset is defined then it contains the id of the policies to display
// if not defined all policies are displayed
PM.policies_text = function(language, sections, level, subset, on_policy, lang) {
    var text = "";
    
    _.each(sections, function(section) {
               if(PM.should_be_displayed(section, subset)) {
                   text += '<div id="{0}">'.format(section.id);
                   text += '<form action=""><h{0}>'.format(level + 1);
                   if(section.schemes.length > 0) {
                       on_policy(section.id);
                       text += '<input type="submit" value="{1}" id="input{0}" />'.format(section.id, $.i18n.prop('pmt_select'));
                   }
                   text += ' {1}</h{0}></form>'.format(level + 1, section.header.title);
                   text += '<p>{0}</p>'.format(PM.markdown_to_html(section.header.description[lang]));
                   text += PM.schemes_text(language, section.schemes, lang);
                   text += PM.policies_text(language, section.sections, level + 1, subset, on_policy, lang);
                   text += '</div>';
               }
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
// or a string.
// language_clj is the language translated to ClojureScript
PM.format_sexpr = function(sexpr, language_clj, lang) {
    var txt = catb.views.pmt.theory.format_statement(sexpr,
                                                     language_clj,
                                                     lang,
                                                     "positive");
    txt = txt.replace(/\?/g, '');
    txt = PM.capitalize(txt);

    return txt;
};

// Returns the header of the current policy
PM.get_policy_header = function(policy_id) {
    var global_policy = PM.policies.get(IMPACT.current_policy);
    var sections = global_policy.sections;
    for(var i = 0; i < sections.length; i++) {
        var subsection = sections[i].sections;
        for(var j = 0; j < subsection.length; j++) {
            if(subsection[j].id == policy_id) {
                return subsection[j].header;
            }
        }
    }
};

// Returns the all the policies ids of the current policy
PM.get_policies_ids = function() {
    var global_policy = PM.policies.get(IMPACT.current_policy);
    var sections = global_policy.sections;
    var policies = [];
    
    for(var i = 0; i < sections.length; i++) {
        var subsection = sections[i].sections;
        for(var j = 0; j < subsection.length; j++) {
            policies.push(subsection[j].id);
        }
    }

    return policies;
};
