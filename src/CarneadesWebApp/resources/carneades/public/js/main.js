// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

// goog.provide('carneades.policy-analysis.web.main');

// This object contains the global variables for the app
var IMPACT = {
    user: "root",
    password: "pw1", // TODO real auth
    db: undefined,
    question: "Q12",
    lang: "en",
    wsurl: "/carneades",
    argumentbrowser_url: "/argumentbrowser",
    simulation_url: "/carneades/policy-analysis/questions",
    current_policy: "copyright-policies",
    debate_db: "copyright",
    rootpath: null
};

// This object contains the functions and acts as a kind of namespace.
var PM = {
};

// argument browser
var AGB = {
};


// adds a format method to all string
// place holders are of the form {0}, {1} etc
String.prototype.format = function() {
  var args = arguments;
  return this.replace(/{(\d+)}/g, function(match, number) {
    return typeof args[number] != 'undefined'
      ? args[number]
      : match
    ;
  });
};

PM.syncget = function(url, callback) {
    $.ajax({url: url,
            type: 'GET',
            success : callback,
            error: function(jqXHR, textStatus) {
              console.log('[ERROR] AJAX '+ textStatus);
            },
            dataType : 'html',
            async: false
        });
};

PM.url_changed = function(url) {
    if(url.value == "/") {
         return;
    }

    var url_regex = /\/([^ \/]+)(?:\/([^ \/]+))?(?:\/([^ ?\/]+))?(?:\/([^ \/]+))?/;
    var result = url_regex.exec(url.value);
    if(result != null) {
        PM.dispatch_url(result);
    }
};

PM.dispatch_url = function(sections) {
    if(sections[1] == "issues") {
        PM.display_issues();
    } else if(sections[1] == "facts") {
        PM.dispatch_facts_url(sections[2]);
    } else if(sections[1] == "arguments") {
        PM.display_arguments(sections[3], sections[2], sections[4]); 
    } else if(sections[1] == "policies") {
        PM.display_policies(sections[2]);
    } else if(sections[1] == "schemes") {
        PM.display_schemes(sections[2]);
    } else if(sections[1] == "admin") {
        PM.display_admin();
    } else if(sections[1] == "introduction") {
        PM.display_introduction();
    } else if(sections[1] == "sct") {
        PM.dispatch_sct_url(sections[2]);
    } else if(sections[1] == "repl") {
        carneades.policy_analysis.web.repl.connect();
    } else if(sections[1] == "report") {
        carneades.policy_analysis.web.views.pmt.report.display();
    }
};

PM.dispatch_facts_url = function(section) {
    if(section == "modify") {
        carneades.policy_analysis.web.views.pmt.submitted_facts.display_facts();
    } else {
        PM.display_facts();
    }
};

PM.dispatch_sct_url = function(section) {
    if(section == "intro") {
        PM.display_sct_intro();
    } else if(section == "issues") {
        PM.display_sct_issues();
    } else if(section == "question") {
        PM.display_sct_question();
    } else if(section == "summary") {
        PM.display_sct_summary();
    } else if(section == "comparison") {
        PM.display_sct_comparison();
    }
};

// ImpactToolbox = {
   
// };

PM.in_uid_toolbox = function() {
    return window.ImpactToolbox != undefined;
};

// This code is executed when the page is loaded
$(function() {
      var head = $('head');

      if(!PM.in_uid_toolbox()) {
          PM.init();
      }
  });


PM.init = function(toolboxState) {
    var head = $('head');

    // TODO suppress rootpath local variables (in function)
    // after the demo
    var rootpath = undefined;

    // bootstrap
    if(toolboxState == undefined) {
        head.append('<script src="js/lib/underscore-min.js" type="text/javascript"></script>');
        head.append('<link rel="stylesheet" href="js/lib/select2.css" type="text/css" />');
    } else {
        rootpath = toolboxState.pmt.path;
        head.append('<script src="' + rootpath +
                    '/js/lib/underscore-min.js" type="text/javascript"></script>');
        head.append('<link rel="stylesheet" href="' + rootpath +
                    '/js/lib/select2.css" type="text/css" />');
    }

    // adds some methods to underscore JS
    _.mixin({isNil: function(o) {
                 return _.isNull(o) || _.isUndefined(o);
             }
            });

    IMPACT.rootpath = rootpath;
    
    if(PM.in_uid_toolbox()) {
        PM.load_scripts(rootpath, true, _.bind(PM.post_load_uid, PM, toolboxState));
    } else {
        PM.load_scripts(rootpath, false, PM.post_load);
    }

 
};

// attachs a listener to the 'select' language
// and sets the value of the 'select' to the one of IMPACT.lang
PM.attach_lang_listener = function() {
    $('#pm-select-lang').val(IMPACT.lang);
    $('#pm-select-lang').change(function(event) {
        PM.languageChanged(event.target.value);
    });
}

PM.start = function(toolboxState) {
    PM.set_introduction_url();

    return toolboxState;
};

PM.stop = function(toolboxState) {
    return toolboxState;
};

PM.languageChanged = function(lang) {
    // reloads page

    IMPACT.lang = lang;
    PM.init_i18n(function() {
        PM.ajax_post(IMPACT.simulation_url, {lang: IMPACT.lang},
                     function() {
                         if(!_.isNil($.address.parameter('lang'))) {
                             // if the page has a lang parameter we change it
                             $.address.parameter('lang', IMPACT.lang);
                         } else {
                             // otherwise we just reload the page
                             // since the IMPACT.lang variable has change
                             // the language display to the user will also
                             // change
                             $.address.update();
                         }
                     },
                     IMPACT.user,
                     IMPACT.password,
                     PM.on_error);
    });
};

PM.canBeStopped = function() {
    return false;
};

PM.add_address_listener = function() {
    $.address.change(PM.url_changed);
};

PM.post_load_uid = function(toolboxState) {
    PM.load_templates(toolboxState);
    PM.add_address_listener();
    PM.load_app_styles(toolboxState.pmt.path);
    
    PM.common_post_load();
    
    // Forces update.
    $.address.update();
};

PM.post_load = function() {
    PM.load_carneades_styles();
    PM.load_app_styles(null);
    PM.load_templates();
    PM.add_address_listener();
    
    PM.common_post_load();
    
    // Forces update.
    $.address.update();

};

PM.init_i18n = function(callbackfn) {
    var site_path = PM.in_uid_toolbox() ? '/carneades/policy-analysis/site/' : 'site/';

    jQuery.i18n.properties(
        {name:'Messages',
         path: site_path,
         mode:'both',
         language: IMPACT.lang, 
         callback: function() {
                 if(_.isFunction(callbackfn)) {
                     callbackfn();
                 }
         }
        });
};

PM.common_post_load = function() {
    $.ajaxSetup({beforeSend: PM.simple_auth});
    
    PM.schemes = new PM.Schemes;
    PM.schemes.fetch();
  
    PM.current_theory = new PM.Theory({id: 'walton'});
    PM.current_theory.fetch();
    
    PM.arguments = new PM.Arguments;
    PM.statements = new PM.Statements;
    
    PM.policies = new PM.Policies;
    PM.policies.fetch();
    
    PM.init_i18n();
    
    PM.debate_arguments = new PM.Arguments([], {db: IMPACT.debate_db});
    PM.debate_statements = new PM.Statements([], {db: IMPACT.debate_db});
    PM.debate_metadata = new PM.MetadataList([], {db: IMPACT.debate_db});
    
    PM.debate_arguments.fetch();
    PM.debate_statements.fetch();
    PM.debate_metadata.fetch();
    
    PM.debate_info = new PM.AgInfo({db: IMPACT.debate_db});
    PM.debate_info.fetch();
    
    PM.sct = new PM.Sct({db: IMPACT.debate_db,
                         lang: IMPACT.lang,
                         arguments: PM.debate_arguments,
                         statements: PM.debate_statements,
                         metadata: PM.debate_metadata});
};

// http://www.lockencreations.com/2011/07/02/cant-debug-imported-js-files-when-using-jquery-getscript/
// Replace the normal jQuery getScript function with one that supports
// debugging and which references the script files as external resources
// rather than inline.
PM.get_script = function(url, callback) {
    var head = document.getElementsByTagName("head")[0];
    var script = document.createElement("script");
    script.src = url;

    // Handle Script loading
    var done = false;

    // Attach handlers for all browsers
    script.onload = script.onreadystatechange = function(){
	if (!done && (!this.readyState || this.readyState == "loaded" || this.readyState == "complete")) {
	    done = true;
	    if (callback){
		callback();
	    }
	    // Handle memory leak in IE
	    script.onload = script.onreadystatechange = null;
	}
    };

    console.log('Loading ' + url);
    head.appendChild(script);

    // We handle everything using the script element injection
    return undefined;
};

PM.load_scripts_helper = function(rootpath, scripts, callback) {
    if(scripts.length > 1) {
        var script = scripts.pop();
        if(rootpath == undefined) {
            PM.get_script(script, _.bind(PM.load_scripts_helper, PM, rootpath, scripts, callback));    
        } else {
            PM.get_script(rootpath + '/' + script, _.bind(PM.load_scripts_helper, PM, rootpath, scripts, callback));    
        }
    } else if(scripts.length == 1) {
        var script = scripts.pop();
        if(rootpath == undefined) {
            PM.get_script(script, callback);
        } else {
            PM.get_script(rootpath + '/' + script, callback);
        }
    }
};

PM.load_scripts = function(rootpath, is_in_toolbox, callback) {
    var head = $('head');
    var scripts = [ // JS libraries must be listed here, JS carneades libraries are listed in project.clj
                    // and compiled by the Google Closure Compiler
                   'js/lib/ICanHaz.js',
                   'js/lib/Markdown.Converter.js',
                   'js/lib/Markdown.Editor.js',
                   'js/lib/Markdown.Sanitizer.js',
                   'js/lib/backbone.js',
                   'js/lib/backbone.memento.min.js',
                   'js/lib/crypto.js',
                   'js/lib/html5slider.js',
                   'js/lib/jquery.i18n.properties-min-1.0.9.js',
                   'js/lib/jquery.scrollTo-1.4.2-min.js',
                   'js/lib/jquery.svg.js',
                   'js/lib/jquery.validate.js',
                   'js/lib/markitup/jquery.markitup.js',
                   'js/lib/markitup/sets/markdown/set.js',
                   'js/lib/parallel.js',
                   'js/lib/select2.js',
                   'js/lib/sprintf-0.7-beta1.js',
                   'js/compiled-app.js' 
                  ];
    
    if(!is_in_toolbox) {
      scripts = scripts.concat('js/lib/jquery.address-1.4.js', 
                               'js/lib/jquery-ui-1.8.23.custom.min.js');
    }
    
    scripts.reverse();
    PM.load_scripts_helper(rootpath, scripts, callback);
};

// loads templates *synchronously*
PM.load_templates = function(toolboxState) {
    // loads partial templates
    _.each([{name: 'menu', url: 'site/menu.html'},
            {name: 'pmmenu', url: 'site/pmmenu.html'},
            {name: 'metadata', url: 'site/metadata.html'},
            {name: 'argumentlink', url: 'site/argumentlink.html'},
            {name: 'statementlink', url: 'site/statementlink.html'},
            {name: 'premise', url: 'site/premise.html'}],
           function(template) {
               var url = toolboxState == undefined ?
                   template.url : toolboxState.pmt.path + '/' + template.url;
               PM.syncget(url,
                          function(content) {
                              ich.addPartial(template.name, content);
                          });
           });

    // loads templates
    _.each(['admin',
            'argumentgraph',
            'argument',
            'argumentlink',
            'arguments',
            'facts',
            'introduction',
            'issues',
            'login',
            'menu',
            'metadata',
            'pmmenu',
            'policies',
            'premise',
            'statement',
            'statementlink',
            'ageditormenuon',
            'statementeditormenu',
            'argumenteditormenu',
            'statementeditor',
            'argumenteditor',
            'premisescandidates',
            'exceptionscandidates',
            'theory',
            'metadataeditor2',
            'addmore',
            'premisecandidate',
            'conclusioncandidate',
            'schemecandidate',
            'metadataelementeditor',
            'button',
            'button2',
            'sct-intro',
            'sct-issues',
            'sct-claim',
            'sct-argument',
            'sct_summary',
            'sct_claim_editor',
            'sct_comparison',
            'vote',
            'after_vote',
            'vote_results',
            'report',
            'submitted_facts',
            'ask_modify_facts'
           ],
           function(name) {
               var url = toolboxState == undefined ?
                   'site/{0}.html'.format(name) :
                   toolboxState.pmt.path + '/' + 'site/{0}.html'.format(name);
               PM.syncget(url,
                          function(content) {
                              console.log('Loading template ' + name);
                              ich.addTemplate(name, content);
                          });
          });
};

// Loads some generic styles when the app is used
// outside of the UID toolbox
PM.load_carneades_styles = function() {
    PM.load_style(undefined, 'impact-ui/jquery-ui-1.8.11.custom.css', 'toolbox/css');
    PM.load_style(undefined, 'impact-ui/impact-green.css', 'toolbox/css');
    PM.load_style(undefined, 'main.css', 'toolbox/css');
    PM.load_style(undefined, 'carneades/style.css', 'toolbox/css');
    PM.load_style(undefined, 'local-app.css');
};

// Loads some specific styles to the app
// They are loaded even when used inside the UID toolbox
PM.load_app_styles = function(rootpath) {
    PM.load_style(rootpath, 'app.css');
    PM.load_style(rootpath, 'style.css', 'js/lib/markitup/skins/markitup');
    PM.load_style(rootpath, 'style.css', 'js/lib/markitup/sets/markdown');
};

// Loads a CSS style
// @cssdir is optional and defaults to 'css'
PM.load_style = function(rootpath, style, cssdir) {
    if(_.isNil(cssdir)) {
        cssdir = "css";
    }

    if(_.isNil(rootpath)) {
        $('head').append('<link href="{0}/{1}" rel="stylesheet" type="text/css" />'.format(cssdir, style));
        // $('head').append('<link href="'+ cssdir + '/' + style + '" rel="stylesheet" type="text/css" />');
    } else {
        $('head').append('<link href="' + rootpath
                         + '/{0}/{1}" rel="stylesheet" type="text/css" />'.format(cssdir, style));
    }
};

// Called when an AJAX error occurs
PM.on_error = function(textstatus) {
    PM.busy_cursor_off();
    $('#pm').prepend('<ul class="warning pm-warning" ><li class="notification">Error: {0}</li></ul>'.format(textstatus));
    setTimeout(function() {
                   $('#pm .warning').remove();
               }, 3000);
    PM.scroll_to_top();
};

PM.notify = function(text) {
     $('#pm').prepend('<ul class="thankyou pm-thankyou"><li class="notification">{0}</li></ul>'.format(text));
    setTimeout(function() {
                   $('#pm .thankyou').remove();
               }, 3000);
    PM.scroll_to_top();
};

// Called when an AJAX error occurs for backbone
PM.on_model_error = function(collection, response) {
    PM.busy_cursor_off();
    $('#pm').prepend('<div style="background-color:  #FFCC33" class="error">Error: {0}</div>'.format(response.statusText));
    setTimeout(function() {
                   $('#pm .error').remove();
               }, 3000);
};


PM.scroll_to_bottom = function() {
    if(PM.in_uid_toolbox()) {
        $("#stage").animate({ scrollTop: 2000 }, "fast");
     } else {
         $('body').animate({ scrollTop: 2000 }, "fast");
     }
}

PM.scroll_to_top = function() {
    if(PM.in_uid_toolbox()) {
        $("#stage").animate({ scrollTop: 0 }, "fast");
     } else {
         $('body').animate({ scrollTop: 0 }, "fast");
     }
}

PM.scroll_to = function(selector) {
    if(PM.in_uid_toolbox()) {
        $("#stage").scrollTo(selector);
     } else {
         $.scrollTo(selector);
     }
}

PM.busy_cursor_on = function() {
    $("*").css("cursor", "progress");
};

PM.busy_cursor_off = function() {
    $("*").css("cursor", "default");
};
