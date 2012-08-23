// This object contains the global variables for the app
var IMPACT = {
    user: "root",
    password: "pw1", // TODO real auth
    db: "",
    question: "Q12",
    lang: "en",
    wsurl: "/impactws",
    argumentbrowser_url: "/argumentbrowser",
    simulation_url: "/policymodellingtool/PolicySimulation",
    current_policy: "copyright-policies",
    rootpath: null
};

// This object contains the functions and acts as a kind of namespace.
// We don't put variables in it since mixing data and functions with objects
// is too often a bad idea
var PM = {
};

// argument browser
var AGB = {
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

    var url_regex = /\/([^ \/]+)(?:\/([^ \/]+))?(?:\/([^ \/]+))?(?:\/([^ \/]+))?/;
    var result = url_regex.exec(url.value);
    if(result != null) {
        PM.dispatch_url(result);
    }
};

PM.dispatch_url = function(sections) {
    if(sections[1] == "issues") {
        PM.display_issues();
    } else if(sections[1] == "facts") {
        PM.display_facts();
    } else if(sections[1] == "arguments") {
        PM.display_arguments(sections[3], sections[2], sections[4]); 
    } else if(sections[1] == "policies") {
        PM.display_policies(sections[2]);
    } else if(sections[1] == "admin") {
        PM.display_admin();
    } else if(sections[1] == "introduction") {
        PM.display_introduction();
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

PM.start = function(toolboxState) {
    PM.set_introduction_url();

    return toolboxState;
};

PM.stop = function(toolboxState) {
    return toolboxState;
};

PM.languageChanged = function(lang) {

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
    $.ajaxSetup({beforeSend: PM.simple_auth});
    
    // Forces update.
    $.address.update();
};

PM.post_load = function() {
    PM.load_carneades_styles();
    PM.load_app_styles(null);
    PM.load_templates();
    PM.add_address_listener();
    $.ajaxSetup({beforeSend: PM.simple_auth});
    
    // Forces update.
    $.address.update();

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
    var scripts = ['js/app/introduction.js',
                   'js/app/menu.js',
                   'js/app/issues.js',
                   'js/app/facts.js',
                   'js/app/arguments.js',
                   'js/app/policies.js',
                   'js/app/markdown.js',
                   'js/app/metadata.js',
                   'js/app/admin.js',
                   'js/app/ajax.js',
                   'js/app/questions.js',
                   'js/app/agb/agb-utils.js',
                   'js/app/agb/login.js',
                   'js/app/agb/metadata.js',
                   'js/app/agb/argumentgraph.js',
                   'js/app/agb/argument.js',
                   'js/app/agb/statement.js',
                   'js/app/agb/map.js',
                   'js/app/agb/markdown.js',
                   'js/app/agb/statement-editor.js',
                   'js/app/agb/argument-editor.js',
                   'js/app/agb/metadata-editor.js',
                   'js/lib/ICanHaz.js',
                   'js/lib/Markdown.Converter.js',
                   'js/lib/Markdown.Sanitizer.js',
                   'js/lib/Markdown.Editor.js',
                   'js/lib/jquery.scrollTo-1.4.2-min.js',
                   'js/lib/jquery.validate.js',
                   'js/lib/jquery.svg.js',
                   'js/lib/crypto.js',
                   'js/lib/select2.js',
                   'js/app/utils.js',
                   'js/lib/parallel.js',
                   'js/lib/markitup/sets/markdown/set.js',
                   'js/lib/markitup/jquery.markitup.js',
                   'js/lib/backbone.js',
                   'js/lib/backbone.memento.min.js',
                   'js/app/models/metadata.js',
                   'js/app/models/statement.js',
                   'js/app/models/scheme.js',
                   'js/app/models/argument.js',
                   'js/app/models/scheme-candidate.js',
                   'js/app/models/argument-candidate.js',
                   'js/app/models/premise-candidate.js',
                   'js/app/models/conclusion-candidate.js',
                   'js/app/collections/statements.js',
                   'js/app/collections/schemes.js',
                   'js/app/collections/metadata-list.js',
                   'js/app/collections/arguments.js',
                   'js/app/collections/premises-candidates.js',
                   'js/app/views/argument-editor.js',
                   'js/app/views/premise-candidate.js',
                   'js/app/views/scheme-candidate.js',
                   'js/app/views/argument-editor-free.js',
                   'js/app/views/conclusion-candidate.js'];
    
    if(!is_in_toolbox) {
      scripts = scripts.concat('js/lib/jquery.address-1.4.js', 
                               'js/lib/jquery-ui-1.8.21.custom.min.js');
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
            {name: 'premise', url: 'site/premise.html'},
            {name: 'premiseeditorpartial', url: 'site/premiseeditorpartial.html'},
            {name: 'ageditormenu', url: 'site/ag-editor-menu.html'}],
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
            'argumenteditor2',
            'argumenteditor2free',
            'metadataeditor',
            'premiseeditor',
            'premiseeditorwithoutscheme',
            'addmore',
            'newstatementlink',
            'premisecandidate',
            'conclusioncandidate',
            'schemecandidate',
            'button'],
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
    PM.load_style(undefined, 'green-theme.css');
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
    } else {
        $('head').append('<link href="' + rootpath
                         + '/{0}/{1}" rel="stylesheet" type="text/css" />'.format(cssdir, style));
    }
};

// Called when an AJAX error occurs
PM.on_error = function(textstatus) {
    $('#pm').prepend('<div style="background-color:  #FFCC33" class="error">Error: {0}</div>'.format(textstatus));
    setTimeout(function() {
                   $('#pm .error').remove();
               }, 3000);
};

// Called when an AJAX error occurs
PM.on_model_error = function(collection, response) {
    $('#pm').prepend('<div style="background-color:  #FFCC33" class="error">Error: {0}</div>'.format(response.statusText));
    setTimeout(function() {
                   $('#pm .error').remove();
               }, 3000);
};