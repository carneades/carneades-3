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
    embedded_agbrowser_history: {index: -1, history: []},
    current_policy: "copyright-policies" 
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

// This code is executed when the page is loaded
$(function() {
      var head = $('head');
      head.append('<script src="js/app/config.js" type="text/javascript"></script>');
      
      if(!PM_CONFIG.in_uid_toolbox) {
          PM.init();
      }
  });


PM.init = function() {
    var head = $('head');
    head.append('<script src="js/lib/underscore-min.js" type="text/javascript"></script>');
    head.append('<script src="js/app/config.js" type="text/javascript"></script>');
    head.append('<link rel="stylesheet" href="js/lib/select2.css" type="text/css" />');
    
    // adds some methods to underscore JS
    _.mixin({isNil: function(o) {
                 return _.isNull(o) || _.isUndefined(o);
             }
            });
    
    if(PM_CONFIG.in_uid_toolbox) {
        PM.load_scripts(PM.post_load_uid);
    } else {
        PM.load_scripts(PM.post_load);        
    }

};

PM.start = function() {
    PM.set_introduction_url();
};

PM.stop = function() {
    
};

PM.languageChanged = function(lang) {
    
};

PM.canBeStopped = function() {
    return false;
};

PM.add_address_listener = function() {
    $.address.change(PM.url_changed);
};

PM.post_load_uid = function() {
    PM.load_templates();
    PM.add_address_listener();
    $('head').append('<link rel="stylesheet" href="toolbox/css/policymodelling/style.css" type="text/css" />');

    // Forces update.
    $.address.update();
};

PM.post_load = function() {
//    PM.load_uid_styles();
    PM.load_templates();
    PM.add_address_listener();
    
    $('head').append('<link rel="stylesheet" href="toolbox/css/policymodelling/style.css" type="text/css" />');

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

PM.load_scripts_helper = function(scripts, callback) {
    if(scripts.length > 1) {
        var script = scripts.pop();
        PM.get_script(script, _.bind(PM.load_scripts_helper, PM, scripts, callback));
    } else if(scripts.length == 1) {
        var script = scripts.pop();
        PM.get_script(script, callback);
    }
};

PM.load_scripts = function(callback) {
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
                   'js/app/embedded-agbrowser.js',
                   'js/app/ajax.js',
                   'js/app/questions.js',
                   'js/app/agb/agb-utils.js',
                   'js/app/agb/login.js',
                   'js/app/agb/metadata.js',
                   'js/app/agb/argumentgraph.js',
                   'js/app/agb/argument.js',
                   'js/app/agb/statement.js',
                   'js/app/agb/map.js',
                   'js/app/agb/login.js',
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
                   'js/lib/jquery-ui-1.8.21.custom.min.js',
                   'js/lib/crypto.js',
                   'js/lib/select2.js',
                   'js/lib/jquery.address-1.4.js',
                   'js/app/utils.js'];
    
    scripts.reverse();
    PM.load_scripts_helper(scripts, callback);
};

// loads templates *synchronously*
PM.load_templates = function() {
    _.each([{name: 'menu', url: 'site/menu.html'},
            {name: 'pmmenu', url: 'site/pmmenu.html'},
            {name: 'metadata', url: 'site/metadata.html'},
            {name: 'argumentlink', url: 'site/argumentlink.html'},
            {name: 'statementlink', url: 'site/statementlink.html'},
            {name: 'premise', url: 'site/premise.html'},
            {name: 'premiseeditorpartial', url: 'site/premiseeditorpartial.html'},
            {name: 'ageditormenu', url: 'site/ag-editor-menu.html'}],
           function(template) {
               PM.syncget(template.url,
                           function(content) {
                               ich.addPartial(template.name, content);
                           });
           });
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
            'statementeditor',
            'argumenteditor',
            'metadataeditor',
            'premiseeditor'],
           function(name) {
               PM.syncget('site/{0}.html'.format(name),
                             function(content) {
                                 console.log('Loading template ' + name);
                                 ich.addTemplate(name, content);
                             });
          });
};

PM.load_uid_styles = function(callback) {
    var files = ['<link type="text/css" href="toolbox/css/impact-ui/jquery-ui-1.8.11.custom.css" rel="stylesheet" />',
                 '<link type="text/css" href="toolbox/css/impact-ui/impact-green.css" rel="stylesheet" />',
                 '<link rel="stylesheet" type="text/css" media="all" href="toolbox/css/impact-ui/plugins/jquery-ui-timepicker.css" />',
                 '<link href="toolbox/css/main.css" rel="stylesheet" type="text/css" />'];
    
    var scripts = ["toolbox/js/impact-ui/jquery-ui-1.8.11.custom.min.js",
                   "toolbox/js/impact-ui/jquery.jscrollpane.min.js",
                   "toolbox/js/impact-ui/jquery.mousewheel.js",
                   "toolbox/js/impact-ui/jquery.mwheelIntent.js",
                   "toolbox/js/impact-ui/ui.checkbox.js",
                   "toolbox/js/impact-ui/jquery.selectbox-0.5.js",
                   "toolbox/js/impact-ui/jquery.busy.min.js",
                   "toolbox/js/impact-ui/jquery.ui.timepicker.js",
                   "toolbox/js/impact-ui/impact-init.js"
                   // "toolbox/js/main.js"
                  ];

    _.each(files, function(file) {
               $('head').append(file);
           });
    
    // http://stackoverflow.com/questions/6502943/resetting-document-ready-getscript
    scripts.reverse();
    PM.load_scripts_helper(scripts, callback);
    
};

PM.on_error = function(textstatus) {
    $('#pm').prepend('<div style="background-color:  #FFCC33" class="error">Error: {0}</div>'.format(textstatus));
    setTimeout(function() {
                   $('#pm .error').remove();
               }, 3000); 
};