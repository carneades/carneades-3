// This object contains the global variables for the app
var IMPACT = {
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
    if(sections[1] == "introduction") {
        PM.display_introduction();
    } else if(sections[1] == "issues") {
        PM.display_issues();
    } else if(sections[1] == "facts") {
        PM.display_facts();
    } else if(sections[1] == "arguments") {
        PM.display_arguments(sections[3], sections[2], sections[4]);
    } else if(sections[1] == "policies") {
        PM.display_policies(sections[2]);
    }
};

// This code is executed when the page is loaded
// TODO: to disable when inside the uid toolbox??
$(function() {
      PM.init();
  });


PM.init = function() {
    var head = $('head');
    
    head.append('<script src="js/lib/jquery.address-1.4.js" type="text/javascript"></script>');
    head.append('<script src="js/lib/underscore-min.js" type="text/javascript"></script>');
    head.append('<script src="js/app/config.js" type="text/javascript"></script>');

    // adds a isNil method to underscore JS
    _.mixin({isNil : function(o) {
                 return _.isNull(o) || _.isUndefined(o);
             }
            });
    
    $.address.change(PM.url_changed);
    
    if(PM_CONFIG.in_uid_toolbox) {
        PM.load_scripts();        
    } else {
        PM.load_uid_styles();        
    }

    PM.load_templates();
    PM.display_introduction();
};

PM.start = function() {
    
};

PM.stop = function() {
    
};

PM.language_changed = function(lang) {
    
};

PM.load_scripts = function() {
    var head = $('head');
  _.each(['js/app/utils.js',
          'js/app/introduction.js',
          'js/app/menu.js',
          'js/app/issues.js',
          'js/app/facts.js',
          'js/app/arguments.js',
          'js/app/policies.js',
          'js/app/markdown.js',
          'js/app/metadata.js',
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
          'js/lib/ICanHaz.js',
          'js/lib/Markdown.Converter.js',
          'js/lib/Markdown.Sanitizer.js',
          'js/lib/Markdown.Editor.js',
          'js/lib/jquery.scrollTo-1.4.2-min.js',
          'js/lib/jquery.validate.js',
          'js/lib/jquery.svg.js'],
        function(name) {
            console.log('Loading script ' + name);
            head.append('<script src="' + name + '" type="text/javascript"></script>');
        });
};

PM.load_templates = function() {
    _.each([{name: 'menu', url: 'site/menu.html'},
            {name: 'pmmenu', url: 'site/pmmenu.html'},
            {name: 'metadata', url: 'site/metadata.html'},
            {name: 'argumentlink', url: 'site/argumentlink.html'},
            {name: 'statementlink', url: 'site/statementlink.html'},
            {name: 'premise', url: 'site/premise.html'}],
           function(template) {
               PM.syncget(template.url,
                           function(content) {
                               ich.addPartial(template.name, content);
                           });
           });
    _.each(['argumentgraph',
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
            'statementlink'],
           function(name) {
               PM.syncget('site/{0}.html'.format(name),
                             function(content) {
                                 console.log('Loading template ' + name);
                                 ich.addTemplate(name, content);
                             });
          });
};

PM.load_uid_styles = function() {
    var files = ['<link type="text/css" href="toolbox/css/impact-ui/jquery-ui-1.8.11.custom.css" rel="stylesheet" />',
                 '<link type="text/css" href="toolbox/css/impact-ui/impact-green.css" rel="stylesheet" />',
                 '<link rel="stylesheet" type="text/css" media="all" href="toolbox/css/impact-ui/plugins/jquery-ui-timepicker.css" />',
                 '<link href="toolbox/css/main.css" rel="stylesheet" type="text/css" />',
                 '<link rel="stylesheet" href="toolbox/css/policymodelling/style.css" type="text/css" />'
                ];
    
    var scripts = ["toolbox/js/impact-ui/jquery-ui-1.8.11.custom.min.js",
                   "toolbox/js/impact-ui/impact-init.js",
                   "toolbox/js/impact-ui/jquery.jscrollpane.min.js",
                   "toolbox/js/impact-ui/jquery.mousewheel.js",
                   "toolbox/js/impact-ui/jquery.mwheelIntent.js",
                   "toolbox/js/impact-ui/ui.checkbox.js",
                   "toolbox/js/impact-ui/jquery.selectbox-0.5.js",
                   "toolbox/js/impact-ui/jquery.busy.min.js",
                   "toolbox/js/impact-ui/jquery.ui.timepicker.js"
                   // "toolbox/js/main.js"
                  ];

    _.each(files, function(file) {
               $('head').append(file);
           });
    
    // http://stackoverflow.com/questions/6502943/resetting-document-ready-getscript
    _.each(scripts, function(script) {
               $.getScript(script);
           });
    
};
