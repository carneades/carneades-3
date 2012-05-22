// This object contains the global variables for the app
var IMPACT = {
    db: "",
    question: "Q12",
    lang: "en",
    impactws_url: "/impactws",
    argumentbrowser_url: "/argumentbrowser",
    simulation_url: "/policymodellingtool/PolicySimulation",
    embedded_agbrowser_history: {index: -1, history: []}  
};

// This object contains the functions and acts as a kind of namespace.
// We don't put variables in it since mixing data and functions with objects
// is too often a bad idea
var PM = {
   
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

    var url_regex = /\/([^ \/]+)(?:\/([^ \/]+))?/;
    var result = url_regex.exec(url.value);
    if(result != null) {
        var element = result[1];
        var section = result[2];
        
        PM.dispatch_url(element, section);
    }
};

PM.dispatch_url = function(element, section) {
    if(element == "introduction") {
        PM.display_introduction();
    } else if(element == "issues") {
        PM.display_issues();
    } else if(element == "facts") {
        PM.display_facts();
    } else if(element == "arguments") {
        PM.display_arguments();
    } else if(element == "policies") {
        PM.display_policies(section);
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
          'js/lib/ICanHaz.js',
          'js/lib/Markdown.Converter.js',
          'js/lib/Markdown.Sanitizer.js',
          'js/lib/Markdown.Editor.js',
          'js/lib/jquery.scrollTo-1.4.2-min.js',
          'js/lib/jquery.validate.js'],
        function(name) {
            console.log('Loading script ' + name);
            head.append('<script src="' + name + '" type="text/javascript"></script>');
        });
};

PM.load_templates = function() {
    PM.syncget('site/pmmenu.html',
                    function(content) {
                        ich.addPartial('pmmenu', content);
                    });
    _.each(['issues',
            'facts',
            'arguments',
            'policies',
            'introduction'],
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
