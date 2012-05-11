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
$(function() {
     $.address.change(PM.url_changed);
      
      initPM();
  });

function initPM() {
    PM.display_introduction();
    if(PM_CONFIG.debug) {
        PM.load_uid_styles();
    }
}

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
