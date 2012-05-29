var CARNEADES = {
    lang : "en",
    carneadeswsurl : "/impactws"
};

var AGB = {
};

// this code is executed when the page is loaded
$(function() {
      AGB.init();
});

AGB.init = function() {
    var head = $('head');
    
    head.append('<script src="js/lib/jquery.address-1.4.js" type="text/javascript"></script>');
    head.append('<script src="js/lib/underscore-min.js" type="text/javascript"></script>');
    head.append('<script src="js/app/config.js" type="text/javascript"></script>');

    $.address.change(AGB.url_changed);
    
    if(AGB_CONFIG.in_uid_toolbox) {
        AGB.load_scripts();     
    } else {
        AGB.load_uid_styles();          
    }
    
    AGB.load_templates(); 
};

AGB.start = function() {
    
};

AGB.stop = function() {
    
};

AGB.load_scripts = function() {
    var head = $('head');
    _.each(['js/app/agb-utils.js',
            'js/app/login.js',
            'js/app/metadata.js',
            'js/app/argumentgraph.js',
            'js/app/argument.js',
            'js/app/statement.js',
            'js/app/map.js',
            'js/app/login.js',
            'js/app/markdown.js',
            'js/app/config.js',
            'js/lib/jquery.svg.js',
            'js/lib/ICanHaz.js',
            'js/lib/Markdown.Converter.js',
            'js/lib/Markdown.Sanitizer.js',
            'js/lib/Markdown.Editor.js',
            'js/lib/crypto.js'],
           function(name) {
               console.log('Loading script ' + name);
               head.append('<script src="' + name + '" type="text/javascript"></script>');
           });
};

AGB.syncget = function(url, callback) {
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

AGB.load_templates = function() {
    _.each([{name: 'menu', url: 'site/menu.html'},
            {name: 'metadata', url: 'site/metadata.html'},
            {name: 'argumentlink', url: 'site/argumentlink.html'},
            {name: 'statementlink', url: 'site/statementlink.html'},
            {name: 'premise', url: 'site/premise.html'}],
            function(template) {
                AGB.syncget(template.url,
                           function(content) {
                               ich.addPartial(template.name, content);
                           });
           });
    _.each(['argumentgraph',
            'argument',
            'statement',
            'login'],
           function(name) {
               AGB.syncget('site/{0}.html'.format(name),
                             function(content) {
                                 console.log('Loading template ' + name);
                                 ich.addTemplate(name, content);
                             });
          });
};

AGB.parse_url = function(urlstring)
{
    var url_regex = /\/([\w-:]+)(\/([\w-]+))?(\/([\w-:]+))?/;
    var result = url_regex.exec(urlstring);

    if(result != null) {
        var element = result[1];
        var db = result[3];
        var element_id = result[5];
    }
    return [element, db, element_id];
};

AGB.url_changed = function(url)
{
    if(url.value == "/") {
         return;
    }
    
    var parsed = AGB.parse_url(url.value);
    AGB.dispatch_url(parsed[0], parsed[1], parsed[2]);
    
};

AGB.dispatch_url = function(element, db, element_id)
{
    if(element == "argument") {
        AGB.display_argument(db, element_id);
    } else if(element == "statement") {
        AGB.display_statement(db, element_id);
    } else if(element == "argumentgraph") {
        AGB.display_argumentgraph(db);
    } else if(element == "map") {
        AGB.display_map(db);
    } else if(element == "login") {
        AGB.display_login();
    }
};

AGB.ajax_post = function(suburl, jsondata, username, password, callback) {
    $.ajax({url: CARNEADES.carneadeswsurl + '/' + suburl,
            type: 'POST',
            'beforeSend' : function(xhr) {
                var bytes = Crypto.charenc.Binary.stringToBytes(username + ":" + password);
                var base64 = Crypto.util.bytesToBase64(bytes);
                xhr.setRequestHeader("Authorization", "Basic " + base64);
            },
            dataType : 'json',
            data : JSON.stringify(jsondata),
            contentType: "application/json; charset=utf-8",
            success: callback,
            failure: function(error) { console.log('error: ' + error); }
        });
};

AGB.ajax_get = function(suburl, callback) {
    $.ajax({url: CARNEADES.carneadeswsurl + '/' + suburl,
            type: 'GET',
            success : callback,
            dataType : 'json'
        });
};

AGB.on_close = function() {
    $("#stage")[0].innerHTML = "<h1>Policy Modeling Tool</h1><div id='pm'></div>";
    $("#stage").addClass("toInit");
    $.address.change(PM.url_changed);
    ich.grabTemplates();
    $.address.path("/arguments");
    init();
    return false;
};

AGB.load_uid_styles = function() {
    var files = ['<link type="text/css" href="toolbox/css/impact-ui/jquery-ui-1.8.11.custom.css" rel="stylesheet" />',
                 '<link type="text/css" href="toolbox/css/impact-ui/impact-green.css" rel="stylesheet" />',
                 '<link rel="stylesheet" type="text/css" media="all" href="toolbox/css/impact-ui/plugins/jquery-ui-timepicker.css" />',
                 '<link href="toolbox/css/main.css" rel="stylesheet" type="text/css" />'
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
                   //                   "toolbox/js/main.js"
                  ];

    _.each(files, function(file) {
               $('head').append(file);
           });
    
    // http://stackoverflow.com/questions/6502943/resetting-document-ready-getscript
    _.each(scripts, function(script) {
               $.getScript(script);
           });
    
};
