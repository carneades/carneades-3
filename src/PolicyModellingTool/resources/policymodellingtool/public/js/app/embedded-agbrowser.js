PM.append_agbrowser = function(id) {
    var url = IMPACT.argumentbrowser_url + '/#/argumentgraph/' + IMPACT.db;

    $(id).append('<div id="framemenu" class="navigation"><a class="first" id="previous" href="">Previous</a><a id="next" href=""">Next</a><a id="popup" href="">Popup</a></div>'
                 + '</div><iframe id="browserframe" width="95%" height="100%"></iframe>');

    var text;
    if(IMPACT.db == "") {
        text = "No facts have been entered. Enter some facts to view the arguments.";
        $('#browserframe').contents().find('body').html('<center>{0}</center>'.format(text));
    } else {
        $('#browserframe').contents().find('body').html('<center>{0}</center>'.format(text));
        $('#browserframe').load(PM.show_argumentgraph_inframe);
        $('#browserframe').attr('src', url);
    
        PM.push_history('argumentgraph', IMPACT.db);
        $('#previous').click(PM.prev_history);
        $('#next').click(PM.next_history);
    
        var argumentbrowser_url = "{0}/#/argumentgraph/{1}".format(IMPACT.argumentbrowser_url, IMPACT.db);
        $('#popup').attr('href', argumentbrowser_url);
        $('#popup').click(_.bind(PM.open_ag_browser, PM, url));
    } 
};


PM.open_ag_browser = function(url) {
    if(PM_CONFIG.debug) {
        // just open URL
        window.open(url);
        return false;
    }
    
    // open the argument browser in the UID toolbox
    $("#stage")[0].innerHTML = '<div id="browser" class=""></div>';
    $("#stage").addClass("toInit");
    $.address.change(url_changed);
    $.address.path("/argumentgraph/" + IMPACT.db);
    ich.grabTemplates();
    init();
    return false;
};

PM.add_map_to_browser_listener = function(domelement) {
    var g = $(domelement);
    if(domelement.id) {
        if(domelement.id[0] == 's') {
            g.click(function() {
                        var uuid = domelement.id.slice(2);
                        PM.push_history("statement", uuid);
                        PM.show_statement_inframe(uuid);
                    });
            g.mouseover(function () {
	                    $(this).css('cursor', 'hand');
                        });
        } else if(domelement.id[0] == 'a') {
            g.click(function() {
                        var uuid = domelement.id.slice(2);
                        PM.push_history("argument", uuid);
                        PM.show_argument_inframe(uuid);
                    });
            g.mouseover(function () {
	                    $(this).css('cursor', 'hand');
                        });
        }
    }
};

PM.show_statement_inframe = function(uuid) {
    var statement_html = document.getElementById('browserframe').contentWindow.statement_html;

    PM.ajax_get(IMPACT.impactws_url + '/statement-info/' + IMPACT.db + '/' + uuid,
                function(data) {
                    $('#browserframe').contents().find('#browser').html (statement_html(IMPACT.db, data, IMPACT.lang));
                    PM.add_links_listeners();
                });    
};

PM.show_argument_inframe = function(uuid) {
    var argument_html = document.getElementById('browserframe').contentWindow.argument_html;

    PM.ajax_get(IMPACT.impactws_url + '/argument-info/' + IMPACT.db + '/' + uuid,
                function(data) {
                    $('#browserframe').contents().find('#browser').html (argument_html(IMPACT.db, data));
                    PM.add_links_listeners();
                });
};

PM.show_argumentgraph_inframe = function() {
    var argumentgraph_html = document.getElementById('browserframe').contentWindow.argumentgraph_html;

    
    PM.ajax_get(IMPACT.impactws_url + '/argumentgraph-info/' + IMPACT.db,
                function(data) {
                    $('#browserframe').contents().find('#browser').html(argumentgraph_html(IMPACT.db, data));
                    PM.add_links_listeners();
                });

};

PM.show_map_inframe = function() {
    var traverse_map = document.getElementById('browserframe').contentWindow.traverse_map;
    
    $('#browserframe').contents().find('#browser').html('<div id="map"></div>');
    $('#browserframe').contents().find('#map').load(IMPACT.impactws_url + '/map/' + IMPACT.db,
                                                    function(svg, error) {
                                                        traverse_map(PM.add_map_to_browser_listener);
                                                        console.log('loaded');
                                                    });
};

PM.add_links_listeners = function() {
    var agb_parse_url = document.getElementById('browserframe').contentWindow.agb_parse_url;
    
    _.each($('#browserframe').contents().find('a'),
           function(a) {
               a = $(a);
               var href = a.attr('href');
               var parsed = agb_parse_url(href);
               var type = parsed[0];
               var uuid = parsed[2];

               // console.log('href =' + href);
               // console.log(parsed[0]);
               // console.log(parsed[2]);
               
               if(type == "argument") {
                   a.click(function() {
                               PM.push_history("argument", uuid);
                               PM.show_argument_inframe(uuid);
                               return false;
                           });
               } else if(type == "statement") {
                   a.click(function() {
                               PM.push_history("statement", uuid);
                               PM.show_statement_inframe(uuid);
                               return false;
                           });
               } else if (href.match(/map/)) {
                   a.click(function() {
                               PM.push_history("map", null);
                               PM.show_map_inframe();
                               return false;
                           });
               } else if(href.match(/argumentgraph/)) {
                   a.click(function() {
                               PM.push_history("argumentgraph", null);
                               PM.show_argumentgraph_inframe();
                               return false;
                           });
               } else {
                   // external link
                   a.click(function() {
                               window.open(href);
                               return false;
                           });
               }

           });    
};

PM.push_history = function(type, uuid) {
    console.log('push_history index: {0}'.format(IMPACT.embedded_agbrowser_history.index));
    console.log("Pushing {0} - {1}".format(type, uuid));
    IMPACT.embedded_agbrowser_history.index++;
    IMPACT.embedded_agbrowser_history.history = IMPACT.embedded_agbrowser_history.history.slice(0, IMPACT.embedded_agbrowser_history.index);
    IMPACT.embedded_agbrowser_history.history.push({type: type, uuid: uuid});

};

PM.show_element_inframe = function(type, uuid) {
        if(type == 'argumentgraph') {
            PM.show_argumentgraph_inframe();
        } else if(type == 'statement') {
            PM.show_statement_inframe(uuid);
        } else if(type == 'argument') {
            PM.show_argument_inframe(uuid);
        } else if(type == 'map') {
            PM.show_map_inframe();
        }  
};

PM.current_history = function() {
    return IMPACT.embedded_agbrowser_history.history[IMPACT.embedded_agbrowser_history.index];
};

PM.prev_history = function() {
    console.log('prev_history index: {0}'.format(IMPACT.embedded_agbrowser_history.index));
    if(IMPACT.embedded_agbrowser_history.index > 0) {
        IMPACT.embedded_agbrowser_history.index--;
        var current = PM.current_history();
        var type = current.type;
        var uuid = current.uuid;
        
        console.log('(prev) Type =' + type);
        PM.show_element_inframe(type, uuid); 
    }
    return false;
};

PM.next_history = function() {
    console.log('next_history index: {0}'.format(IMPACT.embedded_agbrowser_history.index));
    if(IMPACT.embedded_agbrowser_history.index + 1 < IMPACT.embedded_agbrowser_history.history.length) {
        IMPACT.embedded_agbrowser_history.index++;
        var current = PM.current_history();
        var type = current.type;
        var uuid = current.uuid;
        
        console.log('(next) Type =' + type);
        PM.show_element_inframe(type, uuid); 
    }
    return false;
};