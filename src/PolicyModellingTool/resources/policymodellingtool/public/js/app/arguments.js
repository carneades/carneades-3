PM.arguments_url = function() {
    return 'arguments';    
};

PM.set_arguments_url = function() {
    $.address.value(PM.arguments_url());  
};

PM.display_arguments = function() {
    if(IMPACT.db == "") {
        console.log('No db. Please enter some facts first');
    }
    var arguments_html = ich.arguments();
    $('#pm').html(arguments_html.filter("#arguments"));
    PM.activate('#arguments-item');
    
    // PROBLEM Jquery address iframe
    // TODO: link in iframe    http://stackoverflow.com/questions/740816/open-link-in-iframe
    // https://github.com/asual/jquery-address/issues/106
    // http://stackoverflow.com/questions/821359/reload-an-iframe-without-adding-to-the-history
    // 
    var argumentbrowser_url = "{0}/#/argumentgraph/{1}".format(IMPACT.argumentbrowser_url, IMPACT.db);
    $('#argumentlink').attr('href', argumentbrowser_url);
    // $('#argumentlink').click(_.bind(PM.on_load_arguments, PM, argumentbrowser_url));
    // $('#innerargumentbrowser').append('<iframe name="agbrowser" src="{0}" />'.format(IMPACT.argumentbrowser_url + '/#/argumentgraph/' + db));

};

// PM.on_load_arguments = function(dburl) {
//     PM.ajax_get(dburl, function(data) {
//                     $('#innerargumentbrowser').html(data);
//                 });
//     return false;  
// };