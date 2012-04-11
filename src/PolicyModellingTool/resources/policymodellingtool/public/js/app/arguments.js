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
    
    // IMPACT.db = "copyright"; // "policymodellingtool-d139d8ba-60f2-4821-a168-0928926083ee";
    PM.append_agbrowser('#innerargumentbrowser');
};
