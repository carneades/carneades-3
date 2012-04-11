var CARNEADES = {
    lang : "en",
    carneadeswsurl : "/impactws"
};

// this code is executed when the page is loaded
$(function() {
      $.address.change(url_changed);
//    display_argumentgraph("policymodellingtool-d139d8ba-60f2-4821-a168-0928926083ee");

});

function agb_parse_url(urlstring)
{
    var url_regex = /\/([\w-:]+)(\/([\w-]+))?(\/([\w-:]+))?/;
    var result = url_regex.exec(urlstring);

    if(result != null) {
        var element = result[1];
        var db = result[3];
        var element_id = result[5];
    }
    return [element, db, element_id];
}

function url_changed(url)
{
    if(url.value == "/") {
         return;
    }
    
    var parsed = agb_parse_url(url.value);
    dispatch_url(parsed[0], parsed[1], parsed[2]);
    
}

function dispatch_url(element, db, element_id)
{
    if(element == "argument") {
        display_argument(db, element_id);
    } else if(element == "statement") {
        display_statement(db, element_id);
    } else if(element == "argumentgraph") {
        display_argumentgraph(db);
    } else if(element == "map") {
        display_map(db);
    } else if(element == "login") {
        display_login();
    }
}

function ajax_post(suburl, jsondata, username, password, callback) {
    $.ajax({url: CARNEADESWS.carneadeswsurl + '/' + suburl,
            type: 'POST',
            'beforeSend' : function(xhr) {
                var bytes = Crypto.charenc.Binary.stringToBytes(username + ":" + password);
                var base64 = Crypto.util.bytesToBase64(bytes);
                xhr.setRequestHeader("Authorization", "Basic " + base64);
            },
            dataType : 'json',
            data : JSON.stringify(jsondata),
            contentType: "application/json; charset=utf-8",
            success: callback
        });
}

function ajax_get(suburl, callback) {
    $.ajax({url: CARNEADES.carneadeswsurl + '/' + suburl,
            type: 'GET',
            success : callback,
            dataType : 'json'
        });
}

function on_close() {
    $("#stage")[0].innerHTML = "<h1>Policy Modeling Tool</h1><div id='pm'></div>";
    $("#stage").addClass("toInit");
    $.address.change(PM.url_changed);
    ich.grabTemplates();
    $.address.path("/arguments");
    init();
    return false;
}