var CARNEADES = {
    lang : "en",
    carneadeswsurl : "/impactws"
};

// this code is executed when the page is loaded
$(function() {
      $.address.change(url_changed);
});

function url_changed(url)
{
    var url_regex = /\/([\w-:]+)(\/([\w-]+))?(\/([\w-:]+))?/;
    if(url.value == "/") {
         return;
    }

    var result = url_regex.exec(url.value);
    if(result != null) {
        var element = result[1];
        var db = result[3];
        var element_id = result[5];

        dispatch_url(element, db, element_id);
    }
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
