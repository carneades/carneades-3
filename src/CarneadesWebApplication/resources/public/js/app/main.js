var CARNEADES = {
    lang : "en",
    carneadeswsurl : ""
};

// this code is executed when the page is loaded
$(function() {
      $('#connect').click(on_connect);

      $.address.change(url_changed);
});

function url_changed(url)
{
    var parse_url = /\/([\w-:]+)\/([\w-]+)(\/([\w-:]+))?/;
    if(url.value == "/") {
         return;
    }

    var result = parse_url.exec(url.value);
    if(result != null) {
        var element = result[1];
        var db = result[2];
        var element_id = result[4];

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
    }
}


function ajax_post(suburl, jsondata, username, password, callback) {
    $.ajax({url: CARNEADESWS.carneadeswsurl + suburl,
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
    $.ajax({url: CARNEADES.carneadeswsurl + suburl,
            type: 'GET',
            success : callback,
            dataType : 'json'
        });
}
