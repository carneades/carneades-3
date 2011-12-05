var CARNEADES = {
    lang : "en"
};

// this code is executed when the page is loaded
$(function() {
      $('#connect').click(on_connect);

      $.address.change(url_changed);
});

function url_changed(url)
{
    var parse_url = /\/(\w+)\/(\w+)\/(\w+)/;
    if(url.value == "/") {
         return;
    }

    var result = parse_url.exec(url.value);
    if(result != null) {
        var element = result[1];
        var db = result[2];
        var element_id = result[3];

        dispatch_url(element, db, element_id);
    }
}

function dispatch_url(element, db, element_id)
{
    if(element == "argument") {
        display_argument(db, element_id);
    } else if(element == "statement") {
        display_statement(db, element_id);
    }
}


function ajax_post(url, jsondata, callback) {
    $.ajax({url: suburl,
            type: "POST",
            data : {
                json : JSON.stringify(jsondata)
            },
            success: callback
        });
}

function ajax_get(suburl, callback) {
    $.ajax({url: suburl,
            type: 'GET',
            success : callback,
            dataType : "json"
        });
}
