var CARNEADES = {
    lang : "en"
};

// this code is executed when the page is loaded
$(function() {
      $('#connect').click(on_connect);
});

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
