var CARNEADES = {
    dburl : "http://localhost:8080",
    lang : "en"
};

// this function will be exectued when the page is loaded
$(function() {
      $('#connect').click(on_connect);
});


function do_ajax_post(suburl, jsondata, callback) {
    $.ajax({url: CARNEADES.dburl + suburl,
            type: "POST",
            data : {
                json : JSON.stringify(jsondata)
            },
            success: callback
        });
}

function do_ajax_get(suburl, callback) {
    $.ajax({url: CARNEADES.dburl + suburl,
            type: 'GET',
            success : callback,
            dataType : "json"
        });
}
