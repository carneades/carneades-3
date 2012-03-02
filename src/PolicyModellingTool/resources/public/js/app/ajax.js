// function send_data(url, jsondata, callback) {
//     $.ajax({"url" : url,
//             dataType : "json",
//             data : {
//                 json : JSON.stringify(jsondata)
//             },
//             success : callback});
// }

function send_data(suburl, jsondata, callback) {
    $.ajax({url: suburl,
            type: 'POST',
            dataType : 'json',
            data : JSON.stringify(jsondata),
            contentType: "application/json; charset=utf-8",
            success: callback
        });
}
