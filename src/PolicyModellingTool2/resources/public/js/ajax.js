function send_data(url, jsondata, callback) {
    $.ajax({"url" : url,
            dataType : "json",
            data : {
                json : JSON.stringify(jsondata)
            },
            success : callback});
}
