
PM.ajax_post = function(suburl, jsondata, callback, username, password) {
    $.ajax({url: IMPACT.pmws_url + suburl,
            type: 'POST',
            // 'beforeSend' : function(xhr) {
            //     var bytes = Crypto.charenc.Binary.stringToBytes(username + ":" + password);
            //     var base64 = Crypto.util.bytesToBase64(bytes);
            //     xhr.setRequestHeader("Authorization", "Basic " + base64);
            // },
            dataType : 'json',
            data : JSON.stringify(jsondata),
            contentType: "application/json; charset=utf-8",
            success: callback
        });
};

PM.ajax_get = function(suburl, callback) {
    $.ajax({url: IMPACT.pmws_url + suburl,
            type: 'GET',
            success : callback,
            dataType : 'json'
        });
};
