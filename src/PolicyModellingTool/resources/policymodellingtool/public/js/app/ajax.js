
PM.ajax_post = function(url, jsondata, callback, username, password,
                        callback_error) {
    $.ajax({url: url,
            type: 'POST',
            'beforeSend' : function(xhr) {
                var bytes = Crypto.charenc.Binary.stringToBytes(username + ":" + password);
                var base64 = Crypto.util.bytesToBase64(bytes);
                xhr.setRequestHeader("Authorization", "Basic " + base64);
            },
            dataType : 'json',
            data : JSON.stringify(jsondata),
            contentType: "application/json; charset=utf-8",
            success: callback,
            error: function(jqXHR, textStatus) {
                console.log('[ERROR] ' + textStatus);
                if(!_.isNil(callback_error)) {
                    callback_error(textStatus);
                }
            }
        });
};

PM.ajax_get = function(url, callback, callback_error) {
    $.ajax({url: url,
            type: 'GET',
            success : callback,
            dataType : 'json',
            error: function(jqXHR, textStatus) {
                console.log('[ERROR] AJAX ' + textStatus);
                if(!_.isNil(callback_error)) {
                    callback_error(textStatus);
                }
            }
        });
};

PM.ajax_delete  = function(url, callback, username, password,
                        callback_error) {
    $.ajax({url: url,
            type: 'DELETE',
            success : callback,
            'beforeSend' : function(xhr) {
                var bytes = Crypto.charenc.Binary.stringToBytes(username + ":" + password);
                var base64 = Crypto.util.bytesToBase64(bytes);
                xhr.setRequestHeader("Authorization", "Basic " + base64);
            },
            dataType : 'json',
            error: function(jqXHR, textStatus) {
                console.log('[ERROR] AJAX ' + textStatus);
                if(!_.isNil(callback_error)) {
                    callback_error(textStatus);
                }
            }
           });
};

PM.ajax_put = function(url, jsondata, callback, username, password,
                        callback_error) {
    $.ajax({url: url,
            type: 'PUT',
            'beforeSend' : function(xhr) {
                var bytes = Crypto.charenc.Binary.stringToBytes(username + ":" + password);
                var base64 = Crypto.util.bytesToBase64(bytes);
                xhr.setRequestHeader("Authorization", "Basic " + base64);
            },
            dataType : 'json',
            data : JSON.stringify(jsondata),
            contentType: "application/json; charset=utf-8",
            success: callback,
            error: function(jqXHR, textStatus) {
                console.log('[ERROR] ' + textStatus);
                if(!_.isNil(callback_error)) {
                    callback_error(textStatus);
                }
            }
        });
};
