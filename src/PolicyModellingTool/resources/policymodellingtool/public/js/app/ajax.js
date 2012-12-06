// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

PM.simple_auth = function(xhr) {
    var bytes = Crypto.charenc.Binary.stringToBytes(IMPACT.user + ":" + IMPACT.password);
    var base64 = Crypto.util.bytesToBase64(bytes);
    xhr.setRequestHeader("Authorization", "Basic " + base64);
};

PM.send_authentification = function(username, password, xhr) {
    var bytes = Crypto.charenc.Binary.stringToBytes(username + ":" + password);
    var base64 = Crypto.util.bytesToBase64(bytes);
    xhr.setRequestHeader("Authorization", "Basic " + base64);
};

PM.ajax_post = function(url, jsondata, callback, username, password,
                        callback_error) {
    $.ajax({url: url,
            type: 'POST',
            'beforeSend': _.bind(PM.send_authentification, PM, username, password),
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
    $.ajaxSetup({beforeSend: PM.simple_auth});
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
    $.ajaxSetup({beforeSend: PM.simple_auth});
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
    $.ajaxSetup({beforeSend: PM.simple_auth});
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
    $.ajaxSetup({beforeSend: PM.simple_auth});
};
