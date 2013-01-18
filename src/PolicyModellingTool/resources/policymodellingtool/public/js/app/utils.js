// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

// adds a format method to all string
// place holders are of the form {0}, {1} etc

String.prototype.format = function() {
  var args = arguments;
  return this.replace(/{(\d+)}/g, function(match, number) {
    return typeof args[number] != 'undefined'
      ? args[number]
      : match
    ;
  });
};

PM.normalize = function(o) {
    for(name in o) {
        if(typeof o[name] !== 'function') {
            var normalized = name.replace(/-/g, '_');
            if(normalized !== name) {
                o[normalized] = o[name];
                delete o[name];                            
            }
        }
    }
};

var UTILS = {
    
};

UTILS.escape_html = function escape_html(text) {
    return $('<div/>').text(text).html();
};

UTILS.is_url = function(url) {
    return /^(http|https|ftp)/.test(url);
};

UTILS.gen_id = function() {
    var newDate = new Date;
    return newDate.getTime();
};

PM.get_cookies = function() {
    var cookies = { };

    if (document.cookie && document.cookie != '') {
        var split = document.cookie.split(';');
        for (var i = 0; i < split.length; i++) {
            var name_value = split[i].split("=");
            name_value[0] = name_value[0].replace(/^ /, '');
            cookies[decodeURIComponent(name_value[0])] = decodeURIComponent(name_value[1]);
        }
    }

    return cookies;
   
};
