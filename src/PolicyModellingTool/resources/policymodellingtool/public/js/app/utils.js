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
