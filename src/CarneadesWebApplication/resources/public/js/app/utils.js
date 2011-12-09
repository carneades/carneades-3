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

function escape_html(text)
{
    return $('<div/>').text(text).html();
}

function is_url(url)
{
    return /^(http|https|ftp)/.test(url);
}
