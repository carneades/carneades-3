// this code is executed when the page is loaded
$(function() {
      $('#submit').click(
          function() {
              var query = $('#query').val();
              var database = $('#database').val();
              
              if(query == "") { query = "(undercut ?p)"; }
              if(database == "") { database = "copyright"; }
              
              console.log('query: ' + query);
              ajax_post('matching-statements/'+ database, query, null, null, on_demo_success);
              return false;
        });
});

function on_demo_success(results) {
    console.log('result:' + result);
    $('#result').empty();
    _.each(results, display_one_result);
}

function display_one_result(result) {
    var res = $('#result');
    res.append('STATEMENT<br/>');
    _.each(result.statement, function(value, key) {        
               res.append(key + ' = ' + value + '<br/>');        
           });
}