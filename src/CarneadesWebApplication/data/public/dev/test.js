function ajax_get(suburl, callback) {
    $.ajax({url: suburl,
            type: 'GET',
            success : callback,
            dataType : "json"
        });
}

// this code is executed when the page is loaded
$(function() {
      ajax_get('/argumentgraph-info/aston', 
               function(data) {
                   $('#impactcontent').text(data.metadata[0].title);
               });
});
