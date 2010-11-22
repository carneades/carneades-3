/* 
 * Editor: bbr
 */

// Fix: Ermöglicht das kopieren von Arrays
Array.prototype.copy = function () {
    return ((new Array()).concat(this));
};

$(function(){ // Init

        // Accordion
        $("#questions").accordion({
                header: "h3",
                autoHeight: false,
                navigation: true
        });

        // Progressbar
        $("#progressbar").progressbar({
                value: 0
        });

        // Tabs
        $('#tabs').tabs();

        // Datepicker
        $('#datepicker').datepicker({
                inline: true
        });
        $('.datefield').datepicker({
                regional: "de",
                changeMonth: true,
                changeYear: true
        });

        //button
         $(".ui-button").button();

        // Land -> Datum
        $( "#locale" ).change(function() {
                $( ".datepicker" ).each(function(index) {
                        $(this).datepicker( "option",
                        "regional", $("#locale").val() );
                })
        });

        // Fragen-Liste
        $("li", $("#questionlist")).each(function(index){
                //this.style.backgroundColor="red";
                var li_i = index;
                $(this).click(function(){
                        $(document.getElementsByTagName("h3")[li_i].firstChild).click();
                });
        });

        // Next-Button
        $(".next").each(function(index){
                var nx_i = index;
                $(this).click(function(){
                        // Ergebnis validieren

                        // Daten an Server senden! --AJAX REQUEST HERE--

                        // Neue Fragen Hinzuf�gen? --DOM MANIPULATION--

                        // N�chste Frage �ffnen oder Ende?
                        if (document.getElementsByTagName("h3")[nx_i+1]) {
                                $(document.getElementsByTagName("h3")[nx_i+1].firstChild).click();
                        }
                        else { // Ende
                                alert('done.');
                                $("#tabs-1").html("<h1>Analysis</h1>"+
                                "<p>Not able to start analysis.</p>" /* Server-Antwort */ );
                                statusupdate(1,"Server did not respond.");
                        }
                });
        });

        // Load Content
        $.ajaxSetup({
           url: "/CarneadesWS-web/CarneadesServlet",
           async: true,
           beforeSend: function() {
               statusupdate(0,"Please be patient.");
           },
           complete: function(XMLHttpRequest, textStatus) {
               if (textStatus == "success")
                   $("#status").fadeOut();
               else if(textStatus == "error")
                   statusupdate(1,XMLHttpRequest.status+" "+textStatus);
               else // "notmodified", "timeout", or "parsererror"
                   statusupdate(1,textStatus);
           },
           timeout : 60000,
           dataType: 'json',
           type: "POST"
        });

        $.ajax({
          data: {
              doWhat : "topics"
          },
          success: function(data){
              var qlist = $("#questionlist");
              qlist.empty();
              $.each(data.content, function(i,item){
                  qlist.append("<li onclick=\"loadQuestions('"+item.id+"')\">"+item.name+" ("+item.len+")</li>");
              });
          }
        });
});

function statusupdate(type, text) {
    var icon="";
    if (type == 0) { // Loading
            $("#status").removeClass("ui-state-error");
            $("#status").addClass("ui-state-highlight");
            icon='<p><span class="ui-icon ui-icon-info" style="float: left; margin-right: 0.3em;"></span> <strong>Loading:</strong> ';
    }
    else if (type == 1) { // Alert
            $("#status").removeClass("ui-state-highlight");
            $("#status").addClass("ui-state-error");
            icon='<p><span class="ui-icon ui-icon-alert" style="float: left; margin-right: .3em;"></span> <strong>Alert:</strong> ';
    }
    else if (type == -1) {$("#status").hide();return false;}
    else {$("#status").hide();return false;}
    $("#status").html(icon+text+"</p>");
    $("#status").show();
}

function loadQuestions(id) {
    $.ajax({
      data: {
          doWhat : "questions",
          id : id
      },
      success: function(data){
          var qbox = $("#questions");
          var output;
          qbox.empty();
          $.each(data.content, function(i,item){
                output = "<div>";
                output += "<h3><a href=\"#\">"+item.question+"</a></h3>";
                if (item.type == "select") {
                    output += "<select id=\"qID"+item.id+"\" name=\"qID"+item.id+"\">";
                    $.each(item.answers, function(answindex, answer) {
                       output += "<option value=\""+answer+"\">"+answer+"</option>"; 
                    });
                }
                else if (item.type == "date") {
                    output += "<input type=\"text\" class=\"datefield\" id=\"qID"+item.id+"\" name=\"qID"+item.id+"\"/>";
                }
                else if (item.type == "int") {
                    output += "<input type=\"text\" class=\"integer\" id=\"qID"+item.id+"\" name=\"qID"+item.id+"\"/>";
                }
                else output += "<input type=\""+item.type+"\" id=\"qID"+item.id+"\" name=\"qID"+item.id+"\"/>";
                qbox.append(output);
          });
          $('.datefield').datepicker({
                regional: "de",
                changeMonth: true,
                changeYear: true
          });
          $('.integer').change(function(){
               if (this.value.search(/\D/) != -1) {
                   qwarn(this,"Please insert a integer. No characters or whitespaces allowed.");
               }

          });
      }
    });
}

// Question Warning
// parm obj = form field object
// parm warning = text that appears right bisides the field
function qwarn(obj,warning) {
    var o = $(obj);
    o.css("backgroundColor","#F78181");
    o.after("<div class=\"qwarn\">"+warning+"</div>");
}
function qunwarn(obj,warning) {
    var o = $(obj);
    o.css("backgroundColor","#ffffff");
    if (obj.nextSilbing.className == "qwarn") $(obj.nextSibling).remove();
}