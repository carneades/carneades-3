/**
 * This is the AJAX-Engine for the IMPACT web application.
 *
 * @author bbr
 * @version 0.24
 */

/**
 * Fixes a javascript bug that makes copying of Arrays impossible
 * @return returns a copy of the array
 */
Array.prototype.copy = function () {
    return ((new Array()).concat(this));
};

/**
 * Initialisation
 */
$(function(){ // Init

    /* Accordion
    $("#questions").accordion({
            header: "h3",
            autoHeight: false,
            navigation: true
    });*/

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

                    // Neue Fragen Hinzufuegen? --DOM MANIPULATION--

                    // Naechste Frage oeffnen oder Ende?
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
       url: "/CarneadesWS-web/CarneadesServletDemo",
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
       dataType: "json",
       type: "POST"
    });
    
    $("#questions").empty();
    doAJAX(); // call for questions
});

/**
 * Sends a ajax request to the server and manage the output of the reply.
 * @param json expects a json object with the given answers
 * @see sendAnswers
 * @see statusupdate
 */
function doAJAX(jsondata) {
    if (typeof jsondata == "undefined") var jsondata = null;
    else alert("sending = "+JSON.stringify(jsondata));
    $.ajax({
        dataType : "json",
        data : jsondata,
        success : function(data){
            if (data != null && data.questions) { // getting questions
                var qbox = $("#questions");
                qbox.removeClass();
                qbox.append("<div><h3><a href=\"#\">"+data.questions[0].category+"</a></h3><div><p></p></div></div>");
                qbox = $("p:last", qbox);
                $.each(data.questions, function(i,item){
                    var output = "<p>"+item.question;
                    if (item.type == "select") {
                        output += "<select id=\"qID"+item.id+"\" name=\"qID"+item.id+"\">";
                        $.each(item.answers, function(answindex, answer) {
                            output += "<option value=\""+answer+"\">"+answer+"</option>";
                        });
                        output += "</select>";
                    }
                    else if (item.type == "radio") {
                        $.each(item.answers, function(answindex, answer) {
                            output += "<p><input id=\"qID"+item.id+"\" name=\"qID"+item.id+"\" type=\"radio\">";
                            output += "<span onclick=\"$(this).prev().click()\">"+answer+"</span></p>";
                        });
                    }
                    else if (item.type == "date") {
                        output += "<input type=\"text\" class=\"datefield\" id=\"qID"+item.id+"\" name=\"qID"+item.id+"\""+((item.answers && item.answers[0]!="") ? " value=\""+item.answers[0]+"\"" : "")+"/>";
                    }
                    else if (item.type == "int") {
                        output += "<input type=\"text\" class=\"integer\" id=\"qID"+item.id+"\" name=\"qID"+item.id+"\""+((item.answers && item.answers[0]!="") ? " value=\""+item.answers[0]+"\"" : "")+"/>";
                    }
                    else output += "<input type=\""+item.type+"\" id=\"qID"+item.id+"\" name=\"qID"+item.id+"\""+((item.answers && item.answers[0]!="") ? " value=\""+item.answers[0]+"\"" : "")+"/>";
                    output += "<span class=\"hint\"><div class=\"qinfo\"/> </div>"+((item.hint) ? item.hint : "")+"</span>";
                    output += "</p>";
                    $(qbox).append(output);
                    $("p:last", qbox).focusin(function(){
                        $(this).find("span").fadeIn(500);
                    }).focusout(function(){
                        $(this).find("span").fadeOut(1000);
                    });
                });
                $(qbox).append('<input type="button" class="ui-button next" value="next" onclick="sendAnswers(this.parentNode)"/>');
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
                $("#questions").accordion({
                    header: "h3",
                    autoHeight: false,
                    navigation: true
                });
            }
            else /*if (typeof data == "string")*/ {
                alert("FAILED: "+data);
            }
        }
    });
}

/*
 *  liste bearbeiten.
              var qlist = $("#question");
              qlist.empty();
              $.each(data.questions, function(i,item){
                  qlist.append("<li onclick=\"loadQuestions('"+item.id+"')\">"+item.name+" ("+item.len+")</li>");
              });
 *
 **/

/**
 * Updates the statusfield of the page.
 * @param type Expect a integer with the value of the status. 0 means loading 1 an error and -1 that there is everthing allright so loaded.
 * @param text Here goes the Text that will be displayed in the status.
 */
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

/**
 * Collects the given answers and parse them as JSON before sending them to @link doAJAX .
 * @param obj expects a HTML object that includes the input fields for the given answers.
 * @see doAJAX
 */
function sendAnswers(obj) {
    var jsonA = new Array();
    $(obj).children("p").children(":input").each(function(i, itemobj){ // Pseudoklassen nicht gefunden!
        var item = $(itemobj);
        if ( !item.val() ) return false;
        //alert(item.attr("name") + " : " + item.val());
        var jsonitem = new Object();
        jsonitem = {
            "id" : item.attr("name"),
            "value" : item.val()
        }
        jsonA.push(jsonitem);
    });
    
    var jsonZ = { "answers" : jsonA.copy() }
    doAJAX(jsonZ);
}

/**
 * Displays a warning besides a form field when invalid data is used. To hide this use @link qunwarn
 * @param obj triggering form field HTML object
 * @param warning text that appears right besides the field
 * @see qunwarn
 */
function qwarn(obj,warning) {
    var o = $(obj);
    o.css("backgroundColor","#F78181");
    o.after("<div class=\"qwarn\">"+warning+"</div>");
}

/**
 * Hides a warning besides a form field. @link qwarn
 * @param obj triggering form field HTML object
 * @see qwarn
 */
function qunwarn(obj) {
    var o = $(obj);
    o.css("backgroundColor","#ffffff");
    if (obj.nextSilbing.className == "qwarn") o.next().remove();
}