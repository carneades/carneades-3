/**
 * This is the AJAX-Engine for the IMPACT web application.
 *
 * @author bbr
 * @version 0.26
 */

/** Settings */
var showhints = true;
var jsloadstarted = new Date();

/**
 * Fixes a javascript bug that makes copying of Arrays impossible
 * @returns returns a copy of the array
 * @type Array
 */
Array.prototype.copy = function () {
    return ((new Array()).concat(this));
};

/**
 * Returns the class of the given parameter.
 * @param o can be anything
 * @returns returns the class
 * @type Boolean
 */
function classOf(o) {
    if (undefined === o) return "Undefined";
    if (null === o) return "Null";
    return {}.toString.call(o).slice(8, -1);
}

/**
 * Initialisation
 * @constructor
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
    $.datepicker.setDefaults({
            regional: "de",
            changeMonth: true,
            changeYear: true,
            yearRange: (''+(jsloadstarted.getFullYear()-100)+':'+(jsloadstarted.getFullYear()+10)),
            showAnim: 'slideDown'
    });

    //button
     $(".ui-button").button();

    // Land -> Datum
    $( "#locale" ).change(function() {
        alert("? "+$(this).val() );
        /*
        $("body").append("<script src=\""+"http://jquery-ui.googlecode.com/svn/trunk/ui/i18n/jquery.ui.datepicker-"+$(this).val()+".js\" type=\"text/javascript\"></script>");
        $.datepicker.setDefaults($.datepicker.regional[$(this).val()]);
        /*$( ".datepicker" ).each(function(index) {
                $(this).datepicker( "option",
                "regional", $("#locale").val() );
        })*/
        alert("!");
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

    /** AJAX request config */
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

    /** loads initial questions */
    $("#questions").empty();
    doAJAX(); // call for questions
});

/**
 * Sends a ajax request to the server and manage the output of the reply.
 * @param {JSON} jsondata expects a json object with the given answers
 * @see sendAnswers
 * @see statusupdate
 * @see RadioCheckNewLine
 */
function doAJAX(jsondata) {
    if (typeof jsondata == "undefined") var jsondata = null;
    //else alert("sending = "+JSON.stringify(jsondata));
    $.ajax({
        dataType : "json",
        data : {
            json : JSON.stringify(jsondata)
        },
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
                        var newline = RadioCheckNewLine(item.answers);
                        $.each(item.answers, function(answindex, answer) {
                            if (newline) output += "<br/>";
                            output += "<input id=\"qID"+item.id+"\" name=\"qID"+item.id+"\" type=\"radio\">";
                            output += "<span onclick=\"$(this).prev().click()\">"+answer+"</span>";
                        });
                    }
                    else if (item.type == "date") {
                        output += "<input type=\"text\" class=\"datefield\" id=\"qID"+item.id+"\" name=\"qID"+item.id+"\""+((item.answers && item.answers[0]!="") ? " value=\""+item.answers[0]+"\"" : "")+"/>";
                    }
                    else if (item.type == "int") {
                        output += "<input type=\"text\" class=\"integer\" id=\"qID"+item.id+"\" name=\"qID"+item.id+"\""+((item.answers && item.answers[0]!="") ? " value=\""+item.answers[0]+"\"" : "")+"/>";
                    }
                    else output += "<input type=\""+item.type+"\" id=\"qID"+item.id+"\" name=\"qID"+item.id+"\""+((item.answers && item.answers[0]!="") ? " value=\""+item.answers[0]+"\"" : "")+"/>";
                    if (item.hint) output += "<span class=\"hint qinfo\"><i></i>"+item.hint+"<b></b></span>";
                    output += "</p>";
                    $(qbox).append(output);
                    if (item.type != "radio") {
                        $(":input", qbox).focusin(function(){
                            if (showhints) $(this).next(".hint").fadeIn(400);
                        }).focusout(function(){
                            $(this).next(".hint").fadeOut(600);
                        }).change(function(){
                            validateField(this);
                        });
                    }
                });
                $(qbox).append('<input type="button" class="ui-button next" value="next" onclick="sendAnswers(this.parentNode)"/>');
                $('.datefield').datepicker();
                $("#questions").accordion({
                    header: "h3",
                    autoHeight: false,
                    navigation: true
                });
            }
            else {
                alert("FAILED: "+data);
            }
        }
    });
}

/**
 * Vaildates the formfield content.
 * @param {object} obj Formfield was recently changed or a array that contains this objects
 * @returns returns if a form-field or a set of fields has passed validation
 * @type Boolean
 * @see qwarn
 * @see qunwarn
 * @see sendAnswers
 */
function validateField(obj) {
    if (classOf(obj) == "Array") {
        for (var i=0;i < obj.length;i++) {
            if (validateForm(obj[i])=== false) return false;
        }
    }
    else if (classOf(obj).indexOf("HTML") == 0) {
        qunwarn(this);
        var o = $(obj);
        if (o.hasClass("integer")) {
            if (o.val().search(/\D/) != -1) {
                qwarn(this,"Please insert a integer. No characters or whitespaces allowed.");
                return false;
            }
        }
        if (o.hasClass("datefield")) {
            //if (o.val().search(/\d\d[.\/]\d\d[.\/]\d\d\d\d/) != -1) qwarn(this,"Please insert a integer. No characters or whitespaces allowed.");
            alert("OH SHI-");
        }
        // empty field
        if (o.val() == "" && !o.hasClass("optional")) {
            qwarn(this,"This field is required.");
            return false;
        }
        return true;
    }
    else {
        alert("nothing to validate");
        return false;
    }
}

/**
 * Checks if radio or checkbox input fields needs a new line to seperate them
 * @param {Array} answers Array that contains all possible answers
 * @see doAJAX
 * @returns true when a new line is required
 * @type Boolean
 */
function RadioCheckNewLine(answers) {
    var newline=false;
    for (var i=0; i < answers.length; i++) {
        if (answers[i].length > 12 || i > 4) {
            newline=true;
            break;
        }
    }
    return newline;
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
 * @param {number} type Expect a integer with the value of the status. 0 means loading 1 an error and -1 that there is everthing allright so loaded.
 * @param {string} text Here goes the Text that will be displayed in the status.
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
 * Collects the given answers and parse them as JSON before sending them to {@link doAJAX}.
 * @param {object} obj expects a HTML object that includes the input fields for the given answers.
 * @see doAJAX
 * @see validateField
 */
function sendAnswers(obj) {
    var doRequest = true;
    var jsonA = new Array();
    $(obj).children("p").children(":input").each(function(i, itemobj){ // Pseudoklassen nicht gefunden!
        if (validateField(itemobj) === false) {
            doRequest = false;
            return false
        }
        var item = $(itemobj);
        //alert(item.attr("name") + " : " + item.val());
        var jsonitem = new Object();
        jsonitem = {
            "id" : item.attr("name"),
            "value" : item.val()
        }
        jsonA.push(jsonitem);
    });
    var jsonZ = {"answers" : jsonA.copy()}
    if (doRequest) doAJAX(jsonZ);
}

/**
 * Displays a warning besides a form field when invalid data is used. To hide this use {@link qunwarn}
 * @param {object} obj triggering form field HTML object
 * @param {string} warning text that appears right besides the field
 * @see qunwarn
 */
function qwarn(obj,warning) {
    var o = $(obj);
    o.next(".hint").hide();
    o.css("backgroundColor","#F78181");
    o.after("<p class=\"qwarn\">"+warning+"</p>");
}

/**
 * Hides a {@link qwarn}-warning.
 * @param {Object} obj triggering form field HTML object
 * @see qwarn
 */
function qunwarn(obj) {
    var o = $(obj);
    o.css("backgroundColor","#ffffff");
    o.next(".qwarn").remove();
}