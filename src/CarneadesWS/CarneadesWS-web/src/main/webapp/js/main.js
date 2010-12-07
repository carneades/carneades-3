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
 * Initialisation
 * @constructor
 */
$(function(){ // Init

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
    doAJAX(); // call for questions
});

/**
 * Sends a ajax request to the server and manage the output of the reply.
 * @param {JSON} jsondata expects a json object with the given answers
 * @see sendAnswers
 * @see statusupdate
 * @see radioCheckNewLine
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
                qbox.empty();
                qbox.append("<div id=\"hints\"><h4>hints</h4></div>");
                qbox.append("<div><h3>"+data.questions[0].category+"</h3><div id=\"qcontent\"></div></div>");
                qbox = $("#qcontent", qbox);
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
                        var newline = radioCheckNewLine(item.answers);
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
                    if (item.hint) $("#hints").append("<p id=\"qHINT"+item.id+"\" class=\"hint\">"+item.hint+"</p>");
                    output += "</p>";
                    qbox.append(output);
                    if (item.type != "radio") {
                        $(":input", qbox).focus(function(){
                            if (showhints) {
                                $("#qHINT"+this.id.substring(3)).show();
                            }
                        })
                        $(":input", qbox).blur(function(){
                            $("#qHINT"+this.id.substring(3)).css('display','none');
                        })
                        $(":input", qbox).change(function(){
                            validateField(this);
                        });
                    }
                });
                qbox.append('<input type="button" class="ui-button next" value="next" onclick="sendAnswers(this.parentNode)"/>');
                $('.datefield').datepicker();
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
    var result = true;
    // Array
    if ($.type(obj) == "array") {
        for (var i=0;i < obj.length;i++) {
            if (validateForm(obj[i])=== false) result = false;
        }
        return result;
    }
    // DIV (qcontent)
    else if ($.type(obj) == "object" && obj.nodeName == "DIV") {
        $(":input", obj).each( function(i, elem) {
            if ( !validateField(elem) ) result = false;
        });
        return result;
    }
    // input
    else if ($.type(obj) == "object" && (obj.nodeName == "INPUT"
             || obj.nodeName == "SELECT" || obj.nodeName == "TEXTFIELD") ) {
        var o = $(obj);
        if (obj.type == "button") return true; // buttons ignorieren
        if (o.hasClass("integer")) {
            if (o.val().search(/\D/) != -1) {
                qwarn(obj,"Please insert a integer. No characters or whitespaces allowed.");
                return false;
            }
        }
        if (o.hasClass("datefield")) {
            //if (o.val().search(/\d\d[.\/]\d\d[.\/]\d\d\d\d/) != -1) qwarn(this,"Please insert a integer. No characters or whitespaces allowed.");
        }
        // empty field
        if (o.val() == "" && !o.hasClass("optional")) {
            qwarn(obj,"This field is required.");
            return false;
        }
        qunwarn(obj);
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
function radioCheckNewLine(answers) {
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
    $(":input", obj).each(function(i, itemobj){
        if (validateField(itemobj) === false) {
            doRequest = false;
        }
        var item = $(itemobj);
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
    if (o.next(".qwarn").length > 0) {
        o.next(".qwarn").html("<i></i>"+warning+"<b></b>")
    }
    else {
        o.css("backgroundColor","#F78181");
        o.after("<span class=\"qwarn\"><i></i>"+warning+"<b></b></span>");
        o.next().fadeIn(500);
    }
}

/**
 * Hides a {@link qwarn}-warning.
 * @param {Object} obj triggering form field HTML object
 * @see qwarn
 */
function qunwarn(obj) {
    var o = $(obj);
    o.css("backgroundColor","#ffffff");
    o.next(".qwarn").fadeOut(500, function() { $(this).remove(); });
}