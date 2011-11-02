/**
 * This is the AJAX-Engine for the IMPACT web application.
 *
 */

/** Settings */
var jsloadstarted = new Date();
var showhints = true;

/** global vars */
var argGraph = "undefined";
var policyrules = [];
var langChange = false;
var svgScale = 1;
var svgWrapper = null;
var svgLayout = "hierarchical";
var svgTreeify = "true";
var translate = 'translate(0,0)';
var debug = true;

// adding JSON parser when browser is too old to have a build-in one (pre-IE8, pre-FF3.5, ...)
if( typeof( window[ 'JSON' ] ) == "undefined" ) document.write('<script type="text/javascript" src="https://github.com/douglascrockford/JSON-js/raw/master/json2.js"/>');

/**
 * Fixes a javascript bug that makes copying of Arrays impossible
 * @returns returns a copy of the array
 * @type Array
 */
Array.prototype.copy = function () {
    return ((new Array()).concat(this));
};

function translation_url() {
    return debug ? "/Translation" : "/PolicyModellingTool2/Translation"
}

function simulation_url() {
    return debug ? "/PolicySimulation" : "/PolicyModellingTool2/PolicySimulation"
}

function evaluation_url() {
    return debug ? "/PolicyEvaluation" : "/PolicyModellingTool2/PolicyEvaluation"
}

// when the document is ready, executes this code:
$(function(){
    // enforces the choice of a topic
    $('#nextTopic').click(function () {
        $('#chooseTopic').validate();
        if($('#chooseTopic').valid()) {
            loadTopic($('#topic').val());
            return true;
        } else {
            showErrorStatus('Please select a topic');
            return false;
        }
    });

    $('#abductionButton').click(function () {
        sendAbductionRequest();
    });
    
    // Slider for the graphSVG
    $('#slider').slider({orientation: 'vertical', value: 0, min: -50, max: 50, step: 1, change: onSliderMove});
    
    // Tabs
    $('#tabs').tabs();

    $.datepicker.setDefaults({
            //regional: "de",
            inline: true,
            changeMonth: true,
            changeYear: true,
            yearRange: (''+(jsloadstarted.getFullYear()-100)+':'+(jsloadstarted.getFullYear()+10)),
            showAnim: 'slideDown'
    });

    //button
     $(".ui-button").button();
    
    // Land -> Datum
    $("#locate").change(function(){
        doAJAX(translation_url(), {"language" : $(this).val()});
        langChange = true;
    });

    // Fragen-Liste
    $("li", $("#questionlist")).each(function(index){            
            var li_i = index;
            $(this).click(function(){
                    $(document.getElementsByTagName("h3")[li_i].firstChild).click();
            });
    });

    /** AJAX request config */
    $.ajaxSetup({
            //       url: "/PolicyModellingTool/PolicySimulation",
        url: simulation_url(),
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
       timeout : 600000,       
       type: "POST"
    });

    doAJAX(translation_url(), {get_available_languages : null});
});

/**
 * loads questions for requested topic
 * @param {string} t name of the topic
 * @see doAJAX
 */
function loadTopic(t) {
    doAJAX(simulation_url(), {"request" : t} );
}

/**
 * Sends a ajax request to the server and manage the output of the reply.
 * @param {JSON} jsondata expects a json object with the given answers
 * @see sendAnswers
 * @see statusupdate
 * @see radioCheckNewLine
 */
function doAJAX(url, jsondata) {
    if (typeof jsondata == "undefined") var jsondata = null;
    //else alert("sending = "+JSON.stringify(jsondata));
    $.ajax({
        "url" : url,
        dataType : "json",
        data : {
            json : JSON.stringify(jsondata)
        },
        success : function(data) {
            if (data == null || data == "") alert("Empty Server Answer!")
            // getting questions
            else if (data.questions && data.questions.length >= 1) {
                showQuestions(data.questions);
            }
            // getting solution
            else if (data.solution) {
                showSolution(data.solution, data.path);
            } else if (data.session) {
                // alert(data.session);
                resetContent();
            }
            else if (data.language) {
                // alert("Language set to: "+data.language);
            } else if(data.policyrules) {
                showPolicyRules(data.policyrules);
            } else if(data.evaluated) {
                showArgGraph(data.evaluated);
            } else if(data.graphpath) {
                showSVGGraph(data.graphpath)
            } else if (data.error) {
                showError(data.error);
            } else if(data.position) {
                showPosition(data.position, data.stmts_ids);
            } else if (data.available_languages) {
                showAvailableLanguages(data.available_languages);
            } else {    
                // alert(data);
                return data;
            }
        }
    });
}

function showAvailableLanguages(languages) {
    for(var name in languages) {
        $('#locate').append('<option value="' + languages[name] + '">' + name + '</option>');
    }
            
    // assumes English is always there and set it as default language
    $('#locate').val('en');

}

function showPosition(position, stmts_ids) {
    showPolicy(position[0], stmts_ids);
    // TODO: show all policies
}

function showPolicy(policy, stmts_ids) {
    var stmt = policy[1];
    var policyid = stmts_ids["(valid " + stmt + ")"];
    var g = $('g [id="' + policyid + '"]');
    var rect = $('g [id="' + policyid + '"] rect');

    var x = parseInt(rect.attr('x'), 10) - 4;
    var y = parseInt(rect.attr('y'), 10) - 4;
    var h = parseInt(rect.attr('height'), 10) + 8;
    var w = parseInt(rect.attr('width'), 10) + 8;

    $('svg').svg();
    var svg = $('svg').svg('get');
    var corner = 5;
    svg.rect(g, x, y, w, h, corner, corner, {"stroke-width" : 2, stroke : "purple", fill : "transparent"});
}

function genId() {
    var newDate = new Date;
    return newDate.getTime();
}

/**
 * Displays a list of questions
 * @param {object} questionArray json object representing the questions
 * @see doAJAX
 */
function showQuestions(questionArray) {
    $("#tabs a[href='#tabs-2']").click();        
    var topicName = questionArray[0].category;
    var topicID = topicName.replace(/\s/,"_");
    var qlist = $("#questionlist");
    // add new div to questionlist with id = topic name
    qlist.append('<div id="'+topicID+'"><h3>'+topicName+'</h3><div id="qcontent"></div></div>');
    var qdiv = $("#"+topicID, qlist);
    var qbox = $("#qcontent", qdiv);
    // for each question
    $.each(questionArray, function(i, item) {
        showQuestion(item, qbox);
    });

    
    // qbox.append('<input type="button" class="ui-button next" value="next" onclick="sendAnswers(\''+topicID+'\')"/>');
    var buttonId = genId();
    qbox.append('<input type="button" id="' + buttonId+ '" class="ui-button ui-widget ui-state-default ui-corner-all" value="next" />');

    $('#' + buttonId).click(function () {
        $('#questions').validate();
        if($('#questions').valid()) {
            sendAnswers(topicID);
        } else {
            showErrorStatus('Some fields are not filled');
        }
    });

    $('.datefield', qbox).datepicker();
    if(!langChange) {
        updateTopicList(topicName, topicID);
    }
}

/**
 * Displays a question depending on type
 * @param {object} question json object representing the question
 * @param {object} qcontent div inside topic div
 */
function showQuestion(item, qbox){
    var newline = false;
    // pre append formatting
    var output = "<p><label for=\"qID"+item.id+"\">"+item.question+"</label>";
    // select
    if (item.type == "select") {
        output += "<select class=\"required\" id=\"qID"+item.id+"\" name=\"qID"+item.id+"\">";
        output += "<option value=\"\" selected=\"selected\">-- please choose --</option>";

        $.each(item.answers, function(answindex, answer) {
            output += "<option value=\""+answer+"\">"+answer+"</option>";
        });
        output += "</select>";
    } else if (item.type == "radio" || item.type == "checkbox") {
        newline = radioCheckNewLine(item.answers);
        for(var i = 0; i < item.answers.length; i++) {
            if (newline) output += "<br/>";
            var answer = item.answers[i];
            var formalanswer = item.formalanswers[i];
            output += "<input class=\"required\" name=\"qID"+item.id+"\" type=\""+item.type+"\" value=\""+formalanswer+"\">";
            output += "<span onclick=\"$(this).prev().click()\">"+answer+"</span>";
        }
    } else if (item.type == "date") {
        output += "<input type=\"text\" class=\"datefield required\" id=\"qID"+item.id+"\" name=\"qID"+item.id+"\""+((item.answers && item.answers[0]!="") ? " value=\""+item.answers[0]+"\"" : "")+"/>";
    } else if (item.type == "int") {
        output += "<input type=\"text\" class=\"integer required\" id=\"qID"+item.id+"\" name=\"qID"+item.id+"\""+((item.answers && item.answers[0]!="") ? " value=\""+item.answers[0]+"\"" : "")+"/>";
    } else if (item.type == "float") {
        output += "<input type=\"text\" class=\"float required\" id=\"qID"+item.id+"\" name=\"qID"+item.id+"\""+((item.answers && item.answers[0]!="") ? " value=\""+item.answers[0]+"\"" : "")+"/>";
    } else {
        output += "<input class=\"required\" type=\""+item.type+"\" id=\"qID"+item.id+"\" name=\"qID"+item.id+"\""+
            ((item.answers && item.answers.length != 0) ? " value=\""+item.answers[0]+"\"" : "")+"/>";
    }
    
    output += "</p>";
    qbox.append(output);

    if (item.hint) $("#hints").append("<p id=\"qHINT"+item.id+"\" class=\"hint\">"+item.hint+"</p>");
    // post append formatting
    if (item.optional) $("p:last :input:first", qbox).addClass("optional");
    // focus and blur does not work on radio/checkbox
    if (item.type != "radio" && item.type != "checkbox") {
        $(":input:last", qbox).focus(function(){
            if (showhints) {
                $("#hints p").css('display','none');
                $("#qHINT"+this.id.substring(3)).show();
            }
        });
        $(":input:last", qbox).blur(function(){
            $("#qHINT"+this.id.substring(3)).css('display','none');
        });
        // validation
        $(":input:last", qbox).change(function(){
            validateField(this);
        });
    }
    else { // radios & checkboxes
        $("input:last", qbox).parent().mouseover(function(){
            if (showhints) {
                var hinton=$("#hints > p:not(:hidden)");
                // statusupdate(1,"Verstecke: "+((hinton.length > 0)?"#qID"+hinton.attr("id").substring(5):"-")+" | Zeige: "+"#qHINT"+$(this).children("input:first").attr("name").substring(3));
                if (hinton.length > 0) $("#qID"+hinton.attr("id").substring(5)).blur();
                $("#qHINT"+$(this).children("input:first").attr("name").substring(3)).show();
            }
        });
        $("input:last", qbox).parent().mouseout(function(){
            $("#qHINT"+$(this).children("input:first").attr("name").substring(3)).css('display','none');
        });
        // validation
        $("input[name='qID"+item.id+"']", qbox).change(function(){
            validateField($("input:first", this.parentNoded)[0]);
        });
    }
}

/**
 * Displays the solution
 * @param {string} solution string representing the main issue
 * @param {string} path url pointing to solution lkif
 * @see doAJAX
 */
function showSolution(solution, path) {
    // go to next tab
    $("#tabs a[href='#tabs-3']").click(); 
    // display solution statement
    $("#solutionstatement").append(solution);
    // communicate with evaluation servlet
    // get policy rules
    var json = {"policyrules" : path}
    doAJAX(evaluation_url(), json);
    // display argument graph
    showArgGraph(path);
}

/**
 * Vaildates the formfield content.
 * @param {object} obj a question formfield or a array or DIV-element that contains this HTML nodes
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
            if (validateForm(obj[i])=== false) {
                result = false;
                return false;
            }
        }
        return result;
    }
    // DIV (qcontent)
    else if ($.type(obj) == "object" && obj.nodeName == "DIV") {
        $(":input", obj).each( function(i, elem) {
            if ( !validateField(elem) ) {
                result = false;
                //return false;
            }
        });
        return result;
    }
    // validation:
    else if ($.type(obj) == "object" && (obj.nodeName == "INPUT"
             || obj.nodeName == "SELECT" || obj.nodeName == "TEXTFIELD") ) {
        var o = $(obj);
        // ignore buttons:
        if (obj.type && (obj.type == "button" || obj.type == "submit" || obj.type == "reset") ) return true;
        if (o.hasClass("integer")) {
            if (o.val().search(/\D/) != -1) {
                qwarn(obj,"Please insert a integer. No characters or whitespaces allowed.");
                return false;
            }
        }
        if (o.hasClass("float")) {
            if (o.val() != "" && o.val().search(/[\sa-zA-Z]/i) != -1 ||
               (null == /(^-?\d*\.?\d*$)/.exec(o.val()) ? true : /(^-?\d*\.?\d*$)/.exec(o.val())[0] !== o.val() ) ) {
                qwarn(obj,"No valid number found.");
                return false;
            }
        }
        if (o.hasClass("datefield")) {
            if (o.val() != "" && o.val().search(/[\sa-zA-Z]/i) != -1 || o.val().search(/\d\d[\.\/]\d\d[\.\/]\d\d\d\d/i) != -1
                && (null == /(\d\d[\.\/]\d\d[\.\/]\d\d\d\d)/i.exec(o.val()) ? true : /(\d\d[\.\/]\d\d[\.\/]\d\d\d\d)/i.exec(o.val())[0] !== o.val() ) ) {
                qwarn(obj,"Invalid date.");
                return false;
            }
        }
        // empty radio/box
        if ( obj.type == "radio" || obj.type == "checkbox") {
            if (obj.parentNode.getElementsByTagName("input")[0] == obj && !o.hasClass("optional")) {
                if ($("input:checked[name='"+obj.name+"']").length == 0) {
                    qwarn(obj,"This field is required.");
                    return false;
                }
                // something is selected
            }
            else { // abort validation because it already has been validated or is optional
                return true;
            }
        }
        // empty field
        else if (o.val() == "" && !o.hasClass("optional")) {
            qwarn(obj,"This field is required.");
            return false;
        }
        // valid content
        qunwarn(obj);
        return true;
    }
    else {
        alert("nothing to validate on field: "+obj.nodeName+" of type "+$.type(obj));
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

function showErrorStatus(text) {
    $("#status").removeClass("ui-state-highlight");
    $("#status").addClass("ui-state-error");
    icon='<p><span class="ui-icon ui-icon-alert" style="float: left; margin-right: .3em;"></span> <strong>Alert:</strong> ';
    $("#status").html(icon+text+"</p>");
    $("#status").show();
}



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
        showErrorStatus(text);
    }
    else if (type == -1) {$("#status").hide();return false;}
    else {$("#status").hide();return false;}
    $("#status").html(icon+text+"</p>");
    $("#status").show();
}


/**
 * Collects the given answers and parse them as JSON before sending them to {@link doAJAX}.
 * @param {string} topicID the ID of the question div
 * @see doAJAX
 * @see validateField
 */
function sendAnswers(topicID) {
    var doRequest = true;
    var jsonA = new Array();
    var topicDiv = $("#"+topicID);
    topicDiv.hide();
    $("input", topicDiv).each(function(i, itemobj){
        var item = $(itemobj);
        // skip buttons
        if (item.hasClass("ui-button") || itemobj.type && (itemobj.type == "button" || itemobj.type == "submit" || itemobj.type == "reset") ) return true;
        
        if (validateField(itemobj) == false) {
            doRequest = false;
            return true;
        }
        if (itemobj.type == "radio" || itemobj.type == "checkbox") {            
            
            /*var valArray = new Array();
            $(":input:checked[name='"+itemobj.name+"']", itemobj.parentNode).each(function(i, valobj) {
                valArray.push(valobj.value);
            });*/
            if(itemobj.checked) {
                var jsonitem = {
                    "id" : item.attr("name").substring(3),
                    "value" : item.val()
                }
                jsonA.push(jsonitem);
            }            
        }
        else {            
            var jsonitem = {
                "id" : item.attr("name").substring(3),
                "value" : item.val()
            }
            jsonA.push(jsonitem);
        }
    });
    $("select", topicDiv).each(function(i, itemobj){
        var item = $(itemobj);
        var jsonitem = {
            "id" : item.attr("name").substring(3),
            "value" : item.val()
        }
        jsonA.push(jsonitem);
    });
    var jsonZ = {"answers" : jsonA.copy()}
    langChange = false;
    if (doRequest) doAJAX(simulation_url(), jsonZ);
}

function sendAbductionRequest() {
    doAJAX(evaluation_url(), {abduction : {argGraph : argGraph, acceptability : $('input[name=abduction]').val()}});
}

/**
 * Displays a warning besides a form field when invalid data is used. To hide this use {@link qunwarn}
 * @param {object} obj triggering form field HTML object
 * @param {string} warning text that appears right besides the field
 * @see qunwarn
 */
function qwarn(obj,warning) {
    var o = $(obj);
    if ($(".qwarn", obj.parentNode).length > 0) {
        $(".qwarn", obj.parentNode).html("<i></i>"+warning+"<b></b>");
    }
    else if (obj.type == "radio" || obj.type == "checkbox") {
        $("span", obj.parentNode).css("backgroundColor","#F78181");
        if ($("br", obj.parentNode).length > 0) { // newline checkbox/radio
            o=$("br:first", obj.parentNode);
            o.before("<span class=\"qwarn\"><i></i>"+warning+"<b></b></span>");
            o.prev().fadeIn(500);
        }
        else {
            o=$(":last-child", obj.parentNode);
            o.after("<span class=\"qwarn\"><i></i>"+warning+"<b></b></span>");
            o.next().fadeIn(500);
        }
    }
    else {
        o.css("backgroundColor","#F78181");
        o.after("<span class=\"qwarn\"><i></i>"+warning+"<b></b></span>");
        o.next().fadeIn(500);
    }
}
/**
 * Hides a {@link qwarn}-warning.
 * @param {object} obj triggering form field HTML object
 * @see qwarn
 */
function qunwarn(obj) {
    var o = $(obj);
    if (obj.type == "radio" || obj.type == "checkbox") {
        $("span", obj.parentNode).css("backgroundColor","");
    }
    else {
        o.css("backgroundColor","");
    }
    $(".qwarn", obj.parentNode).fadeOut(500, function() {$(this).remove();});
}

/**
 * adding a new topic to the question-topic-list
 * @param {string} topic name of the topic (must equal the id of the DIV)
 */
function updateTopicList(topicName, topicID) {
    $("#questionlinklist").append("<li>"+topicName+"</li>").click(function() {
        // TODO : debug
        $("#questionlist div").hide();
        var d = $("#"+topicID);
        d.show();
        d.children().show();
        // $("#hints").show();
    });
}

/**
 * list policy rules derived from argument graph as checkboxes
 * @param {object} rules json array of policy rules
 */
function showPolicyRules(rules) {
    
    var policyList = $("#policylist");
    policyrules = [];
    $.each(rules, function(ruleindex, r) {
       policyList.append('<li><input type="checkbox" name="'+r+'" />'+r+'</li>');       
       policyrules.push(r);
    });   
    $("#policyrules").append('<input type="button" class="ui-state-hover ui-button ui-widget ui-state-default ui-corner-all ui-button ui-widget ui-state-hover evaluate" value="Evaluate" onclick="evaluateGraph()"/>');
    
}

/**
 * creating svg representation of an argument graph
 * @param {string} path path to lkif with argument graph
 */
function showArgGraph(path) {
    // set global path to lkif argument graph
    argGraph = path;
    var json = {"showgraph" : path};
    doAJAX(evaluation_url(), json);
}

/**
 * display svg representation of an argument graph
 * @param {string} path path to svg with argument graph
 */
function showSVGGraph(path) {
    var graphBox = $("#graph");
    graphBox.svg();    
    graphBox.svg('get').load(path, onSVGLoad);
    

}

/**
 * function called after svg is loaded; binds mousewheel and drag
 * events to svg for zoom and dragging
 * @param {svgWrapper} the svg wrapper
 */
function onSVGLoad(svgW) {
           
    svgWrapper = svgW;
    
    // reset scale and translate
    svgScale = 1;
    translate="translate(0, 0)";

    var svgroot = document.getElementsByTagName('svg')[0];
    // SVGPan has a problem if there is already a viewBox, so we remove it
    svgroot.removeAttribute('viewBox');
    resetSvgRoot();
    setupSVGHandlers(document.documentElement);
    
    $("#tabs-1").height($("#wrapper").height());
    
   
    
}

/**
 * evaluate argument graph with selected policy rules
 */
function evaluateGraph() {
    // get selected checkboxes / policyrules
    var accArray = [];
    var rejArray = [];
    $.each(policyrules, function(index, r) {
       if($('input[name='+r+']').attr('checked')) {
           accArray.push(r);
       } else {
           rejArray.push(r);
       }
    }); 
    
    // call engine
    var json = {evaluate : {argGraph : argGraph, accept : accArray, reject : rejArray}}
    doAJAX(evaluation_url(), json);
}

function showError(error) {
    // TODO : handle errors
    alert(error);
}

// clean DOM after server response
function resetContent() {
    // clear question nav
    $('#questionlinklist').empty();
    // clear question forms
    $('#questionlist').empty();
    // clear question hints
    $('#hints').empty();
    $('#hints').append('<h4>hints</h4>');
    // go to first tab
    $("#tabs a[href='#tabs-1']").click(); 
}
