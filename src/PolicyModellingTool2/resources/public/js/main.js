/**
 * Client implementation for the IMPACT web application.
 *
 */

var IMPACT = {
    jsloadstarted :new Date(),
    showhints : true,
    arg_graph : "undefined",
    policyrules : [],
    lang_changed : false,
    current_questions : [],
    displayed_categories : [],
    debug : true
};

// adding JSON parser when browser is too old to have a build-in one (pre-IE8, pre-FF3.5, ...)
if( typeof( window[ 'JSON' ] ) == "undefined" ) document.write('<script type="text/javascript" src="https://github.com/douglascrockford/JSON-js/raw/master/json2.js"/>');

// Javascript EPO!
Array.prototype.copy = function () {
    return ((new Array()).concat(this));
};

String.prototype.format = function() {
  var args = arguments;
  return this.replace(/{(\d+)}/g, function(match, number) { 
    return typeof args[number] != 'undefined'
      ? args[number]
      : match
    ;
  });
};


function translation_url() {
    return IMPACT.debug ? "/Translation" : "/PolicyModellingTool2/Translation";
}

function simulation_url() {
    return IMPACT.debug ? "/PolicySimulation" : "/PolicyModellingTool2/PolicySimulation";
}

function evaluation_url() {
    return IMPACT.debug ? "/PolicyEvaluation" : "/PolicyModellingTool2/PolicyEvaluation";
}

// when the document is ready, executes this code:
$(function(){
    // enforces the choice of a topic
    $('#nextTopic').click(function () {
        $('#chooseTopic').validate();
        if($('#chooseTopic').valid()) {
            load_topic($('#topic').val());
            return true;
        } else {
            show_error_status('Please select a topic');
            return false;
        }
    });

    $('#abductionButton').click(function () {
        send_abduction_request();
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
            yearRange: (''+(IMPACT.jsloadstarted.getFullYear()-100)+':'+(IMPACT.jsloadstarted.getFullYear()+10)),
            showAnim: 'slideDown'
    });

     //button
     $(".ui-button").button();

    $("#locate").change(
        function() 
        {
            // if the language is changed
            var lang = $(this).val();

            // informs the server
            send_data(translation_url(), {"language" : lang}, function(data) {});
            
            var label = $('#chooseTopicLabel').text();
            var topicLabel = $('#topicLabel').text();

            // translates labels
            translate(["choose_topic", "topics", "hints", "questions", "solution", "next" ,
                       "orphan_works"], lang,
                      function(translations) {
                          $('#chooseTopicLabel').text(translations[0]);
                          $('#topicLabel').text(translations[1]);
                          $('#topicTabLabel').text(translations[1]);
                          $('#hintsLabel').text(translations[2]);
                          $('#questionsTabLabel').text(translations[3]);
                          $('#solutionTabLabel').text(translations[4]);
                          $('#nextTopic').val(translations[5]);
                          $('#topicName').html(translations[6]);
                      });
            
            // translates all categories
            $.each(IMPACT.displayed_categories, translate_category);

            // translates current question
            if(IMPACT.current_questions.length > 0) {
                var top_question = IMPACT.current_questions[0];
                var current_category = top_question.category;

                send_data(simulation_url(),
                          {retrieve_question : {id : top_question.id,
                                                statement : top_question.statement}},
                          function (data) { show_questions(data, false, true); });
            }

            IMPACT.lang_changed = true;
        });

    // Fragen-Liste
    $("li", $("#questionlist")).each(function(index){            
            var li_i = index;
            $(this).click(function(){
                    $(document.getElementsByTagName("h3")[li_i].firstChild).click();
            });
    });

    /** AJAX request config */
      $.ajaxSetup({url: simulation_url(),
                   async: true,
                   beforeSend: function() {
                       update_status(0,"Please be patient.");
                   },
                   complete: function(XMLHttpRequest, textStatus) {
                       if (textStatus == "success")
                           $("#status").fadeOut();
                       else if(textStatus == "error")
                       update_status(1,XMLHttpRequest.status+" "+textStatus);
                       else // "notmodified", "timeout", or "parsererror"
                           update_status(1,textStatus);
                   },
                   timeout : 600000,       
                   type: "POST"
                  });

    send_data(translation_url(), {get_available_languages : null}, show_available_languages);
});

/**
 * Translate the category name under which are grouped a set of questions
 */
function translate_category(index)
{
    var cat = IMPACT.displayed_categories[index];
    send_data(simulation_url(),
              {retrieve_question : {id : cat.first_question.id,
                                    statement : cat.first_question.statement}},
              function (data) { 
                  var categoryName = data.questions[0].category_name;
                  $('#categories :nth-child({0})'.format(index + 1)).text(categoryName);
              });
}

/**
 * loads questions for requested topic
 * @param {string} t name of the topic
 */
function load_topic(t) {
    send_data(simulation_url(), 
              {"request" : t}, 
              function(data) { show_questions(data, true, false); });
}

function show_available_languages(data) {
    var languages = data.available_languages;
    
    $('#locate').empty();
    for(var name in languages) {
        $('#locate').append('<option value="{0}">{1}</option>'.format(languages[name], name));
    }

    // assumes English is always there and set it as default language
    $('#locate').val('en');

}

function show_position(data) {
    var position = data.position;
    var stmts_ids = data.stmts_ids;
    
    show_policy(position[0], stmts_ids);
    // TODO: show all policies
}

function show_policy(policy, stmts_ids) {
    
    var stmt = policy[1];
    var policyid = stmts_ids["(valid " + stmt + ")"];
    var g = $('g [id="' + policyid + '"]');
    var rect = $('g [id="' + policyid + '"] rect');

    var x = - 4;
    var y = - 4;
    var h = parseInt(rect.attr('height'), 10) + 8;
    var w = parseInt(rect.attr('width'), 10) + 8;

    $('svg').svg();
    var svg = $('svg').svg('get');
    var corner = 5;
    svg.rect(g, x, y, w, h, corner, corner, {"stroke-width" : 2, stroke : "purple", fill : "transparent"});
}

function gen_id() {
    var newDate = new Date;
    return newDate.getTime();
}

function show_questions(data, is_first_display, lang_changed) {
    // {"questions":[{"answers":[],"formalanswers":null,"id":1,"category":"Name","optional":false,"hint":"Please enter the name of the rights owner.","type":"text","question":"What is the name of the right owner?","statement":["hatName","?Person"]},
                     // {"answers":[],"formalanswers":null,"id":2,"category":"Work","optional":false,"hint":null,"type":"text","question":"What is the name of the concerned orphaned work?","statement":["betrifftWerk","?x"]} ]}
    
    IMPACT.current_questions = data.questions;
    
    $("#tabs a[href='#tabs-2']").click();        
    var category_name = IMPACT.current_questions[0].category_name;
    var category = IMPACT.current_questions[0].category;

    if(is_first_display) {
        IMPACT.displayed_categories.push({category : category,
                                          first_question : IMPACT.current_questions[0]});
        
    }

    var qlist = $("#questionlist");

    // remove previous title if it exits (for instance after asking a change in the language)
    $('#questionlist').empty();
    // add new div to questionlist with id = topic name
    // TODO: what is this qcontent id use for?
    qlist.append('<div id="{0}"><h3>{1}</h3><div id="qcontent"></div></div>'.format(category, category_name));
    var qdiv = $("#"+category, qlist);
    var qbox = $("#qcontent", qdiv);
    // for each question
    $.each(IMPACT.current_questions, function(i, item) {
        show_question(item, qbox);
    });

    
    // qbox.append('<input type="button" class="ui-button next" value="next" onclick="send_answers(\''+category+'\')"/>');
    var buttonId = gen_id();
    qbox.append('<input type="button" id="' + buttonId+ '" class="ui-button ui-widget ui-state-default ui-corner-all" value="next" />');
    translate(["next"], $('#locate').val(),
              function(translations) {
                  $('#' + buttonId).val(translations[0]);
    });

    $('#' + buttonId).click(function () {
        $('#questions').validate();
        if($('#questions').valid()) {
            send_answers(category);
        } else {
            show_error_status('Some fields are not filled');
        }
    });

    $('.datefield', qbox).datepicker();

    if(!lang_changed) {
        add_category(category_name, category, IMPACT.current_questions[0]);        
    }

}

/**
 * Displays a question depending on type
 * @param {object} question json object representing the question
 * @param {object} qcontent div inside topic div
 */
function show_question(item, qbox){
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
        newline = radio_check_newline(item.answers);
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
            if (IMPACT.showhints) {
                $("#hints p").css('display','none');
                $("#qHINT"+this.id.substring(3)).show();
            }
        });
        $(":input:last", qbox).blur(function(){
            $("#qHINT"+this.id.substring(3)).css('display','none');
        });
        // validation
        $(":input:last", qbox).change(function(){
            validate_field(this);
        });
    }
    else { // radios & checkboxes
        $("input:last", qbox).parent().mouseover(function(){
            if (IMPACT.showhints) {
                var hinton=$("#hints > p:not(:hidden)");
                // update_status(1,"Verstecke: "+((hinton.length > 0)?"#qID"+hinton.attr("id").substring(5):"-")+" | Zeige: "+"#qHINT"+$(this).children("input:first").attr("name").substring(3));
                if (hinton.length > 0) $("#qID"+hinton.attr("id").substring(5)).blur();
                $("#qHINT"+$(this).children("input:first").attr("name").substring(3)).show();
            }
        });
        $("input:last", qbox).parent().mouseout(function(){
            $("#qHINT"+$(this).children("input:first").attr("name").substring(3)).css('display','none');
        });
        // validation
        $("input[name='qID"+item.id+"']", qbox).change(function(){
            validate_field($("input:first", this.parentNoded)[0]);
        });
    }
}

/**
 * Displays the solution
 * @param {string} solution string representing the main issue
 * @param {string} path url pointing to solution lkif
 */
function show_solution(data) {
    var solution = data.solution;
    var path = data.path;
    
    // go to next tab
    $("#tabs a[href='#tabs-3']").click(); 
    // display solution statement
    $("#solutionstatement").append(solution);

    send_data(evaluation_url(), {"policyrules" : path}, show_policyrules);
    // display argument graph
    show_arg_graph(path);
}

/**
 * Vaildates the formfield content.
 * @param {object} obj a question formfield or a array or DIV-element that contains this HTML nodes
 * @returns returns if a form-field or a set of fields has passed validation
 * @type Boolean
 * @see qwarn
 * @see qunwarn
 * @see send_answers
 */
function validate_field(obj) {
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
            if ( !validate_field(elem) ) {
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
 * @returns true when a new line is required
 * @type Boolean
 */
function radio_check_newline(answers) {
    var newline=false;
    for (var i=0; i < answers.length; i++) {
        if (answers[i].length > 12 || i > 4) {
            newline=true;
            break;
        }
    }
    return newline;
}

function show_error_status(text) {
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
function update_status(type, text) {
    var icon="";
    if (type == 0) { // Loading
            $("#status").removeClass("ui-state-error");
            $("#status").addClass("ui-state-highlight");
            icon='<p><span class="ui-icon ui-icon-info" style="float: left; margin-right: 0.3em;"></span> <strong>Loading:</strong> ';
    }
    else if (type == 1) { // Alert
        show_error_status(text);
    }
    else if (type == -1) {$("#status").hide();return false;}
    else {$("#status").hide();return false;}
    $("#status").html(icon+text+"</p>");
    $("#status").show();
}


/**
 * Collects the given answers and parse them as JSON before sending them to the server.
 * @param {string} category the ID of the question div
 * @see validate_field
 */
function send_answers(category) {
    var doRequest = true;
    var jsonA = new Array();
    var topicDiv = $("#"+category);
    topicDiv.hide();
    $("input", topicDiv).each(function(i, itemobj){
        var item = $(itemobj);
        // skip buttons
        if (item.hasClass("ui-button") || itemobj.type && (itemobj.type == "button" || itemobj.type == "submit" || itemobj.type == "reset") ) return true;
        
        if (validate_field(itemobj) == false) {
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
                };
                jsonA.push(jsonitem);
            }            
        }
        else {            
            var jsonitem = {
                "id" : item.attr("name").substring(3),
                "value" : item.val()
            };
            jsonA.push(jsonitem);
        }
        // TODO: check what the return value if used for                                  
        return undefined;
    });

    $("select", topicDiv).each(function(i, itemobj){
        var item = $(itemobj);
        var jsonitem = {
            "id" : item.attr("name").substring(3),
            "value" : item.val()
        };
        jsonA.push(jsonitem);
    });

    IMPACT.lang_changed = false;

    if (doRequest) {
        send_data(simulation_url(), {"answers" : { values : jsonA,
                                                   questions : IMPACT.current_questions}}, 
                                     show_questions_or_answer);
    }
}

function show_questions_or_answer(data) {
    if (data.questions) {
        show_questions(data, true);
    } else if (data) {
        show_solution(data, true);
    }
}

function send_abduction_request() {
    send_data(evaluation_url(),
              {abduction : {argGraph : IMPACT.arg_graph, acceptability : $('input[name=abduction]').val()}},
              show_position);
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

function add_category(category_name, category, first_question) {
    var category_html_id = category + 'Category';
    $("#categories")
        .append('<li id="' + category_html_id + '">'  + category_name + '</li>');

    $('#' + category_html_id).click(
        function () {
            // retrieves question of the clicked category and displays them
            send_data(simulation_url(),
                      {retrieve_question : {id : first_question.id,
                                            statement : first_question.statement}},
                      function (data) { show_questions(data, false, true); });
        }); 
}

/**
 * list policy rules derived from argument graph as checkboxes
 * @param {object} rules json array of policy rules
 */
function show_policyrules(data) {
    var rules = data.policyrules;
    var policyList = $("#policylist");
    IMPACT.policyrules = [];
    $.each(rules, function(ruleindex, r) {
       policyList.append('<li><input type="checkbox" name="'+r+'" />'+r+'</li>');       
       IMPACT.policyrules.push(r);
    });   
    $("#policyrules").append('<input type="button" class="ui-state-hover ui-button ui-widget ui-state-default ui-corner-all ui-button ui-widget ui-state-hover evaluate" value="Evaluate" onclick="evaluate_graph()"/>');
    
}

/**
 * creating svg representation of an argument graph
 * @param {string} path path to lkif with argument graph
 */
function show_arg_graph(path) {
    // set global path to lkif argument graph
    IMPACT.arg_graph = path;
    send_data(evaluation_url(), {"showgraph" : path}, show_svg_graph);
}

/**
 * display svg representation of an argument graph
 * @param {string} path path to svg with argument graph
 */
function show_svg_graph(data) {
    var path = data.graphpath;
    var graphBox = $("#graph");
    graphBox.svg();    
    graphBox.svg('get').load(path, on_svg_load);
}

function on_svg_load(svgW) {
           
    svgWrapper = svgW;

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
function evaluate_graph() {
    // get selected checkboxes / policyrules
    var accArray = [];
    var rejArray = [];
    $.each(IMPACT.policyrules, function(index, r) {
       if($('input[name='+r+']').attr('checked')) {
           accArray.push(r);
       } else {
           rejArray.push(r);
       }
    }); 
    
    send_data(evaluation_url(),
              {evaluate : {argGraph : IMPACT.arg_graph, accept : accArray, reject : rejArray}},
              function (data) { show_arg_graph(data.evaluated); });
}

function show_error(error) {
    // TODO : handle errors
    alert(error);
}

// clean DOM after server response
function reset_content() {
    // clear question nav
    $('#categories').empty();
    // clear question forms
    $('#questionlist').empty();
    // clear question hints
    $('#hints').empty();
    $('#hints').append('<h4>hints</h4>');
    // go to first tab
    $("#tabs a[href='#tabs-1']").click(); 
}

// translate with the data from i18n-data.js, if the
// data is not found it asks the translation service
function translate(keys, lang, callback) {
    var translated = [];
    var missing_translations = [];
    
    for(var i = 0; i < keys.length; i++) {
        if(i18n[keys[i]] == undefined || i18n[keys[i]][lang] == undefined) {
            missing_translations.push(i18n[keys[i]]["en"]);
        } else {
            translated[i] = i18n[keys[i]][lang];
        }
    }

    if(missing_translations.length == 0) {
        callback(translated);
    } else {
        send_data(translation_url(), {"translate" : {"text" : missing_translations, "from" : "en", "to" : lang}},
                  function(data) {
                      var translations = data.translations;
                      var idx = 0;
                      // complete translated array with the translations from the service
                      for(var i = 0; i < keys.length; i++) {
                          if(translated[i] == undefined) {
                              translated[i] = translations[idx];
                              idx++;
                          }
                      }
                      callback(translated);
                  });
    }
}


