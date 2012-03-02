
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

function impact_show_questions_or_answer(data) {
    if (data.questions) {
        impact_show_questions(data, '#questionslistbox');
    } else if (data) {
        show_solution(data, true);
    }
}

function impact_show_questions(data, questions_selector)
{
    var questions = data.questions;
    var qlist = $(questions_selector);
    var category_name = questions[0].category_name;
    var category =  questions[0].category;

    $("#tabs a[href='#tabs-2']").click();        // to remove
    qlist.empty();
    qlist.append('Category = {0}'.format(questions[0].category_name));
    qlist.append('<br/>');
    _.map(questions, function(q) { impact_show_question(q, qlist); });
    
    var button_id = gen_id();
    qlist.append('<input type="button" value="next" id="{0}"/>'.format(button_id));
    
    $('#' + button_id).click(function() { impact_send_answers(questions); });
}

function impact_send_answers(questions)
{
    var widget_to_val = {
        text: function(input) {
            return input.val();
        },
        radio: function(input) {
            if(input.is(':checked')) {
                return input.val();
            }
            return null;
        },
        select: function(input) {
            if(input.is(':checked')) {
                return input.val();
            }
            return null;
        }
    };

    var inputs = $(".inputfield");
    var answers_values = _.reduce(inputs,
                                  function(answers_values, html_input) {
                                    var input = $(html_input);
                                    var id = input.attr('id').substr(1);
                                    var question = _.filter(questions, function(q) {return q.id = id;})[0];
                                    var value = widget_to_val[question.widget](input);
                                    answers_values.push({id: id, value: value});
                                    return answers_values;
                                },
                                []);
    
    // filter out non selected values
    answers_values = _.filter(answers_values, function(x) { return x.value !== null; });

    send_data(simulation_url(), {answers:  {values: answers_values}}, impact_show_questions_or_answer);
}

function impact_show_question(question, qlist)
{
    // by convention the id of the input for the question N is qN
    var widget_to_html = {
        text: function(id, proposed_answers, formal_answers) {
            return '<input class="inputfield" type="text" id="q{0}" />'.format(id);
        },
        radio: function(id, proposed_answers, formal_answers) {
            var html = "";
            _.each(formal_answers, function(formal_answer, index) {
                       html += '<input id="q{0}" class="inputfield radiofield" name="inputq{0}" value="{1}" type="radio" />{2} '
                           .format(id, formal_answer, proposed_answers[index]);
                   });
            return html;
        },
        select: function(id, proposed_answers, formal_answers) {
            var html = '<select>';
            
            _.each(formal_answers, function(formal_answer, index) {
                       html += '<option id="q{0}" class="inputfield" value="{1}">{2}</option>'
                           .format(id, formal_answer, proposed_answers[index]);
                   });
            html += '</select>';
            return html;
        }
    };

    qlist.append('Question text = {0} '.format(question.question));
    qlist.append(widget_to_html[question.widget](question.id, question.answers, question.formalanswers));
    qlist.append('<br/>');
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
    if (item.widget == "select") {
        output += "<select class=\"required\" id=\"qID"+item.id+"\" name=\"qID"+item.id+"\">";

        $.each(item.answers, function(answindex, answer) {
            output += "<option value=\""+item.formalanswers[answindex]+"\">"+answer+"</option>";
        });
        output += "</select>";
    } else if (item.widget == "radio" || item.widget == "checkbox") {
        newline = radio_check_newline(item.answers);
        for(var i = 0; i < item.answers.length; i++) {
            if (newline) output += "<br/>";
            var answer = item.answers[i];
            var formalanswer = item.formalanswers[i];
            output += "<input class=\"required\" name=\"qID"+item.id+"\" type=\""+item.widget+"\" value=\""+formalanswer+"\">";
            output += "<span onclick=\"$(this).prev().click()\">"+answer+"</span>";
        }
    } else if (item.widget == "date") {
        output += "<input type=\"text\" class=\"datefield required\" id=\"qID"+item.id+"\" name=\"qID"+item.id+"\""+((item.answers && item.answers[0]!="") ? " value=\""+item.answers[0]+"\"" : "")+"/>";
    } else if (item.widget == "int") {
        output += "<input type=\"text\" class=\"integer required\" id=\"qID"+item.id+"\" name=\"qID"+item.id+"\""+((item.answers && item.answers[0]!="") ? " value=\""+item.answers[0]+"\"" : "")+"/>";
    } else if (item.widget == "float") {
        output += "<input type=\"text\" class=\"float required\" id=\"qID"+item.id+"\" name=\"qID"+item.id+"\""+((item.answers && item.answers[0]!="") ? " value=\""+item.answers[0]+"\"" : "")+"/>";
    } else {
        output += "<input class=\"required\" type=\""+item.widget+"\" id=\"qID"+item.id+"\" name=\"qID"+item.id+"\""+
            ((item.answers && item.answers.length != 0) ? " value=\""+item.answers[0]+"\"" : "")+"/>";
    }
    
    output += "</p>";
    qbox.append(output);

    if (item.hint) $("#hints").append("<p id=\"qHINT"+item.id+"\" class=\"hint\">"+item.hint+"</p>");
    // post append formatting
    if (item.optional) $("p:last :input:first", qbox).addClass("optional");
    // focus and blur does not work on radio/checkbox
    if (item.widget != "radio" && item.widget != "checkbox") {
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
 * Validates the formfield content.
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
