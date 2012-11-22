// Adds the new questions to the end of the current page
PM.show_questions = function(questions, questionlist, on_submit) {
    catb.views.pmt.questions.show_questions(questions, questionlist, on_submit);
    return false;
};

// Adds some input elements when the '+' button is clicked
PM.add_fields = function(question) {
    console.log('+PLUS+');
    console.log(question);
    var question_html = PM.get_question_html(question);
    $('#q' + question.id + ' .plus').last().remove();
    $('#q' + question.id).append('<br>' + question_html);
    $('#q' + question.id + ' .plus').click(_.bind(PM.add_fields, PM, question));

    return false;
};

// Returns the HTML code of a widget used for responding to a question
PM.get_question_widget = function(question, index) {
    // by convention the id of the input for the question N is iqN
    var widget_to_html = {
        text: function(id, proposed_answers, formal_answers) {
            var name_id = UTILS.gen_id();
            return '<input class="inputfield required" type="text" name="name{0}" />'.format(name_id, id);
        },
        radio: function(id, proposed_answers, formal_answers) {
            var html = "";
            _.each(formal_answers, function(formal_answer, index) {
                       html += '<input id="iq{0}" class="radiobutton inputfield required" name="inputq{0}" value="{1}" type="radio" />{2} '
                           .format(id, formal_answer, proposed_answers[index]);
                   });
            return html;
        },
        select: function(id, proposed_answers, formal_answers) {
            var html = '<select class="combobox required">';
            
            _.each(formal_answers, function(formal_answer, index) {
                       html += '<option id="iq{0}" class="dropdown-menu inputfield" value="{1}">{2}</option>'
                           .format(id, formal_answer, proposed_answers[index]);
                   });
            html += '</select>';
            return html;
        }
    };

    return widget_to_html[question.widgets[index]](question.id, question.answers, question.formalanswers);
};

// Adds one question to current page
PM.show_question = function(question, questionlist) {
    questionlist.append('<p><i>{0}</i></p>'.format(question.hint == null ? "" : question.hint));

    var question_html = PM.get_question_html(question);
    questionlist.append('<div id="q{0}" class="question">{1}</div>'.format(question.id, question_html));
    questionlist.append('<br/>');
};

// Returns the HTML code to answer a question
PM.get_question_html = function(question) {
    var variable = /\?[a-zA-Z_0-9-]+/g;

    var html = "";
    if(question.yesnoquestion) {
        html = question.question + PM.get_question_widget(question, 0);
    } else {
        var index = -1;
        html = question.question.replace(variable,
                                         function() {
                                             index++;
                                             console.log('get_question_html ' + index);
                                             return PM.get_question_widget(question, index);
                                         });
        
    }
    
    return html;
};