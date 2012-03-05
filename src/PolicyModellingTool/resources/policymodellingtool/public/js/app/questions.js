PM.show_questions = function(questions, questionlist, on_submit) {
    questionlist.append('<h2>{0}</h2>'.format(questions[0].category_name));
    _.map(questions, function(q) { PM.show_question(q, questionlist); });
    
    var button_id = UTILS.gen_id();
    questionlist.append('<input type="button" value="submit" id="submit{0}"/>'.format(button_id));
    questionlist.append('<hr/>');
    $('#submit' + button_id).click(on_submit);
};

PM.show_question = function(question, questionlist) {
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

    questionlist.append('<p>{0}</p>'.format(question.hint == null ? "" : question.hint));
    questionlist.append(question.question);
    questionlist.append(widget_to_html[question.widget](question.id, question.answers, question.formalanswers));
    questionlist.append('<br/>');
};
