PM.facts_url = function() {
    return 'facts';
};

PM.set_facts_url = function() {
  $.address.value(PM.facts_url());
};

PM.display_facts = function() {
    var facts_html = ich.facts();
    $('#pm').html(facts_html.filter("#facts"));
    PM.activate('#facts-item');
    PM.ajax_post(IMPACT.simulation_url, {request: IMPACT.question}, PM.show_questions_or_ag);
};

PM.show_questions_or_ag = function(data) {
        if (data.questions) {
            PM.show_questions(data.questions, $('#questions'), function() {
                                  PM.send_answers(data.questions, PM.show_questions_or_ag);
                              });
        } else {
            IMPACT.db = data.db;
            PM.set_arguments_url();
        }
};

PM.send_answers = function(questions, on_response) {
    console.log('send_answers');
    console.log(questions);
    
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

    var questions_html = _.map(questions, function(q) { return $('#q{0}'.format(q.id)); });
    var answers_values = _.reduce(questions_html,
                                 function(answers_values, question_html) {
                                     var inputs = question_html.find('.inputfield');
                                     var id = parseInt($(question_html).attr('id').substr(1), 10);
                                     var question = _.filter(questions, function(q) { return q.id = id;})[0];
                                     var values = _.map(inputs, function(input) { return widget_to_val[question.widget]($(input)); });
                                     values = _.filter(values, function(e) { return e != null; });
                                     console.log('input {0} has values {1}'.format(id, values));
                                     answers_values.push({id: id, values: values});
                                     return answers_values;
                                 },
                                  []);

    PM.ajax_post(IMPACT.simulation_url, {answers:  {values: answers_values}}, on_response);
};