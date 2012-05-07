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
                                    var id = input.attr('id').substr(2);
                                    var question = _.filter(questions, function(q) {return q.id = id;})[0];
                                    var value = widget_to_val[question.widget](input);
                                    answers_values.push({id: id, value: value});
                                    return answers_values;
                                },
                                []);
    
    // filter out non selected values
    answers_values = _.filter(answers_values, function(x) { return x.value !== null; });

    PM.ajax_post(IMPACT.simulation_url, {answers:  {values: answers_values}}, on_response);
};