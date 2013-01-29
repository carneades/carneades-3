// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

// Returns the relative url of the facts page
PM.facts_url = function() {
    return 'facts';
};

// Sets the current URL to the facts URL.
// This will cause the facts page to be displayed.
PM.set_facts_url = function() {
  $.address.value(PM.facts_url());
};

// Displays the questions
PM.display_facts = function() {
    if(IMPACT.db == undefined) {
        var facts_html = ich.facts(PM.merge_menu_props({}));
        $('#pm').html(facts_html.filter("#facts"));
        PM.activate('#facts-item');
        PM.attach_lang_listener();
        
        PM.ajax_post(IMPACT.simulation_url, {request: IMPACT.question},
                     PM.show_questions_or_ag,
                     IMPACT.user,
                     IMPACT.password,
                     PM.on_error);    
    } else {
        var facts_html = ich.ask_modify_facts(PM.merge_menu_props(
            {'pmt_facts_already_submitted': $.i18n.prop('pmt_facts_already_submitted'),
             'pmt_modify_facts': $.i18n.prop('pmt_modify_facts')}));
        
        $('#pm').html(facts_html.filter("#facts"));
        PM.activate('#facts-item');
        PM.attach_lang_listener();
    }
};

// Shows the remaining questions to the user or the argument graph if
// all questions have been answered.
PM.show_questions_or_ag = function(data) {
    if (data.questions) {
        catb.views.pmt.questions.show_questions(
            data.questions, 
            $('#questions'),
            function() {
                if($('#questionsform').valid()) {
                    PM.send_answers(data.questions,
                                    PM.show_questions_or_ag);
                } 
            });
    } else {
        IMPACT.db = data.db;
        PM.set_arguments_url(IMPACT.db);

    }
};

PM.collect_answer = function(qid) {
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

    var inputs = $('#q{0} .inputfield'.format(qid));
    
    // for each input field
    var vals = [];
    _.reduce(inputs,
             function(index, input) {
                 var val = widget_to_val[input.type || 'select']($(input));
                 if(val != null) {
                     console.log('input {0} has value {1}'.format(qid, val));
                     vals.push(val);
                 }
                 
                 return index + 1;
             },
             0);

    return vals;
};

PM.collect_answers = function(questions) {
    return _.reduce(questions,
                    function(answers_values, question) {
                        var vals = PM.collect_answer(question.id);
                        answers_values.push({id: question.id, values: vals});
                        return answers_values;
                    },
                    []);
};

// Sends the answers to the server
PM.send_answers = function(questions, on_response) {
    // catb.views.pmt.facts.send_answers(questions, on_response);
    // return false;
    console.log('send_answers');
    console.log(questions);
    
    // for each question in the category
    var answers_values = PM.collect_answers(questions);

    PM.busy_cursor_on();
    PM.ajax_post(IMPACT.simulation_url,
                 {answers:  answers_values},
                 function(data) {
                     PM.busy_cursor_off();
                     on_response(data);
                 },
                 IMPACT.user,
                 IMPACT.password,
                 PM.on_error);
};
