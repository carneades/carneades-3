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
    if(IMPACT.facts_state == 'waiting') {
        IMPACT.facts_state = 'entering';
        var facts_html = ich.facts({'pmt_pmt': $.i18n.prop('pmt_pmt'),
                                    pmt_pmt: $.i18n.prop('pmt_pmt'),
                                    pmt_intro: PM.markdown_to_html($.i18n.prop('pmt_intro')),
                                    pmt_start: $.i18n.prop('pmt_start'),
                                    pmt_intro_pmt: $.i18n.prop('pmt_intro_pmt'),
                                    pmt_menu_intro: $.i18n.prop('pmt_menu_intro'),
                                    pmt_menu_issues: $.i18n.prop('pmt_menu_issues'),
                                    pmt_menu_facts: $.i18n.prop('pmt_menu_facts'),
                                    pmt_menu_arguments: $.i18n.prop('pmt_menu_arguments'),
                                    pmt_menu_schemes: $.i18n.prop('pmt_menu_schemes'),
                                    pmt_menu_policies: $.i18n.prop('pmt_menu_policies'), 
                                             });
        $('#pm').html(facts_html.filter("#facts"));
        PM.activate('#facts-item');
        PM.ajax_post(IMPACT.simulation_url, {request: IMPACT.question},
                     PM.show_questions_or_ag,
                     IMPACT.user,
                     IMPACT.password,
                     PM.on_error);    
    } else {
        var facts_html = ich.facts({pmt_intro_pmt: $.i18n.prop('pmt_intro_pmt'),
                                    pmt_menu_intro: $.i18n.prop('pmt_menu_intro'),
                                    pmt_menu_issues: $.i18n.prop('pmt_menu_issues'),
                                    pmt_menu_facts: $.i18n.prop('pmt_menu_facts'),
                                    pmt_menu_arguments: $.i18n.prop('pmt_menu_arguments'),
                                    pmt_menu_schemes: $.i18n.prop('pmt_menu_schemes'),
                                    pmt_menu_policies: $.i18n.prop('pmt_menu_policies'), 
                                   });
        $('#pm').html(facts_html.filter("#facts"));
        $('#pm').append('<div>' + $.i18n.prop('pmt_facts_not_available') + '</div>');
        PM.activate('#facts-item');
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
        IMPACT.facts_state = 'done';
        PM.set_arguments_url(IMPACT.db);

    }
};

// Sends the answers to the server
PM.send_answers = function(questions, on_response) {
    // catb.views.pmt.facts.send_answers(questions, on_response);
    // return false;
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

    // for each question in the category
    var answers_values = _.reduce(questions,
                                 function(answers_values, question) {
                                     var subquestions = $('#q{0}'.format(question.id));
                                     
                                     // for each subquestion
                                     _.reduce(subquestions,
                                              function(answers_values, subquestion) {
                                                  subquestion = $(subquestion);
                                                  var inputs = subquestion.find('.inputfield');
                                                  
                                                  // for each input field
                                                  var vals = [];
                                                  _.reduce(inputs,
                                                           function(index, input) {
                                                               var val = widget_to_val[input.type || 'select']($(input));
                                                               if(val != null) {
                                                                   console.log('input {0} has value {1}'.format(question.id, val));
                                                                   vals.push(val);
                                                               }
                                                               
                                                               return index + 1;
                                                           },
                                                           0);
                                                  
                                                  answers_values.push({id: question.id, values: vals});
                                                  return answers_values;
                                              },
                                              answers_values);

                                     return answers_values;
                                 },
                                  []);

    PM.ajax_post(IMPACT.simulation_url,
                 {answers:  answers_values},
                 on_response,
                 IMPACT.user,
                 IMPACT.password,
                 PM.on_error);
};
