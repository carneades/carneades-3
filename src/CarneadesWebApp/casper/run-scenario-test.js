var casper = require('casper').create({waitTimeout: 6000});

// casper.on("remote.message", function(message) {
//   this.echo("remote console.log: " + message);
// });

console.log('url = ' + casper.cli.get('url'));

var test_timeout = function () {
    take_picture.call(this);
    this.fail('Timeout!');
};

var test_scenario_entry = function() {
    this.test.assertEval(function() {
        return $('body p:first').text().match("is the policy") != null;
    }, 'Introduction page contains some text');


    this.click('#start');

    casper.waitForSelector('#issues', test_scenario_issues);
};

var test_scenario_issues = function() {
    // console.log('New location is ' + this.getCurrentUrl());

    this.test.assertEval(function() {
        return $('body p:first').text().match('the following issues') != null;
    }, 'Issues page contains some text');

    this.click('#submit');

    casper.waitForSelector('#facts', test_scenario_facts);

};

var test_scenario_facts = function() {
    // console.log('New location is ' + this.getCurrentUrl());

    this.test.assertEval(function() {
        return $('body p:first').text().match('be used for commercial') != null;
    }, 'The fact page contains a question about the use of the work');


    this.click('input[type=button]');

    casper.waitForSelector('h3:nth-of-type(1)', test_scenario_facts_license);
    
};

var test_scenario_facts_license = function() {
    this.test.assertEval(function() {
        return $('p:nth(1)').text().match('existing license') != null;
    }, 'The fact page contains a question about the license');

    // selects 'no' for the license question
    casper.evaluate(function(term) {
        document.querySelector('input[name="input-q2-2"][value="no"]').setAttribute('checked', true);
    });

    this.click('input[type=button]');


    casper.waitForSelector('h3:nth-of-type(2)', test_scenario_facts_search);
};

var test_scenario_facts_search = function() {
    this.test.assertEval(function() {
        return $('p:nth(2)').text().match('copyright') != null;
    }, 'The fact page contains a question about the copyright owner');

    this.test.assertEval(function() {
        return $('p:nth(3)').text().match('announcement') != null;
    }, 'The fact page contains a question about an announcement');

    // selects 'yes' for the announcement question
    casper.evaluate(function(term) {
        document.querySelector('input[name="input-q4-5"][value="yes"]').setAttribute('checked', true);
    });

    this.click('input[type=button]');

    casper.waitForSelector('#argumentgraph', test_scenario_outline);
};

var test_scenario_outline = function() {
    this.test.assertEval(function() {
        return $('p:first').text() == "☐ The person may publish the work.";
    }, 'The outline page shows that the main issue is not acceptable');

    this.click('#schemes-item');

    casper.waitForSelector('.theory-view', test_scenario_schemes);
    // casper.then(function() {
    //     this.click('#newstatement');
    // });
    // casper.waitForSelector('#save-statement', test_scenario_newstatement);
};

var test_scenario_schemes = function() {
    this.test.assertEval(function() {
        return $('#outline li:first a').text().match('Argument from') != null;
    }, 'There is a least one scheme on the schemes page named Argument from..');

    this.click('#policies-item');

    casper.waitForSelector('.policies-filtering', test_scenario_policies);
};

var test_scenario_policies = function() {
    this.click('#in');
    this.waitWhileSelector('#UrhG', test_scenario_policies_aktionbundnis);

};

var test_scenario_policies_aktionbundnis = function() {
    this.test.assertEval(function() {
        return $('input[type=submit]').length == 1;
    }, 'Only one policy makes the issue acceptable');

    this.click('#inputQ12-Aktionsbundnis');

    casper.waitForSelector('#argumentgraph', 
                           test_scenario_outline_after_eval,
                           function() {
                               this.fail('Selecting a policy does not work');
                           },
                           10000);
};

var test_scenario_outline_after_eval = function() {
    this.test.assertEval(function() {
        return $('p:first').text() == "☑ The person may publish the work.";
    }, 'The outline page shows that the main issue is now acceptable');

    this.click('#facts-item');

    casper.waitForText('Modify the facts.', test_scenario_change_facts);

};

var test_scenario_change_facts = function() {
    this.click('a[href="#/facts/modify"]');

    casper.waitForText('You can modify the answers below', test_scenario_change_facts_license);
};

var test_scenario_change_facts_license = function() {

    // selects 'yes' for the license question
    casper.evaluate(function(term) {
        document.querySelector('input[name="input-q3-7"][value=yes]').setAttribute('checked', true);
    });

    this.click('input[type=button][value=Submit]');

    casper.waitForSelector('#argumentgraph', 
                           test_scenario_select_urhg,
                           function() {
                               this.fail('Modifying the answer does not work');
                           },
                           10000);
};

var test_scenario_select_urhg = function() {
    this.click('#policies-item');

    casper.waitForSelector('.policies-filtering', test_scenario_select_urhg2);
};


var test_scenario_select_urhg2 = function() {
    this.click('#inputUrhG');

    casper.waitForSelector('#argumentgraph', 
                           test_scenario_copy,
                           function() {
                               this.fail('Applying UrhG policy does not work');
                           },
                           10000)
};

var test_scenario_copy = function () {
    this.click('#copy');

    casper.setFilter("page.confirm", function(msg) {
        return msg === "Make a copy of the current case?" ? true : false;
    });

    casper.waitForText('You are now viewing a copy of the case.', 
                       function () {
                           casper.waitForText('may publish the work', test_scenario_vote);
                       });

};

var test_scenario_vote = function () {
    this.click('.vote');

   casper.waitForText('Given the facts of this case', test_scenario_vote2);
};

var test_scenario_vote2 = function () {
    this.click('.vote-now');

    casper.waitForText('Thank you for your vote!', test_scenario_vote3);

};

var test_scenario_vote3 = function () {
    this.click('.show-vote-results');

    casper.waitForText('Claim', test_scenario_vote4);
};

var test_scenario_vote4 = function () {
    this.test.assertEval(function () {
       return $('tr td:nth-of-type(2)').text() == "100.00%";
    }, 'The vote has been registered');

    this.click('#arguments-item');

    casper.waitForSelector('#argumentgraph', test_scenario_newstatement)
};

var test_scenario_evaluation = function () {
    this.click('.evaluate');

    casper.waitForText('Evaluation finished', function() {
        casper.waitForSelector('#newstatement', test_scenario_newstatement);
    });
};

var test_scenario_newstatement = function () {
    this.click('#newstatement');

    casper.waitForSelector('#save-statement', test_scenario_newstatement2);
};    

var test_scenario_newstatement2 = function () {
    this.test.assertExist('input[name=main][value=yes]');
    
    this.click('input[name=main][value=yes]');
    
    casper.evaluate(function(term) {
        var text_en = "Here some text!";
        var metadata_en = "Here some metadata";

        document.querySelector('#statement-editor-text').value = text_en;  
        document.querySelector('.metadata-description-input').value = metadata_en;
    });
    
    

    this.test.assertEval(function() {
        return document.querySelector('input[name=main]:checked').getAttribute('value') == "yes";
    }, 'Main was set to true');
    
    this.click('#save-statement'); 
    
    casper.waitForText('Here some text!', test_scenario_newstatement3);
    
};

var test_scenario_newstatement3 = function () {
    this.click('#newargument');

    casper.waitForSelector('.save-argument', test_scenario_newargument);    
};

var test_scenario_newargument = function () {
    casper.evaluate(function (term) {
        $('#argument-editor-weight').val(0.6).trigger('change');
        
        $('#argument-editor-scheme').select2("val", "appearance").trigger('change');
    });
    
    casper.waitForSelector('.premise-candidate', test_scenario_newargument2);

};

var test_scenario_newargument2 = function () {
    this.test.assertEval(function () { return PM.statements.length > 5; }
                         , 'There are some statements in the case');
    
    casper.evaluate(function () {
        var conclusion_id = PM.statements.at(2).id;
        console.log('conclusion_id = ' + conclusion_id);
        console.log('selected=');
        
        console.log($('.conclusion-candidate .statement-select'));
        $('.conclusion-candidate .statement-select')
            .select2("val", conclusion_id)
            .trigger('change');
        
        
    });
    
    casper.evaluate(function () {
        var premise_id = PM.statements.at(1).id;
        $('.premise-candidate:first .statement-select')
            .select2("val", premise_id)
            .trigger('change');
    });
    
    casper.waitFor(function () {
        return this.evaluate(function () {
            return $('.conclusion-candidate .statement-select').select2("val") != "" 
                &&  $('.premise-candidate:first .statement-select').select2("val") != "";
        });
    });
    
    this.click('.save-argument'); 
    
    casper.waitWhileSelector('.save-argument', function () {
        casper.waitForText('Premises', test_scenario_newargument3);
    });
    
};

var test_scenario_newargument3 = function () {
    this.test.assertTextExists('minor', 'There is a minor text');
    this.test.assertTextExists('0.6', 'There is a 0.6 text');
};

// TODO test reading the general map
// TODO export
// TODO test report

var take_picture = function (filename) {

    this.capture(filename, {
        top: 0,
        left: 0,
        width: 1024,
        height: 1600
    });
};

casper.start(casper.cli.get('url'), function() {
    casper.waitForSelector('#mainmenu', function() {
        test_scenario_entry.call(this);
    });
});

casper.run(function() {
               this.test.renderResults(true);
           });
