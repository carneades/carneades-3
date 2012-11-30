var casper = require('casper').create();
var fs = require("fs");

console.log('dir = ' + casper.cli.get('dir'));
fs.changeWorkingDirectory(casper.cli.get('dir'));

console.log('url = ' + casper.cli.get('url'));
casper.start(casper.cli.get('url'), function() {
                 this.test.assertEval(function() {
                                          return $('#qq-role-yn input[type=radio]').length == 3;
                                      },
                                      'Y/N question for a role is displayed');
                 
                 this.test.assertEval(function() {
                                          return $('#qq-role select').length == 1;
                                      },
                                      'Select input for a role is displayed');
                 
                 this.test.assertEval(function() {
                                          return $('#qq-concept input[type=radio]').length == 3;
                                      },
                                      'Y/N question for a concept');
                 
                 this.test.assertEval(function() {
                                          return $('#qq-predicate-yn input[type=radio]').length == 3;
                                      },
                                      'Y/N question for a predicate');

                 this.test.assertEval(function() {
                                          return $('#qq-predicate input[type=text]').length == 3;
                                      },
                                      'Text inputs for a predicate are displayed');
                 
             });

casper.run(function() {
               this.test.renderResults(true);
           });