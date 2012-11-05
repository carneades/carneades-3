if (phantom.args.length != 4) {
    console.log('Expected a target URL parameter.');
    phantom.exit(1);
}

var fs = require("fs");
console.log('dir = ' + fs.workingDirectory);

var url = phantom.args[3];

console.log('url = ' + url);
// console.log(phantom.args);

var casper = require('casper').create();

casper.start(url);

casper.then(
    function() {
       this.test.assertEval(function() {
                                return catb.test.navigation.run();
                            }, 'Navigation is working');
    });

casper.run(function() {
               this.test.renderResults(true);
           });

casper.run();