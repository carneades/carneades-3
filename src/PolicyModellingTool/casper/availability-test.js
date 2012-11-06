if (phantom.args.length != 4) {
    console.log('Expected a filename parameter.');
    phantom.exit(1);
}

var filename = phantom.args[3];

console.log('Saving XML Tests to file ' + filename);

var casper = require('casper').create();

casper.start('http://localhost:8080/policymodellingtool/#/introduction');

casper.waitForSelector('#pm', 
                       function() {
                           this.test.assertTitleMatch(/Policy Modeling Tool/, 'Title is correct');
    });

casper.run(function() {
               this.test.renderResults(true, 0, filename);
           });

casper.run();