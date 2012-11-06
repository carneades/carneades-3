var casper = require('casper').create();

casper.start('http://localhost:8080/policymodellingtool/#/introduction');

casper.waitForSelector('#pm', 
                       function() {
                           this.test.assertTitleMatch(/Policy Modeling Tool/, 'Title is correct');
    });

casper.run(function() {
               this.test.renderResults(true, 0, this.cli.get('save') || false);
           });

casper.run();