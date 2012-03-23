// This object contains the global variables for the app
var IMPACT = {
    db: "",
    lang: "en",
    impactws_url: "/impactws",
    argumentbrowser_url: "/argumentbrowser",
    simulation_url: "/policymodellingtool/PolicySimulation",
    evaluation_url: "/policymodellingtool/PolicyEvaluation"
};

// This object contains the functions and acts as a kind of namespace.
// We don't put variables in it since mixing data and functions with objects
// is too often a bad idea
var PM = {
   
};

PM.url_changed = function(url) {
    if(url.value == "/") {
         return;
    }

    var url_regex = /\/([^ \/]+)(?:\/([^ \/]+))?/;
    var result = url_regex.exec(url.value);
    if(result != null) {
        var element = result[1];
        var section = result[2];
        
        PM.dispatch_url(element, section);
    }
};

PM.dispatch_url = function(element, section) {
    if(element == "introduction") {
        PM.display_introduction();
    } else if(element == "issues") {
        PM.display_issues();
    } else if(element == "facts") {
        PM.display_facts();
    } else if(element == "arguments") {
        PM.display_arguments();
    } else if(element == "policies") {
        PM.display_policies(section);
    }
};

// This code is executed when the page is loaded
$(function() {
     $.address.change(PM.url_changed);
      
      initPM();
  });

function initPM() {
     PM.display_introduction();
}
