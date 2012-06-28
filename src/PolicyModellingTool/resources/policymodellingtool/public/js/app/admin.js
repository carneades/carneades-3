PM.admin_url = function() {
    return 'admin';
};

PM.set_admin_url = function() {
    $.address.value(PM.admin_url());
};

PM.display_admin = function() {
    var admin_html = ich.admin();
    PM.ajax_post(IMPACT.simulation_url, {"current-policy": null},
                 function(currentpolicy) {
                     PM.ajax_get(IMPACT.wsurl + "/policies",
                                 function(policies) {
                                     $('#pm').html(admin_html.filter("#admin"));
                                     var content = "";
                                     _.each(Object.keys(policies).sort(), function(key) {
                                             var val = policies[key];
                                             content += '<option value="{0}">{1}</option>'.format(key, val.header.title);});
                                     $('#policies').html(content);
                                     $('#policies').val(currentpolicy);
                                     $('#submit').click(PM.on_admin_save);
                                 },
                                PM.on_error);
                 },
                 IMPACT.user,
                 IMPACT.password,
                 PM.on_error);
};

PM.on_admin_save = function() {
    var policy = $('#policies').val();
    PM.ajax_post(IMPACT.simulation_url, {"set-current-policy": policy},
                 function(res) { console.log('setting current-policy: ' + policy); },
                 IMPACT.user,
                 IMPACT.password,
                 PM.on_error);
    return false;
};