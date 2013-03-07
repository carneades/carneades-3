// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

PM.activate = function(itemid) {
    _.map($('#mainmenu a'), function(a) {
              $(a).removeClass('active');
          });
    $(itemid).addClass('active');
};

// merge the object obj with the menu properties
PM.merge_menu_props = function(obj) {
    return $.extend(obj,
                    {pmt_menu_intro: $.i18n.prop('pmt_menu_intro'),
                     pmt_menu_issues: $.i18n.prop('pmt_menu_issues'),
                     pmt_menu_facts: $.i18n.prop('pmt_menu_facts'),
                     pmt_menu_arguments: $.i18n.prop('pmt_menu_arguments'),
                     pmt_menu_schemes: $.i18n.prop('pmt_menu_schemes'),
                     pmt_menu_policies: $.i18n.prop('pmt_menu_policies'),
                     pmt_menu_report: $.i18n.prop('pmt_menu_report')});
};

// merge the object obj with the ag menu properties
PM.merge_ag_menu_props = function(obj) {
    return $.extend(obj,
                    {pmt_ag_menu_copy: $.i18n.prop('pmt_ag_menu_copy'),
                     pmt_ag_menu_export: $.i18n.prop('pmt_ag_menu_export'),
                     pmt_ag_menu_vote: $.i18n.prop('pmt_ag_menu_vote'),
                     pmt_ag_menu_evaluate: $.i18n.prop('pmt_ag_menu_evaluate'),
                     pmt_ag_menu_map: $.i18n.prop('pmt_ag_menu_map')
                     });
};
