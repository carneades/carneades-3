PM.activate = function(itemid) {
    _.map($('#mainmenu a'), function(a) {
              $(a).removeClass('active');
          });
    $(itemid).addClass('active');
};
