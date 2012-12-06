// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

PM.activate = function(itemid) {
    _.map($('#mainmenu a'), function(a) {
              $(a).removeClass('active');
          });
    $(itemid).addClass('active');
};
