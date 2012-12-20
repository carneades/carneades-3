// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

AGB.map_url = function(db)
{
    return '/map/' + db;
};

AGB.add_map_to_browser_listener = function(db, domelement)
{
    var g = $(domelement);
    if(domelement.id) {
        if(domelement.id[0] == 's') {
            g.click(function() {
                        AGB.set_statement_url(db, domelement.id.slice(2));
                    });
            g.mouseover(function () {
	                    $(this).css('cursor', 'hand');
                        });
        } else if(domelement.id[0] == 'a') {
            g.click(function() {
                        AGB.set_argument_url(db, domelement.id.slice(2));
                    });
            g.mouseover(function () {
	                    $(this).css('cursor', 'hand');
                        });
        }
    }
};

AGB.traverse_map = function(visitor)
{
    $('g[id]').each(
           function(index, domelement) {
               visitor(domelement);
           });
};

AGB.display_map = function(db)
{
    $('#browser').html('<div id="map"></div>');
    AGB.add_map_to_div(db, '#map');
};

AGB.add_map_to_div = function(db, id)
{
    $(id).svg();
    $(id).load(IMPACT.wsurl + '/map/' + db + '?lang=' + IMPACT.lang,
               function(svg, error) {
                   AGB.traverse_map(function(element) {
                       AGB.add_map_to_browser_listener(db, element);
                   });
               });
    
};

