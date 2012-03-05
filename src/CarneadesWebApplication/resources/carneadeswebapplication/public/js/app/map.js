function map_url(db)
{
    return '/map/' + db;
}

function add_map_to_browser_listener(db, domelement)
{
    var g = $(domelement);
    if(domelement.id) {
        if(domelement.id[0] == 's') {
            g.click(function() {
                        set_statement_url(db, domelement.id.slice(2));
                    });
            g.mouseover(function () {
	                    $(this).css('cursor', 'hand');
                        });
        } else if(domelement.id[0] == 'a') {
            g.click(function() {
                        set_argument_url(db, domelement.id.slice(2));
                    });
            g.mouseover(function () {
	                    $(this).css('cursor', 'hand');
                        });
        }
    }
}

function traverse_map(visitor)
{
    $('g[id]').each(
           function(index, domelement) {
               visitor(domelement);
           });
}

function display_map(db)
{
    $('#browser').html('<div id="map"></div>');
    add_map_to_div(db, '#map');
}

function add_map_to_div(db, id)
{
    $(id).svg();
    $(id).load(CARNEADES.carneadeswsurl + '/map/' + db, function(svg, error) {
                                       traverse_map(function(element) {
                                                        add_map_to_browser_listener(db, element);
                                                    });
                                 });
    
}

