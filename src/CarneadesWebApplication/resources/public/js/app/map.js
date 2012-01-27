function map_url(db)
{
    return '/map/' + db;
}

function add_map_to_browser_listener(db, domelement)
{
    var g = $("g[id='{0}']".format(domelement.id));
    if(domelement.id) {
        if(domelement.id[0] == 's') {
            g.click(function() {
                        set_statement_url(db, domelement.id.slice(2));
                    });
        } else if(domelement.id[0] == 'a') {
            g.click(function() {
                        set_argument_url(db, domelement.id.slice(2));
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

function load_done(svg, error)
{
    console.log('svg loaded');
}

function display_map(db)
{
    $('body').html('<div id="map"></div>');
    $('#map').svg();
    $('#map').load('/map/aston', function(svg, error) {
                                       traverse_map(function(element) {
                                                        add_map_to_browser_listener(db, element);
                                                    });
                                 });
}

