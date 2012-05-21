AGB.set_argumentgraph_url = function(db)
{
    $.address.value(AGB.argumentgraph_url(db));    
};

AGB.argumentgraph_url = function(db)
{
    return '/argumentgraph/' + db;
};

AGB.argumentgraph_html = function(db, data) 
{
    data.normalize();
    data.db = db;
    data.metadata_text = AGB.format_metadata(data.metadata[0]);
    data.description_text = AGB.description_text(data.metadata[0]);
    AGB.set_mainissues_text(data.main_issues);
    data.references = data.metadata.filter(function (ref) { return ref.key; });
    AGB.set_references_text(data.references);
    data.title = AGB.markdown_to_html(data.metadata[0].title);
    data.outline_text = AGB.outline_text(data.outline, db);
    var argumentgraph_html = ich.argumentgraph(data);
    return argumentgraph_html.filter('#argumentgraph');
};

AGB.display_argumentgraph = function(db)
{
    AGB.ajax_get('argumentgraph-info/' + db,
             function(data) {
                 $('#browser').html(AGB.argumentgraph_html(db, data));
                 // $('#close').click(on_close);
                 // add_map_to_div(db, '#map');
             });
};

AGB.set_mainissues_text = function(mainissues)
{
    $.each(mainissues, 
           function(index, issue) {
               issue.statement_nb = index + 1;
               issue.statement_text = AGB.statement_text(issue);
           });
    return mainissues;
};

AGB.set_references_text = function(metadata)
{
    $.each(metadata, 
           function(index, md) {
               md.metadata_text = AGB.format_metadata(md);
           });
};

AGB.outline_text = function(tree, db, index)
{
    var text = "";
    var node = tree[0];
    var children = tree[1];
    
    if(node === "root") {
        text = "<ul>";
    } else if (node.hasOwnProperty('premises')) {
        var direction_text = node.pro ? "pro" : "con";
        text = '{0} <span class="content">{1}</span> <ul>'.format(direction_text, AGB.argument_link(db, node.id, AGB.argument_text(node, index)));
    } else {
        var stmt_text = AGB.statement_text(node, index);
        text = "{0} <ul>".format(AGB.statement_link(db, node.id, stmt_text));
    }

    $.each(children,
           function(index, child) {
               text += "<li>{0}</li>".format(AGB.outline_text(child, db, index + 1));
           });
    text += "</ul>";

    return text;
};
