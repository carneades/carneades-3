function set_argumentgraph_url(db)
{
    $.address.value(argumentgraph_url(db));    
}

function argumentgraph_url(db)
{
    return '/argumentgraph/' + db;
}

function argumentgraph_html(db, data) 
{
    data.normalize();
    data.db = db;
    data.metadata_text = format_metadata(data.metadata[0]);
    data.description_text = description_text(data.metadata[0]);
    set_mainissues_text(data.main_issues);
    data.references = data.metadata.filter(function (ref) { return ref.key; });
    set_references_text(data.references);
    data.title = markdown_to_html(data.metadata[0].title);
    data.outline_text = outline_text(data.outline, db);
    var argumentgraph_html = ich.argumentgraph(data);
    return argumentgraph_html.filter('#argumentgraph');
}

function display_argumentgraph(db)
{
    ajax_get('argumentgraph-info/' + db,
             function(data) {
                 $('#browser').html(argumentgraph_html(db, data));
                 // $('#close').click(on_close);
                 // add_map_to_div(db, '#map');
             });
}

function set_mainissues_text(mainissues)
{
    $.each(mainissues, 
           function(index, issue) {
               issue.statement_nb = index + 1;
               issue.statement_text = statement_text(issue);
           });
    return mainissues;
}

function set_references_text(metadata)
{
    $.each(metadata, 
           function(index, md) {
               md.metadata_text = format_metadata(md);
           });
}

function outline_text(tree, db, index)
{
    var text = "";
    var node = tree[0];
    var children = tree[1];
    
    if(node === "root") {
        text = "<ul>";
    } else if (node.hasOwnProperty('premises')) {
        var direction_text = node.pro ? "pro" : "con";
        text = '{0} <span class="content">{1}</span> <ul>'.format(direction_text, argument_link(db, node.id, argument_text(node, index)));
    } else {
        var stmt_text = statement_text(node, index);
        text = "{0} <ul>".format(statement_link(db, node.id, stmt_text));
    }

    $.each(children,
           function(index, child) {
               text += "<li>{0}</li>".format(outline_text(child, db, index + 1));
           });
    text += "</ul>";

    return text;
}
