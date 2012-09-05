AGB.format_filtered_statement = function(statement) {
    return "<div>{0}</div>".format(AGB.statement_text(statement));
};

AGB.format_selected_statement = function(statement) {
    if(_.isNil(statement)) {
        // if called on an empty initial selection
        return "";
    }
    return "{0}".format(AGB.statement_raw_text(statement));
};

AGB.format_filtered_scheme = function(scheme) {
    return "<div>{0}</div>".format(scheme.header.title);
};

AGB.format_selected_scheme = function(scheme) {
    return "{0}".format(scheme.header.title);
};

AGB.format_filtered_matching_result = function(result) {
    return AGB.format_filtered_statement(result.statement);
};

AGB.format_selected_matching_result = function(result) {
    if(result.id == "") {
        // if called on an empty initial selection
        return "";
    }
    return AGB.format_selected_statement(result.statement);
};
