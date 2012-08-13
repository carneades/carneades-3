// (jdbc/create-table 
//           :metadata
//           [:id "int identity"]    ; local id, see also key and identifier
//           [:key "varchar unique"] ; citation key, optional
//           [:contributor "varchar"]
//           [:coverage "varchar"]
//           [:creator "varchar"]
//           [:date "varchar"]       ; http://www.w3.org/TR/NOTE-datetime                         
//           [:description "int"]
//           [:format "varchar"]     ; A list of MIME types, semicolon separated
//           [:identifier "varchar"] ; URI, DOI or something similar
//           [:language "varchar"]
//           [:publisher "varchar"]
//           [:relation "varchar"]
//           [:rights "varchar"]
//           [:source "varchar"]
//           [:subject "varchar"]
//           [:title "varchar"]
//           [:type "varchar"]       ; see: http://dublincore.org/documents/dcmi-type-vocabulary/
//           ["foreign key(description) references translation(id)"])

AGB.create_metadata_editor = function() {
    var html = ich.metadataeditor();
    return html;
};

AGB.get_metadata_data = function() {
  return {// key: $('#metadata-key').val() == "" ? null : $('#metadata-key').val(),
          // title: $('#metadata-title').val(),
          description: {en: $('#metadata-description').val() }};
};

AGB.fillin_metadata_editor = function(header) {
    if(_.isNil(header)) {
        return;
    }
    
    $('#metadata-description').val(header.description.en);
};