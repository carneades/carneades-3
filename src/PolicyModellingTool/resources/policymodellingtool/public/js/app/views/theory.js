// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

// A view to represent a theory
PM.TheoryView = Backbone.View.extend(
    {className: "theory-view",
     
     events: {
     },
     
     initialize: function(attrs) {
         this.model.on('change', this.render, this);
         this.current_scheme = attrs.current_scheme;
//         _.bindAll(this, 'render', 'funcname');
     },
     
     render: function() {
         var data = this.model.toJSON();
         var lang = PM.find_available_lang(data);
         
         if(data.header.description && data.header.description[lang]) {
             data.description_text = PM.markdown_to_html(data.header.description[lang]);
         }
         
         data.outline_text = PM.theory_outline_text(data.schemes, 'schemes');
         data.table_of_contents = $.i18n.prop('pmt_table_of_contents');
         data.schemes_text = this.schemes_text();

         data = PM.merge_menu_props(data);

         this.$el.html(ich.theory(data));
         
         if(this.current_scheme != undefined) {
             PM.scroll_to($('#' + this.current_scheme));
         }
         
         return this;
     },
     
     schemes_text: function() {
         var data = this.model.toJSON();
         var text = "";
         var lang = PM.find_available_lang(data);
         var language_clj = catb.views.pmt.theory.convert_language(data.language);
         
         _.each(data.schemes, function(scheme) {
                    text += '<div id="{0}">'.format(scheme.id);
                    text += '<h3>{0}</h3>'.format(scheme.header.title);
                    if(scheme.header.description && scheme.header.description[lang]) {
                        text += '<p class="description">{0}</p>'.format(PM.markdown_to_html(scheme.header.description[lang]));
                    }
                    PM.set_metadata_has_properties(scheme.header);
                    scheme.header.header_hastitle = false;
                    // get the whole html, see http://jquery-howto.blogspot.de/2009/02/how-to-get-full-html-string-including.html
                    var md = ($('<div>').append(ich.metadata(scheme.header))).remove().html();
                    text += md;
                    text += PM.scheme_content_text(language_clj, scheme, lang);
                    text += '</div>';
                });

         return text;
     }
     
    }
);
