// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

// A view to edit metadata
PM.MetadataEditorView = Backbone.View.extend(
    {className: "metadata-editor-view",
     
     events: {
         "blur .metadata-description-input": "description_changed",
         "click .add-metadata-element": "on_add_metadata_element" 
     },
     
     initialize: function() {
         this.model.on('change', this.render, this);
         _.bindAll(this, 'render', 'description_changed', 'on_add_metadata_element',
                  'change_lang');
         this.model.get('metadata').store();
         this.elements = {key: $.i18n.prop('pmt_key'),
                          contributor: $.i18n.prop('pmt_contributor'),
                          coverage: $.i18n.prop('pmt_coverage'),
                          creator: $.i18n.prop('pmt_creator'),
                          date: $.i18n.prop('pmt_date'),
                          format: $.i18n.prop('pmt_format'),
                          identifier: $.i18n.prop('pmt_identifier'),
                          language: $.i18n.prop('pmt_language'),
                          publisher: $.i18n.prop('pmt_publisher'),
                          relation: $.i18n.prop('pmt_relation'),
                          rights: $.i18n.prop('pmt_rights'),
                          source: $.i18n.prop('pmt_source'),
                          subject: $.i18n.prop('pmt_subject'),
                          title: $.i18n.prop('pmt_title'),
                          type: $.i18n.prop('pmt_type')             
                         }; 
     },
     
     render: function() {
         var data = this.model.toJSON();

         this.$el.html(ich.metadataeditor2({description: $.i18n.prop('pmt_description'),
                                            adda: $.i18n.prop('pmt_add_a'),
                                            totheheader: $.i18n.prop('pmt_to_the_header'),
                                            go: $.i18n.prop('pmt_add')}));
         
         this.$('.language-chooser').tabs({select: this.change_lang,
                                           selected: 0});
         
         this.select_tab(data.current_lang);
         
         if(data.metadata.attributes.description) {
             this.description().val(
                 data.metadata.attributes.description[data.current_lang]); 
         } 

         this.$('.metadata-description-input').markItUp(mySettings);

         var self = this;
         _.each(data.metadata.attributes,
                function(elements, type) {
                    if(type != 'description') {
                        _.each(elements,
                              function(val) {
                                  self.add_metadata_element(type, val); 
                              });
                    }
                });
         
         _.each(this.elements,
               function(text, val) {
                  self.$('.select-metadata-element').
                       append('<option value="' + val + '">' + text + '</option>');
               });
         
         return this;
     },

     description: function() {
         return this.$('.metadata-description-input');
     },
     
     remove: function() {
         this.model.get('metadata').restore();
         Backbone.View.prototype.remove.call(this);
     },

     description_changed: function() {
         var metadata = this.model.get('metadata');
         var description = _.clone(metadata.get('description'));
         description[this.model.get('current_lang')] = this.description().val();
         metadata.set('description', description);
     },
     
     add_metadata_element: function(type, val) {
         var element_view = new PM.MetadataElementEditorView(
             {model: this.model,
              type: type,
              name: this.elements[type]});
         if(val) {
             this.model.index_element(type, element_view.cid, val);
         }
         element_view.render();
         this.$('.metadata-elements').append(element_view.$el);
     },
     
     on_add_metadata_element: function() {
         var type = this.$('.select-metadata-element').val();
         this.add_metadata_element(type);
     },

     get_lang: function(a) {
         var href = $(a).attr('href');
         var lang = href.substr(href.length - 2);

         return lang;
     },
     
     select_tab: function(lang) {
         var self = this;
         _.each(this.$('li'), 
                function(li) { 
                    if(self.get_lang($(li).find('a')) == lang) {
                        $(li).addClass('ui-tabs-selected');
                        $(li).addClass('ui-state-active');               
                    } else {
                        $(li).removeClass('ui-tabs-selected'); 
                        $(li).removeClass('ui-state-active');     
                    }
                    
                });

         
     },
     
     change_lang: function(event, ui) {
         var lang = this.get_lang(ui.tab);
         
         this.model.set('current_lang', lang);

         return false;
     }

    }
);
