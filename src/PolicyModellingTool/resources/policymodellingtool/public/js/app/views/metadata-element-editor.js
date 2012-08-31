// A view for an element of a metadata (except description)
PM.MetadataElementEditorView = Backbone.View.extend(
    {className: "metadata-element-editor-view",
     
     events: {
         "change .metadata-element-input": "content_changed",
         "click a.delete": "delete_element"
     },
     
     initialize: function(attrs) {
         this.type = attrs.type;
         this.name = attrs.name;
         this.model.on('change', this.render, this);
         _.bindAll(this, 'render', 'content_changed');
     },
     
     render: function() {
         var data = this.model.toJSON();
         
         this.$el.html(ich.metadataelementeditor({name: this.name}));
         var val = this.model.get_element_val(this.type, this.cid);
         if (val) {
             this.$('.metadata-element-input').val(val);
         } 
         
         return this;
     },
     
     content_changed: function() {
         var content = this.$('.metadata-element-input').val();
         this.model.set_element_val(this.type, this.cid, content);
         
         return false;
     },
     
     delete_element: function () {
         this.model.delete_element_val(this.type, this.cid);
         
         this.remove();
         return false;
     }
     
    }
);
