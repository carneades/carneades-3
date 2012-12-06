// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

// A candidate for replacing some Metadata
// 
// It references a Metadata
// and allows the modification, the indexing,
// and the references of Metadata elements
// by key/id access
// Each id corresponds to the cid of a MetadataElementEditorView
// 
// It also modify the Metadata elements without indirection, thus
// preserving the store/restore semantic
PM.MetadataCandidate = Backbone.Model.extend(
    {
     // Expects a PM.Metadata and a current_lang field
     initialize: function(attrs) {
     },

     // hash type -> id -> index
     // the index is the index of the element
     // in the array for the type
     indexes: {
         
     },
     
     // Indexes an existing metadata element in order
     // to reference it later with the set_element_val
     // function
     index_element: function(type, id, val) {
         if(_.isNil(this.indexes[type])) {
             this.indexes[type] = {};
         }

         var index = this.get('metadata').get(type).indexOf(val);
         this.indexes[type][id] = index; 
     },
     
     // Sets the metadata element of type @type
     // and identified by @id to the val @val
     // in the metadata
     set_element_val: function(type, id, val) {
         var metadata = this.get('metadata');
         // clone preserves the old array for the store/restore operations
         var elements = _.clone(metadata.get(type));
         var index = -1;

         if(_.isNil(this.indexes[type]) || _.isNil(elements)) {
             // first time we create an element of this type
             index = 0;
             this.indexes[type] = {};
             this.indexes[type][id] = index;
             metadata.set(type, [val]);
         } else if(this.indexes[type] && this.indexes[type][id]) {
             // we already have an element for this id
             index = this.indexes[type][id];
             elements[index] = val;
             metadata.set(type, elements);
         } else {
             // we already have element for this type
             // but no element with this id
             index = elements.length;
             this.indexes[type][id] = index;
             elements[index] = val;
             metadata.set(type, elements); 
         }
     },
     
     get_element_val: function(type, id) {
         if(_.isNil(this.indexes[type]) || _.isNil(this.indexes[type][id])) {
             return undefined;
         }

         var index = this.indexes[type][id];
         return this.get('metadata').get(type)[index];
     },
     
     delete_element_val: function(type, id) {
         if(_.isNil(this.indexes[type]) || _.isNil(this.indexes[type][id])) {
             return;
         }
         
         var index = this.indexes[type][id];
         delete this.indexes[type][id];
         var metadata = this.get('metadata');
         var elements = _.clone(metadata.get(type));
         elements.splice(index, 1);
         metadata.set(type, elements);
         // reindexes the consecuting elements
         // of the array
         var self = this;
         _.each(this.indexes[type],
                function(idx, id) {
                    if(idx > index) {
                        self.indexes[type][id] = index - 1;    
                    }
                });
         
     }
     
    }
);
