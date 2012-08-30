// A candidate for replacing some Metadata
PM.MetadataCandidate = Backbone.Model.extend(
    {defaults: function(){
         return {
         
         };
     },
     
     // hash type -> id -> index
     // the index is the index of the element
     // in the array for the type
     elements: {
         
     }, 
     
     // Expects a PM.Metadata and a current_lang field
     initialize: function(attrs) {
         var memento = new Backbone.Memento(this);
         _.extend(this, memento);
     },
     
     // Indexes an existing metadata element in order
     // to reference it later with the set_element_val
     // function
     index_element: function(type, id, val) {
         if(_.isNil(this.elements[type])) {
             this.elements[type] = {};
         }

         var index = this.get('metadata').get(type).indexOf(val);
         this.elements[type][id] = index; 
     },
     
     // Sets the metadata element of type @type
     // and identified by @id to the val @val
     // in the metadata
     set_element_val: function(type, id, val) {
         var metadata = this.get('metadata');
         var elements = metadata.get(type);
         var index = -1;

         if(_.isNil(elements[type])) {
             // first time we create an element of this type
             index = 0;
             this.elements[type] = {};
             this.elements[type][id] = index;
             metadata.set(type, [val]);
         } else if(this.elements[type] && this.elements[type][id]) {
             // we already have an element for this id
             index = this.elements[type][id];
             elements[index] = val;
             metadata.set(type, elements);
         } else {
             // we already have element for this type
             // but no element with this id
             index = elements.length;
             this.elements[type][id] = index;
             elements[index] = val;
             metadata.set(type, elements); 
         }
     },
     
     get_element_val: function(type, id) {
         if(_.isNil(this.elements[type]) || _.isNil(this.elements[type][id])) {
             return undefined;
         }

         var index = this.elements[type][id];
         return this.get('metadata').get(type)[index];
     }
     
    }
);
