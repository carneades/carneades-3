PM.ArgumentEditorView = Backbone.View.extend(
    {className: "argument-editor",
     render: function() {
         $(this.el).html(ich.argumenteditor());

         return this;
     } 
    }
);
