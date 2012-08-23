// A candidate for the conclusion of an argument
PM.SchemeCandidateView = Backbone.View.extend(
    {className: "scheme-candidate-view",
     
     events: {
         "change input[type=hidden]" : "scheme_changed"
     },
     
     initialize: function() {
         this.model.on('change', this.render, this);
         _.bindAll(this, 'scheme_changed', 'render');
     },
     
     render: function() {
         var data = this.model.toJSON();
         
         this.$el.html(ich.schemecandidate({scheme_text: "Scheme"}));
         
         this.scheme().select2({formatResult: AGB.format_filtered_scheme,
                                formatSelection: AGB.format_selected_scheme, 
                                placeholder: data.scheme_name,
                                data: {
                                    results: data.schemes.toJSON(),
                                    text: function(scheme) {
                                        return scheme.header.title;
                                    }
                                },
                                initSelection: function(element, callback) {
                                    callback(data.schemes.get(element.val()).toJSON());
                                }});
         
         // if(data.scheme) {
         //     this.scheme().val(data.scheme.id).trigger('change');    
         // }
         
         return this;
     },
     
     scheme: function() {
         return this.$('input[type=hidden]');
     },
     
     scheme_changed: function() {
         
     }
     
    }
);



