// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

// Subview for displaying the premises or the exceptions candidates
PM.PremisesCandidatesView = Backbone.View.extend(
    {className: "premises-candidates-view",
     
     events: {
         "click .add-premise": "add_premise"
     },

     initialize: function(attrs) {
         this.container = attrs.container;
         this.model.on('change', this.render, this);
         this.model.get(this.container).on('add', this.render, this);
         this.model.get(this.container).on('remove', this.render, this);
         this.add_more_text = attrs.add_more_text;
         this.elements_name = attrs.elements_name;
         _.bindAll(this, 'render');
     },

     render: function() {
         var haselements = this.model.get(this.container).length > 0;
         this.$el.html(ich.premisescandidates({haselements: haselements,
                                               elements_name: this.elements_name}));

         var self = this;
         self.premises_candidates_views = [];
         this.model.get(this.container).each(
             function(premise) {
                 premise.store();
                 premise.set('container', self.model.get(self.container));
                 var premisecandidateview = new PM.PremiseCandidateView({model: premise});
                 self.premises_candidates_views.push(premisecandidateview);
                 premisecandidateview.render();
                 self.$('.argument-premises').append(premisecandidateview.$el);
             });
         
         this.$el.append(ich.button({clazz: "add-premise",
                                     value: this.add_more_text}));    
         
         return this;
     },
     
     add_premise: function() {
         var premisecandidate = new PM.PremiseCandidate(
             {statements: this.model.get('statements')});

         this.model.get(this.container).add(premisecandidate); 
         return false;
     },
     
     remove: function() {
         _.each(this.premises_candidates_views,
                function(view) {
                    view.remove();
                });

         // yep...
         Backbone.View.prototype.remove.call(this);
     },
     
     cancel: function() {
         this.model.get(this.container).each(
             function(premise) {
                 premise.restore();
             });
     }
     
    }
);
