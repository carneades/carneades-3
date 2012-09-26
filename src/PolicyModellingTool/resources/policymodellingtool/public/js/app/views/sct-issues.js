// Displays the list of issues
PM.SctIssues = Backbone.View.extend(
    {className: "sct-issues",
     
     events: {
         "click .submit-issue" : "issue_selected"
     },
     
     initialize: function(attrs) {
         this.policies = attrs.policies;
         this.current_policy = attrs.current_policy;
         this.model.on('change', this.render, this);
         _.bindAll(this, 'render', 'issue_selected');
     },
     
     render: function() {
         var data = this.model.toJSON();
       
         var issues_html = ich['sct-issues'](
             {'sct_choose_issue': $.i18n.prop('sct_choose_issue'),
              'sct_issues': $.i18n.prop('sct_issues'),
              'sct_description': $.i18n.prop('sct_description'),
              policy: this.policies.get(this.current_policy)
             });
         this.$el.html(issues_html);
         
         this.$('input').first().attr('checked', true);
         
         return this;
     },
     
     issue_selected: function() {
         // get issue, begin questions
         alert('issue!');
         
         return false;
     }
     
    }
);
