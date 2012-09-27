// Displays the list of issues
PM.SctIssues = Backbone.View.extend(
    {className: "sct-issues",
     
     events: {
         "click a" : "issue_selected"
     },
     
     initialize: function(attrs) {
         this.issues = attrs.issues;
         this.model.on('change', this.render, this);
         _.bindAll(this, 'render', 'issue_selected');
     },
     
     render: function() {
         var data = this.model.toJSON();

         AGB.set_mainissues_text(this.issues);
         
         var issues_html = ich['sct-issues'](
             {'sct_choose_issue': $.i18n.prop('sct_choose_issue'),
              'sct_issues': $.i18n.prop('sct_issues'),
              'sct_description': $.i18n.prop('sct_description'),
              'main_issues': this.issues
             });
         this.$el.html(issues_html);
         
         this.$('input').first().attr('checked', true);
         
         return this;
     },
     
     issue_selected: function() {
         var index = $(event.target).parent().attr('id').substr(5) - 1;
         var issue = this.issues[index];
         alert(issue);
         return false;
     }
     
    }
);
