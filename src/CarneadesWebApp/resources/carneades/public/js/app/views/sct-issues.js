// Copyright (c) 2012 Fraunhofer Gesellschaft
// Licensed under the EUPL V.1.1

// Displays the list of issues
PM.SctIssues = Backbone.View.extend(
    {className: "sct-issues",
     
     events: {
         "click a" : "issue_selected"
     },
     
     initialize: function(attrs) {
         this.model.set('statement-poll', 
                        new PM.StatementPoll(
                            {id: this.model.get('username'),
                             votes: {}}));
         this.model.set('argument-poll', 
                        new PM.ArgumentPoll(
                            {id: this.model.get('username'),
                             votes: {}}));

         this.issues = attrs.issues;
         this.statements = attrs.statements;
         this.arguments = attrs.arguments;
         // this.model.on('change', this.render, this);
         _.bindAll(this, 'render', 'issue_selected');
     },
     
     render: function() {
         var data = this.model.toJSON();

         AGB.set_mainissues_text(this.issues);
         
         var info = PM.debate_info.get('metadata')[0];
         var description = info.description[IMPACT.lang];
         
         var issues_html = ich['sct-issues'](
             {'sct_choose_issue': $.i18n.prop('sct_choose_issue'),
              'sct_issues': $.i18n.prop('sct_issues'),
              'sct_description': $.i18n.prop('sct_description'),
              'title': info.title,
              'description': PM.markdown_to_html(description),
              'main_issues': this.issues
             });
         this.$el.html(issues_html);
         
         this.$('input').first().attr('checked', true);
         
         return this;
     },
     
     issue_selected: function(event) {
         var index = $(event.target).parent().attr('id').substr(5) - 1;
         var issue = this.issues[index];
         
         this.model.set('issue', issue);
     
         var pro_args = issue.pro;
         var con_args = issue.con;
         
         var self = this;
         var premises_from_pro = _.map(pro_args,
                                       function(pro) {
                                           return self.arguments.get(pro).get('premises');
                                       });
         var premises_from_con = _.map(con_args,
                                       function(con) {
                                           return self.arguments.get(con).get('premises');
                                       });
         var premises = _.flatten(premises_from_pro.concat(premises_from_con));
         
         _.each(premises,
               function(premise) {
                   self.model.push_question(premise.statement, 'claim');
               });
         
         self.model.update_current_question();
         
         PM.set_sct_question_url();
         
         return false;
     }
     
    }
);
