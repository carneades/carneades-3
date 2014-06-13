# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
define ['angular'], (angular) ->
  "use strict"

  initQuestionGroupInputs = (questions) ->
    for question in questions
      if (question.values && question.values[0][0])
        question.input = question.values[0][0]
      else
        question.input = undefined

      if !question.grounded
        question.facts = ({index: i - 1, input: undefined} for i in [1..question.min])
        question.facts[0].input = question.input
        question.nbFacts = question.min
        question.lastIndex = question.nbFacts - 1


  services = angular.module('lican.resources', [])
  #
  # notes on mocking the backend http://docs.angularjs.org/api/ngMock.$httpBackend
  #
  services.factory 'MultiQuestionLoader', ($http) ->
    class Questions
      questions: []
      solution: {db: undefined}
      transaction_id: undefined

      pushNewQuestions: (questions) ->
        initQuestionGroupInputs questions
        @questions.push questions

      processData: (data) ->
        @transaction_id = data.uuid

        if data.db?
          @solution.db = data.db
        else
          @pushNewQuestions data.questions

      analyse: (entity) ->
        console.log 'MultiQuestionLoader', entity

        ($http.get "../carneades/api/lican/analyse?entity=#{entity}")
          .success (data) =>
            @processData data
          .error (data, status) ->
            # TODO: notifications
            console.log 'error calling service'

      getCurrentQuestionGroup: ->
        @questions[@questions.length - 1]

      getQuestionGroups: ->
        @questions

      sendAnswer: ->
        current = @getCurrentQuestionGroup()

        answers = (for q in current
          if current.facts?
            {id: q.id, values: fact.input for fact in current.facts}
          else
            {id: q.id, value: q.input})

        console.log 'answers', answers

        ($http.post "../carneades/api/lican/answers/send",
          {answers: answers, uuid: @transaction_id})
          .success (data) =>
            console.log 'received new questions'
            @processData data

          .error (data, status) ->
            # TODO: notifications
            console.log 'error sending the answers'

    new Questions

  services.factory 'DemoMultiQuestionLoader', ->
    class Questions
      fromServer: [[
        category_name: "Purpose"
        text: "the use by the person of the work is for ?2561 purposes."
        typename: ["commercial (c)", "non-commercial"]
        grounded: false
        max: 2
        statement: ["type-of-use", ["the-use", "the-person", "the-work"], "commercial"]
        concept: false
        type: ["commercial", "non-commercial"]
        hint: "Will the work be used for commercial or non-commercial purposes?"
        min: 1
        role: true
        answers: ["Yes", "No", "Maybe"]
        id: 1
        formalanswers: null
        category: "purpose"
      ], [
        category_name: "License"
        text: "1) Does the person have a license to publish the work?"
        typename: null
        grounded: true
        max: null
        statement: ["license-to-publish", "the-person", "the-work"]
        concept: false
        type: "symbol"
        hint: "Information about an existing license."
        min: 0
        role: true
        answers: ["Yes", "No", "Maybe"]
        id: 2
        formalanswers: ["yes", "no", "maybe"]
        category: "license"
        values: [["no"]]
      ], [
        category_name: "Search"
        text: "The type of the search by the person for the owner of the work was a ?2567."
        typename: ["professional documented search", "standard documented search", "none"]
        grounded: false
        max: 1
        statement: ["search-type", ["the-search", "the-person", "the-work"], "professional"]
        concept: false
        type: ["professional", "standard", "none"]
        hint: "What type of search was performed to try to find the copyright owner?"
        min: 1
        role: true
        answers: ["Yes", "No", "Maybe"]
        id: 3
        formalanswers: null
        category: "search"
      ,
        category_name: "Search"
        text: "Was the search by the person for the owner of the work publically announced?"
        widgets: ["radio"]
        grounded: true
        statement: ["announcement", ["the-search", "the-person", "the-work"]]
        concept: true
        hint: "Information about an announcement."
        role: false
        answers: ["Yes", "No", "Maybe"]
        id: 4
        formalanswers: ["yes", "no", "maybe"]
        category: "search"
      ]]
      questions: []
      idx: -1
      solution: {db: undefined}

      getNextQuestionGroup: ->
        if @hasQuestions()
          @idx++
          @questions.push(@fromServer[@idx])
          initQuestionGroupInputs(@questions[@idx])
        else
          console.log 'Error, no more questions'

        undefined

      analyse: (entity) ->
        @getNextQuestionGroup()

      getCurrentQuestionGroup: ->
        if @questions[@idx]?
          @questions[@idx]
        else
          []

      getQuestionGroups: ->
        @questions

      hasQuestions: ->
        return @idx + 1 < @fromServer.length

      sendAnswer: () ->
        console.log @getCurrentQuestionGroup()

        if @hasQuestions()
          @getNextQuestionGroup()
        else
          @solution.db = "main"

    new Questions

  services
