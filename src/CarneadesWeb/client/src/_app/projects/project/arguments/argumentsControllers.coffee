# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.
define [
  'angular'
  'root'
  'classes'
  'utils'
], (angular, cn) ->
  'use strict'

  carneades = cn.carneades

  # If you want to merge two arrays without creating a new object

  Array::where = (query) ->
    return [] if typeof query isnt "object"
    hit = Object.keys(query).length
    @filter (item) ->
      match = 0
      for key, val of query
        match += 1 if item[key] is val
      if match is hit then true else false

  _resolveRoleKey = (p, roles) ->
    if isFinite p.role
      p.role = roles[p.role].title

  _resolveRoleKeys = (premises, roles) ->
    for p in premises
      _resolveRoleKey p, roles

  mergeRolesD = (a1, a2) ->
    for o in a2
      contains = a1.where title: o.title
      if contains.length is 0
        a1.push id: a1.length + 1, title: o.title

  mergePremisesD = (a1, a2, roles) ->
    for o in a2
      contains = a1.where role: o.role
      if contains.length is 0
        _resolveRoleKey o, roles
        a1.push o

  modules = [
    'pascalprecht.translate'
    'angular-capitalize-filter'
  ]

  module = angular.module 'arguments.controllers', modules


  class PremiseService extends carneades.Service
    @.$inject = [
      '$state'
      '$stateParams'
      'editorService'
    ]

    constructor: (@state, @stateParams, @editorService) ->
      @.repoRoles = []
      @.repoPremises = []

    addPremise: (scope) ->
      @editorService.addPremise scope.argument

    deletePremise: (scope, p) ->
      @editorService.deletePremise scope.argument, p

    getRoles: ({premises}) ->
      roles = []
      unless premises then return roles
      for p,i in premises
        roles.push id: i, title: p.role

      return roles

    getPremises: ({premises}) ->
      ps = []
      unless premises then return ps
      for p in premises
        if p.statement? and typeof p.statement.text is 'string'
          ps.push p
        else
          p.statement = {}
          p.statement.id = ''
          p.statement.text = ''
          ps.push p

      return ps

    initRoles: (a) ->
      if a.premises?.length > 0
        @.repoRoles = @.getRoles a
        @.repoPremises = @.getPremises a

    updatePremises: (scope, newVal, oldVal) ->
      if oldVal is '' and scope.argument.premises.length > 0
        @.initRoles scope.argument

      roles = @repoRoles
      premises = @repoPremises
      if newVal?
        newRoles = @.getRoles newVal
        newPremises = @.getPremises newVal

        if premises.length > 0
          _resolveRoleKeys premises, roles
          mergePremisesD @.repoPremises, newPremises, roles
        else @.repoPremises = newPremises

        if roles.length > 0
          mergeRolesD @.repoRoles, newRoles
        else @.repoRoles = newRoles

        scope.premiseRoles = @.repoRoles
        scope.argument.premises = []
        scope.argument.premises.push @.repoPremises...


  module.service '$cnPremise', PremiseService


  class NewArgumentController extends carneades.Controller
    @.$inject = [
      '$scope'
      '$state'
      '$stateParams'
      '$translate'
      '$cnBucket'
      '$cnPremise'
      'Argument'
      'editorService'
      'statements'
      'theory'
      'conclusion'
    ]

    constructor: (@scope, @state, @stateParams, @translate,
      @cnBucket, @cnPremise, @Argument, @editorService, @statements,
      @theory, @conclusion
    ) ->
      @.defineScope()

    getArgumentTemplate: =>
      return carneades.extend {},
        header:
          title: ''
          description:
            en: ''
            de: ''
            fr: ''
            it: ''
            sp: ''
            nl: ''
        pro: true
        scheme: ''
        strict: false
        weight: 0.5
        conclusion: if @conclusion then @conclusion.id else ''
        premises: []

    showModel: =>
      @scope.tabModel = true
      @scope.tabMetadata = false

    showMetadata: =>
      @scope.tabModel = false
      @scope.tabMetadata = true


    save: =>
      pid = @stateParams.pid
      db = @stateParams.db
      @cnPremise.updatePremises @scope, @scope.argument

      for p in @scope.argument.premises
        key = 'translation'
        if key of p then delete p[key]

      @Argument.save({pid: pid, db: db}, {
        header: @scope.argument.header
        pro: @scope.argument.pro
        scheme: "(#{@scope.argument.scheme.id})"
        weight: @scope.argument.weight
        conclusion: @scope.argument.conclusion
        premises: @scope.argument.premises
      }).$promise.then((a) =>
        url = 'home.projects.project.arguments.argument'
        params = pid: pid, db: db, aid: a.id
        @state.transitionTo url, params
        @cnBucket.remove @state.$current
      )


    defineScope: =>
      @scope = carneades.extend @scope,
        statements: @statements
        theory: @theory
        argument: @.getArgumentTemplate()
        addPremise: () => @cnPremise.addPremise @scope
        deletePremise: (p) => @cnPremise.deletePremise @scope, p
        tabModel: true
        tabMetadata: false
        showModel: @.showModel
        showMetadata: @.showMetadata
        onSave: @.save
        onCancel: () =>
          url = 'home.projects.project.outline'
          @state.transitionTo url, @stateParams
          @cnBucket.remove @state.$current
        languages: @editorService.getLanguages()
        getSchemeTitle: @.getSchemeTitle

        getStatementText: @.getStatementText
        editorOptions: @editorService.getCodeMirrorOptions()
        title: @translate.instant 'projects.createargument'
        tooltipPremise: @translate.instant 'tooltip.premise'
        tooltipCancel: @translate.instant 'tooltip.cancel'
        tooltipSave: @translate.instant 'tooltip.argument.save'


      @scope.$watch 'argument.scheme', (newVal, oldVal) =>
        @cnPremise.updatePremises @scope, newVal, oldVal


  module.controller 'ArgumentNewCtrl', NewArgumentController
