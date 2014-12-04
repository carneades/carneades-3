define [
  'root'
  'classes'
  'utils'
], (cn) ->

  class ProjectViewController extends cn.carneades.Controller
    @.$inject = [
      "$scope"
      "$stateParams"
      "$state"
      "$translate"
      "$location"
      "$previousState"
      "project"
      ]

    @.$resolve =
      project: ($stateParams, ProjectLoader) ->
        return new ProjectLoader $stateParams

    constructor: (
      @scope, @stateParams, @state,
      @translate, @location,
      @previousState, @project
    ) ->
      @state.$current.self.tooltip = @project.title
      @stateParams.db = 'main'
      @.tooltipNew = @translate.instant 'tooltip.argumentgraph.new'

    newArgumentGraph: ->
      @state.go 'home.projects.project.new', pid: @project.id, reload: true
      @previousState.memo 'newArgumentGraphEditor'

    getLink: ->
      lnk = [
        @location.protocol()
        "://"
        @location.host()
        ":"
        @location.port()
        "/carneades"
        @state.href 'home.projects.project'
      ].join ''

      window.prompt(
        "Copy to clipboard: Ctrl+C, Enter",
        lnk,
        pid: @project.id
      )
