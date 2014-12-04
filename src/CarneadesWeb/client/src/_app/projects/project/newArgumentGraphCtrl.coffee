define [
  'root'
  'classes'
  'utils'
], (cn) ->

  class NewArgumentGraphController extends cn.carneades.Controller
    @.$inject = [
      "$scope"
      "$state"
      "$cnBucket"
      "$stateParams"
      "$translate"
      "$previousState"
      "Project"
      "editorService"
      ]

    constructor: (
      @scope, @state, @cnBucket, @stateParams, @translate, @previousState,
      @Project, @editorService
    ) ->
      @scope = cn.carneades.extend @scope,
        ag:
          name: ""
          header:
            description:
              en: ""
              de: ""
              fr: ""
              it: ""
              sp: ""
              nl: ""
            title: ""
        languages: @editorService.getLanguages()
        onSave: @.save
        onCancel: => @editorService.onCancel 'newArgumentGraphEditor'
        placeholderName: @translate.instant 'placeholder.name'
        placeholderTitle: @translate.instant 'placeholder.title'
        tooltipSave: @translate.instant 'tooltip.argumentgraph.save'
        tooltipCancel: @translate.instant 'tooltip.cancel'

    save: ->
      _description = {}
      for k, v of @scope.ag.header.description
        _description[k] = editor.htmlize v

      @scope.ag.header.description = _description

      project = {
        name: @scope.ag.name
        title: @scope.ag.title
        header: @scope.ag.header
      }

      @Project.newArgumentGraph(
        {pid: $stateParams.pid},
        project
      ).$promise.then((data) =>
        @cnBucket.remove @state.$current
        state = @previousState.get 'newArgumentGraphEditor'
        params = pid: @stateParams.pid, db: @scope.ag.name
        @state.go state.state.name, params, reload: true
        @previousState.forget 'newArgumentGraphEditor'
      )
