define [
  'root'
  'classes'
  'utils'
], (cn) ->

  class ProjectEditController extends cn.carneades.Controller
    @.$inject = [
      "$scope"
      "$state"
      "$previousState"
      "$stateParams"
      "$translate"
      "$cnBucket"
      "metadata"
      "Metadata"
      "editorService"
      ]

    @.$resolve =
      metadata: ($stateParams, MetadataRawLoader) ->
        $stateParams.db = 'main'
        $stateParams.mid = 1
        return new MetadataRawLoader $stateParams

    constructor: (
      @scope, @state, @previousState, @stateParams, @translate,
      @cnBucket, @metadata, @Metadata, @editorService
    ) ->
      @scope = cn.carneades.extend @scope,
        metadata: @metadata
        languages: @editorService.getLanguages()
        onSave: @.save
        onCancel: => @editorService.onCancel 'newMetadataEditor'
        tooltipSave: @translate.instant 'tooltip.ag.save'
        tooltipCancel: @translate.instant 'tooltip.cancel'

    save: ->
      params = pid: @stateParams.pid, db: 'main', mid: 1

      # no put implemented yet
      @Metadata.update(params, @metadata).$promise.then((data) ->
        @cnBucket.remove @state.$current
        state = @previousState.get 'newMetadataEditor'
        @state.go state.state.name, state.params, reload: true
        @previousState.forget 'newMetadataEditor'
      )
