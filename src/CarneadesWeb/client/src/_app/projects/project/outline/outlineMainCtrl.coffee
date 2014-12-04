define [
  'root'
  'classes'
  'utils'
], (cn) ->

  class OutlineMainController extends cn.carneades.Controller
    @.$inject = [
      "$scope", "$state", "$stateParams", "$previousState",
      "$translate", "$location", "$window", "project", "tproject",
      "scroll", "tpid", "markos"
      ]

    @.$resolve =
      tproject: (ProjectLoader, $stateParams) ->
        return new ProjectLoader $stateParams
      project: (MetadataLoader, $stateParams) ->
        $stateParams.mid = 1
        return new MetadataLoader $stateParams
      scroll: 'scroll'
      tpid: (projectInfo, $stateParams, tproject) ->
        tpid = projectInfo.getSchemesProject tproject
        $stateParams.tpid = tpid
        return tpid
      tid: (projectInfo, $stateParams, tproject) ->
        tid = projectInfo.getSchemesName tproject
        $stateParams.tid = tid
        return tid

    constructor: (
      @scope, @state, @stateParams, @previousState, @translate, @location,
      @window, @project, @tproject, @scroll, @tpid, @markos
    ) ->

      @stateParams.tpid = @tpid
      @.tooltipEdit = @translate.instant 'tooltip.outline.edit'
      @.tooltipNewStatemen = @translate.instant 'tooltip.statement.new'
      @.tooltipNewArgument = @translate.instant 'tooltip.argument.new'
      @.tooltipShare = @translate.instant 'tooltip.share'
      @.currentUrl = @location.absUrl()
      @.isSharing = false

    scrollTo: -> @scroll.scrollTo()

    onShare: ->
      @scope.isSharing = !@scope.isSharing

    shareOnMarkos: ->
      markos.share @scope.currentUrl

    shareOnTwitter: ->
      _url = escape @scope.currentUrl
      _text = escape @project.title
      link = "https://twitter.com/intent/tweet?url=#{_url}&text=#{_text}"
      @window.open link
      return true

    openMetadataEditor: ->
      @state.go 'home.projects.project.edit', @stateParams
      @previousState.memo 'newMetadataEditor'

    openArgumentEditor: ->
      @state.go 'home.projects.project.arguments.new', @stateParams
      @previousState.memo 'newArgumentEditor'

    openStatementEditor: ->
      @state.go 'home.projects.project.statements.new', @stateParams
      @previousState.memo 'newStatementEditor'
