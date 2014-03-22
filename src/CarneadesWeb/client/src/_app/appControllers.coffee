# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define ['angular', 'common/services/i18nNotifications', 'common/services/httpRequestTracker'], (angular) ->
  "use strict"
  angular.module('app.controllers', ['services.i18nNotifications', 'services.httpRequestTracker']).controller('AppCtrl', ($scope, $location, i18nNotifications) ->
    $scope.notifications = i18nNotifications
    $scope.removeNotification = (notification) ->
      i18nNotifications.remove(notification)
      undefined

    $scope.$on '$routeChangeError', (event, current, previous, rejection) ->
       i18nNotifications.pushForCurrentRoute 'errors.route.changeError', 'error', {}, {rejection: rejection}
       undefined

     ( ->
        "use strict"
        carneades = (converter) ->
          [
            #  ? title ? syntax
            type: "lang"
            regex: "\\[@([a-zA-Z0-9]+)[^\\]]*\\]"
            replace: (match, citation_key) ->
              metadata = {}

              request = new XMLHttpRequest
              request.open 'GET', $location.protocol() + "://" + $location.host() + ":" + $location.port() + "/carneades/api/projects/copyright/main/metadata?k=#{citation_key}", false

              request.onload = ->
                if request.status >= 200 and request.status < 400
                  metadata = JSON.parse request.responseText
                else
                  # We reached our target server, but it returned an error
                  console.log 'Server reached, but it returned an error'

                request.onerror = ->
                  # There was a connection error of some sort
                  console.log 'Error'

              request.send()

              metadatum = undefined
              if metadata and metadata.length is 1
                metadatum = metadata[0].source
                return "<a href=\"" + metadatum + "\" >" + match + "</a>" if metadatum
                metadatum = metadata[0].identifier
                return "<a href=\"" + metadatum + "\" >" + match + "</a>"  if metadatum and (metadatum.indexOf("http://") is 0 or metadatum.indexOf("https://") is 0 or metadatum.indexOf("ftp://") is 0 or metadatum.indexOf("file://") is 0)
              match
          ,
            type: "output"
            filter: (source) ->
              source.replace /file:\/\/(\w+)\/(\w+)/g, (match, project, document) ->
                "carneadesws/documents/" + project + "/" + document

          ]


        # Client-side export
        window.Showdown.extensions.carneades = carneades  if typeof window isnt "undefined" and window.Showdown and window.Showdown.extensions

        # Server-side export
        module.exports = carneades  if typeof module isnt "undefined"

        undefined
      )()
    undefined
  )
  .controller 'HeaderCtrl', ($breadcrumb, $scope, $location, notifications, httpRequestTracker) ->
    $scope.hasPendingRequests = ->
      httpRequestTracker.hasPendingRequests()

    cmdsToNavigation = (commands = []) ->
      results = []
      for c in commands
        results.push {label: c.label, state: c.state, params: $scope.$stateParams}
      return results

    isActive = (state) ->
      return $scope.$state.$current.name == state.name

    isLast = (state, length) ->
      return

    setNavigationState = () ->
      currentState = $scope.$state.$current
      upperBound = 0
      depreciated = []

      if $breadcrumb.getIndex() + 1 == $breadcrumb.getNavigationStates().length
        # replace it completely because the whole breadcrumb must be updated.
        upperBound = currentState.path.length
      # replace every entry til n and append all elements > n + 1
      else
        upperBound = $breadcrumb.getNavigationStates().length
        depreciated = $breadcrumb.getNavigationStates().slice($breadcrumb.getIndex() + 1, $breadcrumb.getNavigationStates().length - 1)

      $breadcrumb.clear()
      idx = 0
      angular.forEach currentState.path, (state) ->
        $breadcrumb.push {
          label: state.self.label
          name: state.self.name
          params: $scope.$stateParams
          commands: cmdsToNavigation state.self.commands
          isActive: isActive state.self
          isLast: idx++ == upperBound
        }

        angular.forEach depreciated, (s) ->
          s.isActive = false
          s.isLast = idx++ == upperBound
          $breadcrumb.push s

      $scope.$navigationStates = $breadcrumb.getNavigationStates()

    $scope.$on '$stateChangeSuccess', ->
      setNavigationState()

    setNavigationState()

    undefined
