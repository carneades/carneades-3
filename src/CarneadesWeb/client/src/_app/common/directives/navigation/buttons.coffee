# Copyright (c) 2014 Fraunhofer Gesellschaft
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

define [
  'angular',
  'angular-bootstrap'
], (angular) ->
  return angular.module 'carneades.ui.buttons', [
    'ui.bootstrap.position',
    'ui.bootstrap.bindHtml'
  ]

  .provider '$button', () ->
    defaultOptions =
      placement: 'left'
      animation: true
      popupDelay: 0

    triggerMap =
      'mouseenter': 'mouseleave'
      'click': 'click'
      'focus': 'blur'

    globalOptions = {}

    @options = (value) ->
      angular.extend globalOptions, value

    @setTriggers = (triggers) ->
      angular.extend triggerMap, triggers

    snake_case = (name) ->
      regexp = /[A-Z]/g
      separator = '-'
      return name.replace regexp, (letter, pos) ->
        return (pos ? separator : '') + letter.toLowerCase()

    @$get = ['$window', '$compile', '$timeout', '$parse', '$document',
    '$position', '$interpolate', ($window, $compile, $timeout, $parse,
    $document, $position, $interpolate) ->

      return (type, prefix, defaultTriggerShow) ->
        options = angular.extend {}, defaultOptions, globalOptions

        getTriggers = (trigger) ->
          show = trigger or options.trigger or defaultTriggerShow
          hide = triggerMap[show] or show
          return show: show, hide: hide

        directiveName = snake_case type
        startSym = $interpolate.startSymbol()
        endSym = $interpolate.endSymbol()
        template = [
          "<div #{directiveName}-popup ",
          "title='#{startSym}tt_title#{endSym}' ",
          "placement='#{startSym}tt_placement#{endSym}' ",
          ">"
          "</div>"
        ].join ''

        return {
          restrict: 'EA'
          scope: true
          compile: (tElem, tAttrs) ->
            tooltipLinker = $compile template

            fnLink = (scope, element, attrs) ->
              tooltip = ''
              appendToBody =
                if angular.isDefined options.appendToBody
                  options.appendToBody
                else
                  false
              triggers = getTriggers undefined

              positionTooltip = () ->
                ttPosition = $position.positionElements element, tooltip, scope.tt_placement, appendToBody
                ttPosition.top += 'px'
                ttPosition.left += 'px'
                # set calculated positioning
                tooltip.css ttPosition

              createTooltip = () ->
                tooltip = tooltipLinker scope, () -> return undefined
                scope.$digest

              showTooltipBind = () ->
                show()()

              hide = () ->
                console.log 'hide tooltip'

              show = () ->
                popupTimeout = null
                createTooltip()
                tooltip.css top: 0, left: 0, display: 'block'
                if appendToBody
                  $document.find 'body'.append tooltip
                else
                  element.after tooltip

                positionTooltip()
                scope.tt_isOpen = true
                scope.$digest()
                return positionTooltip

              attrs.$observe prefix + 'Title', (value) ->
                scope.tt_title = value

              attrs.$observe type, (value) ->
                scope.tt_content = value
                if not value and scope.tt_isOpen then hide()

              attrs.$observe prefix + 'Placement', (value) ->
                scope.tt_placement =
                  if angular.isDefined value
                    value
                  else
                    options.placement

              attrs.$observe prefix + 'Trigger', (value) ->
                #unregisterTriggers()
                triggers = getTriggers value
                element.bind triggers.show, showTooltipBind
            return fnLink
        }
    ]

    return @


  .directive 'test', ($button) ->
    return $button 'test', 'test', 'mouseenter'
