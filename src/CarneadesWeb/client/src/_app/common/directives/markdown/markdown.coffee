define [
  'angular',
  'angular-sanitize'
], (angular) ->
  angular.module('markdown', ['ngSanitize'])

  .provider('markdownConverter', () ->
    opts = {}
    return {
      config: (newOpts) ->
        opts = newOpts
      $get: () ->
        return new Showdown.converter(opts)
    }
  )

  .directive('markdown', ($sanitize, $stateParams, markdownConverter) ->
    return {
      restrict: 'E'
      require: '?ngModel'
      link: (scope, element, attrs, model) ->
        callPrettyPrint = false
        # Check for option to strip whitespaces
        stripWS = attrs['strip']
        if String(stripWS).toLowerCase() is 'true'
          stripWS = true
        else
          stripWS = false

        render = () ->
          htmlText = ''
          val = ''

          if attrs['ngModel']
            if model.$modelValue
              val = model.$modelValue
          else
            val = element.text()

          if stripWS
            val = val.replace(/^[ /t]+/g, '').replace(/\n[ /t]+/g, '\n')

          html = $sanitize markdownConverter.makeHtml val
          html = html.replace /%DB%/, $stateParams.db
          html = html.replace /%PID%/, $stateParams.pid
          element.html html

          if callPrettyPrint then prettyPrint()

        if attrs['ngModel']
          scope.$watch attrs['ngModel'], render

        render()
    }
  )
