define ['angular', 'angularMocks'], (angular, mocks) ->
  describe 'Questions', ->
      scope = undefined
      elem = undefined

      beforeEach ->
        mocks.module 'directives.questions'

        html = '''<div>
          <question-group group=questions></question-group>
          </div>
          '''

        inject ($compile, $rootScope) ->
          scope = $rootScope.$new()
          elem = angular.element(html)
          compiled = $compile(elem)
          compiled(scope)
          scope.$digest()

      it 'should works', ->
        scope.group = ["abc"]
        expect(elem.text()).toEqual('')
        expect(2).toEqual(2)
