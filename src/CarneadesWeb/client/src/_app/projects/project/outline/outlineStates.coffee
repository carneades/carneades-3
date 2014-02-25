define ['angular', '../../../common/resources/projects', '../../../common/resources/nodes', '../../../common/resources/metadata'], (angular) ->
  "use strict"
  angular.module('outline.states', ['resources.projects', 'resources.nodes', 'resources.metadata', 'outline.controllers']).config(['$stateProvider', ($stateProvider) ->
    states = [
      {
        name: 'projects.project.outline'
        label: 'Outline'
        url: '/:db/outline'
        commands: [
          label: "Map"
          state: "projects.project.map"
        ,
          label: "Theory"
          state: "projects.project.theory"
        ]
        views: {
          "@": {
            templateUrl: 'project/outline/outline.tpl.html',
            controller: 'OutlineCtrl',
            resolve: {
              project: ['$stateParams', 'ProjectLoader', ($stateParams, ProjectLoader) ->
                new ProjectLoader($stateParams)
              ],
              node: ['$stateParams', 'NodeLoader', ($stateParams, NodeLoader) ->
                $stateParams.nid = 1;
                new NodeLoader($stateParams)
              ],
              metadata: ['$stateParams', 'MetadataLoader', ($stateParams, MetadataLoader) ->
                  $stateParams.mid = 1;
                  new MetadataLoader($stateParams)
              ]
            }
          }
        }
      }
    ]

    angular.forEach states, (state) ->
      $stateProvider.state state
      undefined

    undefined
  ])
