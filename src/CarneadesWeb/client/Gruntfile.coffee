#global module, require

##########################
# Carneades config
#
##########################
#
# Build process differentiates three states:
#  1) Preprocessed (prep)
#  2) Compiled (comp)
#  3) Distributed (dist)
#
# 1) Preprocessed (prep)
# Contains code that needs to compiled into target
# languages in order to be used in 3)
# This includes
#   .coffee -> .js
#   .haml   -> .tpl.html
#   .scss   -> css
#   .js     -> .js (vendor specific)
#
#
# 2) Compiled (comp)
# Processed defines an intermediate state. Here we collect
# all code that is going to be distributed/packaged
#
#
# 3) Distributed (dist)
# The production ready code
#
#########################
fs = require 'fs'

module.exports = (grunt) ->
  "use strict"

  readSubdirNames = (dir) ->
    isDir = (dir, f) ->
      stat = fs.statSync(dir + "/" + f)
      stat and stat.isDirectory() and (file != "templates" or file != "css")
    return (file for file in fs.readdirSync dir when isDir dir, file)

  cfgHtmlmin = (
    # collapseBooleanAttributes prevents a bug
    # in grunt-html2js version 0.6.0
    # see https://github.com/karlgoldstein/grunt-html2js/issues/42
    collapseBooleanAttributes = false,
    collapseWhitespace = true,
    removeAttributeQuotes = true,
    removeComments = true,
    removeEmptyAttributes = true,
    removeRedundantAttributes = true,
    removeScriptTypeAttributes = true,
    removeStyleLinkTypeAttributes = true) ->

    collapseBooleanAttributes: collapseBooleanAttributes
    collapseWhitespace: collapseWhitespace
    removeAttributeQuotes: removeAttributeQuotes
    removeComments: removeComments
    removeEmptyAttributes: removeEmptyAttributes
    removeRedundantAttributes: removeRedundantAttributes
    removeScriptTypeAttributes: removeScriptTypeAttributes
    removeStyleLinkTypeAttributes: removeStyleLinkTypeAttributes

  cfgHtml2Js = (mapcfg) ->
    recursiveSearch = (isRecursive = true) ->
      if isRecursive then "/**/*.tpl.html" else "/*.tpl.html"

    config = {}
    config.options =
      base: mapcfg.base
      useStrict: true
      htmlmin: mapcfg.htmlmin

    config.src = mapcfg.base + recursiveSearch mapcfg.recursive
    config.dest = mapcfg.dest + "/" + mapcfg.name + ".js"
    config.module = "templates." + mapcfg.name
    return config

  createHtml2JsConfig = (dir) ->
    config = {}
    dirs = readSubdirNames(dir)
    dirs.forEach (d) ->
      config[d] = cfgHtml2Js(
        name: d
        base: "<%= comp.base %>/" + d
        dest: "<%= comp.templates.base %>"
        htmlmin: cfgHtmlmin()
      )

    config.app = cfgHtml2Js(
      name: "app"
      base: "<%= comp.base %>"
      dest: "<%= comp.templates.base %>"
      htmlmin: cfgHtmlmin()
      recursive: false)

    return config

  # Print a timestamp
  grunt.registerTask "timestamp", ->
    grunt.log.subhead Date()

  # Project configuration
  grunt.initConfig
    pkg: grunt.file.readJSON("package.json")

    prep:
      base: "src/_app"
      assets: "<%= prep.base %>/assets"
      scss: "<%= prep.assets %>/stylesheets"
      bootstrap: "<%= prep.assets %>/javascripts/bootstrap"

    comp:
      base: "src/app"
      bower: "<%= comp.base %>/components"
      templates:
        base: "<%= comp.base %>/templates"

    dist:
      base: "dist/public/carneades"

    projects:
      base: "../../../projects"
      copyright: "<%= projects.base %>/copyright/theme"
      default: "<%= projects.base %>/default/theme"
      markos: "<%= projects.base %>/markos/theme"

    banner: "/*! <%= pkg.title || pkg.name %> - v<%= pkg.version %> - <%= grunt.template.today(\"yyyy-mm-dd\") %>\n" + "<%= pkg.homepage ? \" * \" + pkg.homepage + \"\\n\" : \"\" %>" + " * Copyright (c) <%= grunt.template.today(\"yyyy-mm-dd\") %> <%= pkg.author %>;\n" + " * Licensed <%= _.pluck(pkg.licenses, \"type\").join(\", \") %>\n */\n"
    src:
      js: ["src/**/*.js"]
      jsTpl: ["<%= distdir %>/templates/**/*.js"]
      scenarios: ["test/**/*.scenario.js"]
      html: ["src/index.html"]

    clean:
      comp:
        src: ["<%= comp.base %>/*", "!<%= comp.base %>/components/**"]
      dist: ["<%= dist.base %>"]

    copy:
      fonts:
        files: [
          dest: "<%= dist.base %>/fonts"
          src: "**"
          expand: true
          cwd: "<%= prep.assets %>/fonts"
        ]

      images:
        files: [
          dest: "<%= dist.base %>/images"
          src: "**"
          expand: true
          cwd: "<%= prep.assets %>/images"
        ]

      languages:
        files: [
          dest: "<%= dist.base %>/languages"
          src: "**"
          expand: true
          cwd: "<%= prep.assets %>/languages"
        ]

      requirejs:
        files: [
          dest: "<%= dist.base %>"
          src: "require.js"
          cwd: "<%= comp.bower %>/requirejs"
          expand: true
        ]

    coffee:
      scripts:
        files: [
          expand: true
          cwd: "<%= prep.base %>"
          src: ["./**/*.coffee"]
          dest: "<%= comp.base %>"
          ext: ".js"
          ignores: ["assets/*"]
        ]
        options:
          sourceMap: false

    ngmin:
      scripts:
        expand: true
        cwd: "<%= comp.base %>"
        src: ['./**/*.js'],
        dest: "<%= comp.base %>"
        ignores: ["components/*"]

    haml:
      scripts:
        files: [
          expand: true
          cwd: "<%= prep.base %>"
          src: ["./**/*.haml"]
          dest: "<%= comp.base %>"
          ext: ".tpl.html"
        ]

    compass:
      carneades:
        options:
          sassDir: "<%= prep.scss %>"
          cssDir: "<%= comp.base %>/css"

    cssmin:
      layout:
        expand: true
        cwd: '<%= comp.base %>/css'
        src: "layout.css"
        dest: '<%= dist.base %>/css'
        ext: '.min.css'

      animate:
        expand: true
        cwd: '<%= comp.base %>/css'
        src: "animate.css"
        dest: '<%= dist.base %>/css'
        ext: '.min.css'

      default:
        expand: true
        cwd: '<%= comp.base %>/css'
        src: "default.css"
        dest: '<%= projects.default %>'
        ext: '.min.css'
      copyright:
        expand: true
        cwd: '<%= comp.base %>/css'
        src: "copyright.css"
        dest: '<%= projects.copyright %>'
        ext: '.min.css'
      markos:
        expand: true
        cwd: '<%= comp.base %>/css'
        src: "markos.css"
        dest: '<%= projects.markos %>'
        ext: '.min.css'

    concat:
      bootstrap:
        src: [
          "<%= prep.bootstrap %>/affix.js",
          "<%= prep.bootstrap %>/alert.js",
          "<%= prep.bootstrap %>/button.js",
          "<%= prep.bootstrap %>/carousel.js",
          "<%= prep.bootstrap %>/collapse.js",
          "<%= prep.bootstrap %>/dropdown.js",
          "<%= prep.bootstrap %>/tab.js",
          "<%= prep.bootstrap %>/transition.js",
          "<%= prep.bootstrap %>/scrollspy.js",
          "<%= prep.bootstrap %>/modal.js",
          "<%= prep.bootstrap %>/tooltip.js",
          "<%= prep.bootstrap %>/popover.js"
        ]
        dest: "<%= comp.base %>/bootstrap-sass.js"
      index:
        src: ["src/index.html"]
        dest: "<%= dist.base %>/index.html"
        options:
          process: true

    watch:
      haml:
        files: ["<%= prep.base %>/**/*.haml"]
        tasks: ["haml", "chtml2js", "timestamp"]
      coffee:
        files: ["<%= prep.base %>/**/*.coffee"]
        tasks: ["coffee", "timestamp"]
      css:
        files: ["<%= comp.base %>/*.css"]
        tasks: ["compass", "cssmin"]

    bower:
      target:
        rjsConfig: "src/app/main.js"
        indent: "    "

    karma:
      unit:
        options:
          browsers: [
            'PhantomJS'
          ]
          captureTimeout: 5000
          colors: true
          files: [
            # {pattern: 'src/app/components/angular-mocks/angular-mocks.js', included: true}
            # {pattern: 'src/app/components/angular/angular.js', included: false}

            # 'src/app/components/angular-ui-router/**/*.js'
            {pattern: 'src/app/*.js', included: false}
            {pattern: 'src/app/{common,projects,lican}/**/*.js', included: false}
            {pattern: 'src/app/components/**/*.js', included: false},
            # {pattern: 'src/app/**/*.js', included: false}
            {pattern: 'test/*.coffee', included: false}
            {pattern: 'test/**/*.coffee', included: false}
            # 'src/app/**/*.html'
            'test/test-main.js'
          ]
          exclude: [
            'dist/carneades/main.js'
            'src/app/components/**/test/**'
            'src/app/components/angular-ui-router/config/jsdoc.js'
            'src/app/components/angular-ui-router/release/doc/scripts/prettify/lang-css.js'
            # 'src/app/components/**'
          ]
          frameworks: [
            'jasmine', 'requirejs'
          ]
          plugins: [
            'karma-jasmine'
            'karma-phantomjs-launcher',
            'karma-coffee-preprocessor',
            'karma-requirejs'
            'karma-ng-html2js-preprocessor'
          ]
          junitReporter:
            outputFile: 'test-results.xml'
          keepalive: false
          logLevel: 'INFO'
          port: 9876
          preprocessors:
            '**/*.coffee': ['coffee']
            # '**/*.html': ['ng-html2js']
          reporters: [
            'dots'
            # 'junit'
            'progress'
          ]
          runnerPort: 9100
          singleRun: false
          autoWatch: true

    requirejs:
      compile:
        options:
          findNestedDependencies: true
          mainConfigFile: "<%= comp.base %>/main.js"
          locale: "en-us"
          baseUrl: "<%= comp.base %>"
          optimize: "uglify2"
          useStrict: true

          # see: http://requirejs.org/docs/optimization.html#sourcemaps
          generateSourceMaps: true
          preserveLicenseComments: false

          uglify2:
            output:
              beautify: true
            compress:
              sequences: false

            warnings: true,
            mangle: false

          name: "app"
          out: "<%= dist.base %>/main.js"
          keepBuildDir: true
          removeCombined: true
          fileExclusionRegExp: /\.tpl\.html/
          onBuildWrite: (name, path, contents) ->
            grunt.log.writeln "Writing: " + name
            contents

        done: (done, output) ->
          duplicates = require("rjs-build-analysis").duplicates(output)
          if duplicates.length > 0
            grunt.log.subhead "Duplicates found in requirejs build:"
            grunt.log.warn duplicates
            done new Error("r.js built duplicate modules, please check the excludes option.")
          done()

  grunt.loadNpmTasks 'grunt-contrib-cssmin'
  grunt.loadNpmTasks "grunt-contrib-concat"
  grunt.loadNpmTasks "grunt-contrib-jshint"
  grunt.loadNpmTasks "grunt-contrib-requirejs"
  grunt.loadNpmTasks "grunt-contrib-clean"
  grunt.loadNpmTasks "grunt-contrib-copy"
  grunt.loadNpmTasks "grunt-contrib-watch"
  grunt.loadNpmTasks "grunt-contrib-compass"
  grunt.loadNpmTasks "grunt-html2js"
  grunt.loadNpmTasks "grunt-bower-requirejs"
  grunt.loadNpmTasks "grunt-contrib-coffee"
  grunt.loadNpmTasks "grunt-contrib-haml"
  grunt.loadNpmTasks "grunt-karma"
  grunt.loadNpmTasks "grunt-ngmin"

  grunt.registerTask 'chtml2js', 'Dynamically generate html2js sub tasks', (target) ->
    html2js = createHtml2JsConfig 'src/app'
    grunt.config 'html2js', html2js
    grunt.task.run 'html2js'
    #grunt.config 'clean', tplhtml: ["<%= comp.base %>/**/*.tpl.html"]
    #grunt.task.run 'clean:tplhtml'


  # clean task
  grunt.registerTask "cleanup", ["clean:comp", "clean:dist"]

  # basic build tasks
  grunt.registerTask "compile", ["haml", "chtml2js", "coffee", "ngmin", "compass", "concat:bootstrap"]
  grunt.registerTask "package", ["bower", "requirejs", "concat:index", "cssmin", "copy:requirejs", "copy:images", "copy:fonts", "copy:languages"]

  # magic continues: test
  grunt.registerTask "test", ["karma"]

  # full build from scratch
  grunt.registerTask 'build', ["compile", "package"]

  # deploy: do a full build from scratch
  grunt.registerTask "deploy", ["clean", "build", "test"]

  # watchers
  grunt.registerTask "build-watch", ["watch"]
