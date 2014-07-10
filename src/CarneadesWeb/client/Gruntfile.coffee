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

  require('load-grunt-tasks')(grunt)
  require('time-grunt')(grunt)

  readSubdirNames = (dir) ->
    isDir = (dir, f) ->
      stat = fs.statSync(dir + "/" + f)
      stat and stat.isDirectory() and (file != "templates" or file != "css")
    return (file for file in fs.readdirSync dir when isDir dir, file)

  cfgHtmlmin = (
    # collapseBooleanAttributes prevents a bug
    # in grunt-html2js version 0.6.0
    # see https://github.com/karlgoldstein/grunt-html2js/issues/42
    collapseBooleanAttributes = true,
    collapseWhitespace = true,
    removeAttributeQuotes = false,
    removeComments = true,
    removeCommentsFromCDATA = true,
    removeEmptyAttributes = false,
    removeRedundantAttributes = false,
    removeScriptTypeAttributes = false,
    removeStyleLinkTypeAttributes = false,
    removeOptionalTags = true) ->

    collapseBooleanAttributes: collapseBooleanAttributes
    collapseWhitespace: collapseWhitespace
    removeAttributeQuotes: removeAttributeQuotes
    removeComments: removeComments
    removeCommentsFromCDATA: removeCommentsFromCDATA
    removeEmptyAttributes: removeEmptyAttributes
    removeRedundantAttributes: removeRedundantAttributes
    removeScriptTypeAttributes: removeScriptTypeAttributes
    removeStyleLinkTypeAttributes: removeStyleLinkTypeAttributes
    removeOptionalTags: removeOptionalTags

  cfgHtml2Js = (mapcfg) ->
    config = {}
    config.options =
      base: mapcfg.base
      useStrict: true
      htmlmin: mapcfg.htmlmin

    config.src = [mapcfg.base + '/**/*.jade', '!' + mapcfg.base + '/index.jade']
    config.dest = mapcfg.dest + "/" + mapcfg.name + ".js"
    config.module = "templates." + mapcfg.name
    return config

  createHtml2JsConfig = () ->
    config = {}
    config.app = cfgHtml2Js(
      name: "app"
      base: "<%= src.base %>"
      dest: "<%= gen.templates.base %>"
      htmlmin: cfgHtmlmin()
    )

    return config

  # Print a timestamp
  grunt.registerTask "timestamp", ->
    grunt.log.subhead Date()

  # Project configuration
  grunt.initConfig
    pkg: grunt.file.readJSON("package.json")

    src:
      base: "src/_app"
      assets: "<%= src.base %>/assets"
      scss: "<%= src.assets %>/stylesheets"

    gen:
      base: ".generated"
      templates:
        base: "<%= gen.base %>/templates"

    dist:
      base: "dist/public/carneades"

    projects:
      base: "../../../projects"
      copyright: "<%= projects.base %>/copyright/theme"
      default: "<%= projects.base %>/default/theme"
      markos: "<%= projects.base %>/markos/theme"

    banner: "/*! <%= pkg.title || pkg.name %> - v<%= pkg.version %> - <%= grunt.template.today(\"yyyy-mm-dd\") %>\n" + "<%= pkg.homepage ? \" * \" + pkg.homepage + \"\\n\" : \"\" %>" + " * Copyright (c) <%= grunt.template.today(\"yyyy-mm-dd\") %> <%= pkg.author %>;\n" + " * Licensed <%= _.pluck(pkg.licenses, \"type\").join(\", \") %>\n */\n"

    clean:
      gen: ["<%= gen.base %>"]
      dist: ["<%= dist.base %>"]

    bowercopy:
      options:
        srcPrefix: 'bower_components'
      requirejs:
        options:
          destPrefix: '<%= dist.base %>'
        files:
          'require.js': 'requirejs/require.js'
      lib:
        options:
          destPrefix: '<%= gen.base %>/libs'
        files:
          'angular.js': 'angular/angular.js'
          'angular-sanitize.js': 'angular-sanitize/angular-sanitize.js'
          'angular-ui-router.js': 'angular-ui-router/release/angular-ui-router.js'
          'angular-ui-utils.js': 'angular-ui-utils/ui-utils.js'
          'angular-resource.js': 'angular-resource/angular-resource.js'
          'angular-translate.js': 'angular-translate/angular-translate.js'
          'angular-translate-loader-static-files.js': 'angular-translate-loader-static-files/angular-translate-loader-static-files.js'
          'angular-bootstrap.js': 'angular-bootstrap/ui-bootstrap-tpls.js'
          'requirejs-domready.js': 'requirejs-domready/domReady.js'
          'showdown': 'showdown/src'
          'spin.js': 'spin.js/spin.js'

    jade:
      compile:
        options:
          data:
            debug: false
        files:
          '<%= dist.base %>/index.html': '<%= src.base %>/index.jade'

    copy:
      index:
        files: [
          src: ["index.html"]
          dest: "<%= gen.base %>"
          cwd: 'src'
          expand: true
        ]
      layout:
        files: [
          expand: true
          cwd: '<%= gen.base %>/css'
          src: ['layout.css']
          dest: '<%= dist.base %>/css'
        ]
      default:
        files: [
          expand: true
          cwd: '<%= gen.base %>/css'
          src: ['default.css']
          dest: '<%= projects.default %>'
        ]
      copyright:
        files: [
          src: ['copyright.css']
          expand: true
          cwd: "<%= gen.base %>/css"
          dest: '<%= projects.copyright %>'
        ]
      markos:
        files: [
          src: ['markos.css']
          expand: true
          cwd: "<%= gen.base %>/css"
          dest: '<%= projects.markos %>'
        ]
      fonts:
        files: [
          dest: "<%= dist.base %>/fonts"
          src: "**"
          expand: true
          cwd: "<%= src.assets %>/fonts"
        ]

      images:
        files: [
          dest: "<%= dist.base %>/images"
          src: "**"
          expand: true
          cwd: "<%= src.assets %>/images"
        ]

      languages:
        files: [
          dest: "<%= dist.base %>/languages"
          src: "**"
          expand: true
          cwd: "<%= src.assets %>/languages"
        ]

    coffee:
      scripts:
        files: [
          expand: true
          cwd: "<%= src.base %>"
          src: ["./**/*.coffee"]
          dest: "<%= gen.base %>"
          ext: ".gen.js"
          ignores: ["assets/*"]
        ]

    ngmin:
      scripts:
        expand: true
        cwd: "<%= gen.base %>"
        src: ['./**/*.gen.js']
        dest: "<%= gen.base %>"
        ext: '.js'

    compass:
      carneades:
        options:
          sassDir: "<%= src.scss %>"
          cssDir: "<%= gen.base %>/css"
          importPath: 'bower_components'
          outputStyle: 'compressed'

    watch:
      haml:
        files: ["<%= src.base %>/**/*.haml"]
        tasks: ["haml", "chtml2js", "timestamp"]
      coffee:
        files: ["<%= src.base %>/**/*.coffee"]
        tasks: ["coffee", "timestamp"]
      css:
        files: ["<%= comp.base %>/*.css"]
        tasks: ["compass", "cssmin"]

     requirejs:
      compile:
        options:
          findNestedDependencies: true
          mainConfigFile: "<%= gen.base %>/main.js"
          include: 'main'
          locale: "en-us"
          optimize: "uglify2"
          useStrict: true

          # see: http://requirejs.org/docs/optimization.html#sourcemaps
          generateSourceMaps: true
          preserveLicenseComments: false

          uglify2:
            output:
              beautify: false
            compress:
              sequences: false
              dead_code: true
              unused: true
            warnings: false
            mangle: false
            inSourceMap: true

          name: "app"
          out: "<%= dist.base %>/main.min.js"
          keepBuildDir: true
          removeCombined: true
          fileExclusionRegExp: /\.tpl\.html/

  grunt.registerTask 'chtml2js',
    'Dynamically generate html2js sub tasks',
    (target) ->
      html2js = createHtml2JsConfig()
      grunt.config 'html2js', html2js
      grunt.task.run 'html2js'
      #grunt.config 'clean', tplhtml: ["<%= comp.base %>/**/*.tpl.html"]
      #grunt.task.run 'clean:tplhtml'

  grunt.registerTask "build", [
    "clean",
    "chtml2js",
    "coffee",
    "ngmin",
    "bowercopy",
    "compass",
    "requirejs",
    "copy",
    "jade"
  ]

  grunt.registerTask 'css-only', [
    'compass'
    'copy'
  ]

  # # magic continues: test
  # grunt.registerTask "test", ["karma"]

  # # full build from scratch
  # grunt.registerTask 'build', ["compile", "package"]

  # # deploy: do a full build from scratch
  # grunt.registerTask "deploy", ["clean", "build", "test"]

  # # watchers
  # grunt.registerTask "build-watch", ["watch"]
