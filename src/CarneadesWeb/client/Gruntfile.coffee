1#global module, require

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
      fonts: "<%= src.assets %>/fonts"

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
          'angular-capitalize-filter.js': 'angular-capitalize-filter/capitalize.js'
          'angular-sanitize.js': 'angular-sanitize/angular-sanitize.js'
          'angular-ui-router.js': 'angular-ui-router/release/angular-ui-router.js'
          'angular-ui-utils.js': 'angular-ui-utils/ui-utils.js'
          'angular-ui-slider.js': 'angular-ui-slider/src/slider.js'
          'angular-ui-select.js': 'angular-ui-select/dist/select.js'
          'angular-resource.js': 'angular-resource/angular-resource.js'
          'angular-translate.js': 'angular-translate/angular-translate.js'
          'ui-router-extras.js': 'ui-router-extras/release/ct-ui-router-extras.js'
          'angular-translate-loader-static-files.js': 'angular-translate-loader-static-files/angular-translate-loader-static-files.js'
          'angular-bootstrap.js': 'angular-bootstrap/ui-bootstrap-tpls.js'
          'requirejs-domready.js': 'requirejs-domready/domReady.js'
          'perfect-scrollbar': 'perfect-scrollbar/min',
          'angular-perfect-scrollbar': 'angular-perfect-scrollbar-allfix53/src/angular-perfect-scrollbar.js'
          'showdown': 'showdown/src'
          'spin.js': 'spin.js/spin.js'
          'hallo.js': '../libs/hallo.js'
          'to-markdown.js': '../libs/to-markdown.js'
          'jquery.js': 'jquery/jquery.min.js'
          'jquery-ui.js': 'jquery-ui/ui/jquery-ui.js'
          'jquery-htmlclean.js': 'jquery-htmlclean/jquery.htmlClean.js'
          'rangy-core.js': 'rangy/rangy-core.js'
          'angular-ui-codemirror.js': 'angular-ui-codemirror/ui-codemirror.js'
          'codemirror/lib': 'codemirror/lib'
          'codemirror/mode/clojure': '../libs/codemirror/mode/clojure'
          'codemirror/addon': '../libs/codemirror/addon'
          'fontawesome/css': 'fontawesome/css/font-awesome.min.css'
          'fontawesome/fonts': 'fontawesome/fonts'
          'open-sans/css': 'open-sans/css/open-sans.min.css'
          'open-sans/fonts': 'open-sans/fonts'
          'selectize.js': 'selectize/dist/js/standalone/selectize.min.js'
          'angular-selectize.js': 'angular-selectize2/dist/selectize.js'
          'angular-cache.js': 'angular-cache/dist/angular-cache.js'

    jade:
      compile:
        options:
          data:
            debug: false
        files:
          '<%= dist.base %>/index.html': '<%= src.base %>/index.jade'

    copy:
      codemirror:
        files: [
          src: ["codemirror.css"]
          dest: "<%= dist.base %>/css"
          cwd: 'libs/codemirror/lib'
          expand: true
        ]

      codemirror_theme_xq_light:
        files: [
          src: ["xq-light.css"]
          dest: "<%= dist.base %>/css"
          cwd: 'libs/codemirror/theme'
          expand: true
        ]

      perfect_scrollbar:
        files: [
          src: ["perfect-scrollbar.min.css"]
          dest: "<%= dist.base %>/css"
          cwd: '<%= gen.base %>/libs/perfect-scrollbar'
          expand: true
        ]

      index:
        files: [
          src: ["index.html"]
          dest: "<%= gen.base %>"
          cwd: 'src'
          expand: true
        ]

      fonts:
        files: [
          dest: "<%= dist.base %>/fonts"
          src: "**"
          expand: true
          cwd: "<%= src.fonts %>"
        ]

      default:
        files: [
          expand: true
          cwd: '<%= gen.base %>/css/theme'
          src: ['default.css']
          dest: '<%= projects.default %>'
        ]

      copyright:
        files: [
          src: ['copyright.css']
          expand: true
          cwd: "<%= gen.base %>/css/theme"
          dest: '<%= projects.copyright %>'
        ]

      markos:
        files: [
          src: ['markos.css']
          expand: true
          cwd: "<%= gen.base %>/css/theme"
          dest: '<%= projects.markos %>'
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

    ngAnnotate:
      scripts:
        options:
          singleQuotes: true
        files: [
          expand: true
          cwd: "<%= gen.base %>"
          src: ['./**/*.gen.js']
          dest: "<%= gen.base %>"
          ext: '.js'
        ]

    compass:
      carneades:
        options:
          sassDir: "<%= src.scss %>"
          cssDir: "<%= gen.base %>/css"
          fontsPath: "<%= src.fonts %>"
          fontsDir: "../fonts/"
          imagesPath: "<%= src.images %>"
          imagesDir: "../images/"
          importPath: 'bower_components'
          outputStyle: 'compressed'

    concat_css:
      all:
        src: ["<%= gen.base %>/css/**/*.css"]
        dest: "<%= dist.base %>/css/carneades.css"

    watch:
      coffee:
        files: ["<%= src.base %>/**/*.coffee"]
        tasks: ["coffee", "timestamp"]
        options:
          livereload: true
      css:
        files: ["<%= comp.base %>/*.css"]
        tasks: ["compass", "cssmin"]
        options:
          livereload: true

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
    "ngAnnotate",
    "bowercopy",
    "compass",
    "concat_css",
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
