"use strict";

module.exports = function(grunt) {
  grunt.initConfig({ 
    jshint: {
      all: [
        "gruntfile.js",
        "tasks/*.js",
        "<%= nodeunit.tests %>"
      ],
      options: {
        jshintrc: ".jshintrc",
      },
    },
    
    clean: {
      tests: ["tmp"],
    },

    purescript: {
      options: {
          
      },
      compile: {
          files: {
              "tmp/out.js": ["src/**/*.hs", "examples/**/*.hs"]
          }
      }
    },
    
    nodeunit: {
      tests: ["test/*_test.js"],
    },
  });

  grunt.loadTasks("tasks");
  grunt.loadNpmTasks("grunt-contrib-jshint");
  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-contrib-nodeunit");
  grunt.loadNpmTasks("grunt-purescript");

  grunt.registerTask("test", ["clean", "purescript", "nodeunit"]);
  grunt.registerTask("default", ["jshint", "test"]);
};