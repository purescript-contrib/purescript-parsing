module.exports = function(grunt) {
  "use strict";
  
  grunt.initConfig({ 
    jshint: {
      all: [
        "Gruntfile.js",
        "tasks/*.js",
        "<%= nodeunit.tests %>"
      ]
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