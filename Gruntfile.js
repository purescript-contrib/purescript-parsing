module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({ 
  
    libFiles: [
      "src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs",
    ],
    
    clean: ["output"],
  
    pscMake: ["<%=libFiles%>"],
    dotPsci: ["<%=libFiles%>"],
    docgen: {
        readme: {
            src: "src/**/*.purs",
            dest: "docs/Module.md"
        }
    },
    
    psc: {
      options: {
        main: true
      },
      exampleTest: {
        src: ["examples/Test.purs", "<%=libFiles%>"],
        dest: "tmp/Test.js"
      }
    },

    execute: {
      exampleTest: {
        src: "tmp/Test.js"
      }
    }

  });

  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-purescript");
  grunt.loadNpmTasks("grunt-execute");
  
  grunt.registerTask("exampleTest", ["psc:exampleTest", "execute:exampleTest"]);
  grunt.registerTask("make", ["pscMake", "dotPsci", "docgen"]);
  grunt.registerTask("test", ["exampleTest"]);
  grunt.registerTask("default", ["make"]);
};
